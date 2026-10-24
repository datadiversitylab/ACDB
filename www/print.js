// Download a species card as a PDF. The server sends the card as HTML. The card is drawn
// off screen, cut into blocks, and the blocks are placed on A4 pages. Links stay clickable.
// If the PDF libraries cannot load, the card opens in the print window instead.
(function () {
  var LIBS = {
    canvas: "https://cdnjs.cloudflare.com/ajax/libs/html2canvas/1.4.1/html2canvas.min.js",
    pdf: "https://cdnjs.cloudflare.com/ajax/libs/jspdf/2.5.1/jspdf.umd.min.js"
  };
  var PAGE = { w: 210, h: 297, m: 10 };
  var busy = false;

  function load(src) {
    return new Promise(function (resolve, reject) {
      var s = document.createElement("script");
      s.src = src; s.onload = resolve; s.onerror = reject;
      document.head.appendChild(s);
    });
  }

  function libs() {
    var jobs = [];
    if (!window.html2canvas) jobs.push(load(LIBS.canvas));
    if (!(window.jspdf && window.jspdf.jsPDF)) jobs.push(load(LIBS.pdf));
    return Promise.all(jobs);
  }

  function overlay(text) {
    var o = document.createElement("div");
    o.className = "acdb-busy";
    o.innerHTML = '<div><span class="spin"></span>' + text + "</div>";
    document.body.appendChild(o);
    return o;
  }

  function wait(ms) { return new Promise(function (r) { setTimeout(r, ms); }); }

  // Wait until the photo is in the card, or give up after a few seconds
  function photoReady(box) {
    var tries = 0;
    function check() {
      var photo = box.querySelector(".acdb-photo[data-species]");
      var img = photo && photo.querySelector("img");
      var about = box.querySelector(".acdb-about");
      var aboutDone = !about || about.classList.contains("done") || about.classList.contains("none");
      var done = aboutDone && (!photo || (photo.classList.contains("has-photo") && img && img.complete));
      if (done || tries++ > 40) return Promise.resolve();
      return wait(150).then(check);
    }
    return check();
  }

  // Blocks of the card, in order. Headings stay with what follows them
  function blocks(card) {
    var list = [];
    function add(el, gap, keep) { list.push({ el: el, gap: gap, keep: !!keep }); }
    Array.prototype.forEach.call(card.children, function (el) {
      var c = el.classList;
      if (c.contains("pdf-foot") || el.offsetHeight === 0) return;
      if (c.contains("pdf-band")) add(el, 0);
      else if (c.contains("pdf-hero")) add(el, 3);
      else if (c.contains("pdf-h")) add(el, 5, true);
      else if (c.contains("pdf-group")) {
        Array.prototype.forEach.call(el.children, function (k, i) {
          add(k, i === 0 ? 3 : 1.5, k.tagName === "H3" || k.classList.contains("loc"));
        });
      } else if (c.contains("pdf-refs")) {
        Array.prototype.forEach.call(el.children, function (k) { add(k, 1.2); });
      } else add(el, 1.5);
    });
    return list;
  }

  // Number the references by hand, because a list item alone loses its number
  function flattenRefs(card) {
    var ol = card.querySelector(".pdf-refs");
    if (!ol) return;
    ol.style.paddingLeft = "0";
    Array.prototype.forEach.call(ol.children, function (li, i) {
      li.style.listStyle = "none";
      var n = document.createElement("b");
      n.textContent = (i + 1) + ". ";
      li.insertBefore(n, li.firstChild);
    });
  }

  // Use the photo as a background so that it is cropped like on the screen
  function flattenPhoto(card) {
    var photo = card.querySelector(".acdb-photo.has-photo");
    var img = photo && photo.querySelector("img");
    if (!img) return;
    photo.style.backgroundImage = 'url("' + img.src + '")';
    photo.style.backgroundSize = "cover";
    photo.style.backgroundPosition = "center";
    img.style.display = "none";
  }

  // A word with its box. A word that wraps at a hyphen (Roman-Palacios) comes back as one piece per line
  function pieces(node, start, word, range) {
    range.setStart(node, start); range.setEnd(node, start + word.length);
    var rects = Array.prototype.filter.call(range.getClientRects(), function (q) { return q.width > 0; });
    if (rects.length <= 1) {
      var box = range.getBoundingClientRect();
      return box.width ? [{ text: word, r: box }] : [];
    }
    var out = [], cur = null;
    for (var i = 0; i < word.length; i++) {
      range.setStart(node, start + i); range.setEnd(node, start + i + 1);
      var q = range.getBoundingClientRect();
      if (!q.width) { if (cur) cur.text += word[i]; continue; }
      if (cur && Math.abs((q.top + q.bottom) / 2 - (cur.r.top + cur.r.bottom) / 2) < 0.35 * (cur.r.bottom - cur.r.top)) {
        cur.text += word[i]; cur.r.right = q.right;
      } else {
        cur = { text: word[i], r: { left: q.left, right: q.right, top: q.top, bottom: q.bottom } };
        out.push(cur);
      }
    }
    return out;
  }

  // Text of a block as lines with position, size, style, and color. It is drawn as real text in the PDF
  function textRuns(el, k) {
    var base = el.getBoundingClientRect(), out = [];
    var walker = document.createTreeWalker(el, NodeFilter.SHOW_TEXT);
    var range = document.createRange();
    while (walker.nextNode()) {
      var node = walker.currentNode, text = node.nodeValue;
      if (!text.trim()) continue;
      var st = getComputedStyle(node.parentNode);
      if (st.display === "none" || st.visibility === "hidden") continue;
      var rgb = (st.color.match(/[\d.]+/g) || [0, 0, 0]).slice(0, 3).map(Number);
      var px = parseFloat(st.fontSize), line = null, m, re = /\S+/g;
      var style = {
        pt: px * k * 72 / 25.4, bold: parseInt(st.fontWeight, 10) >= 600, italic: st.fontStyle === "italic", color: rgb
      };
      while ((m = re.exec(text))) {
        pieces(node, m.index, m[0], range).forEach(function (pc) {
          var r = pc.r;
          var word = st.textTransform === "uppercase" ? pc.text.toUpperCase() : pc.text;
          // Same line: the vertical centers are close
          if (line && Math.abs((r.top + r.bottom) / 2 - (line.top + line.bottom) / 2) < 0.35 * (line.bottom - line.top) && r.left >= line.right - 1) {
            line.text += " " + word; line.right = r.right; line.words.push({ text: word, left: r.left, right: r.right });
          } else {
            line = { text: word, left: r.left, right: r.right, top: r.top, bottom: r.bottom, st: style, words: [{ text: word, left: r.left, right: r.right }] };
            out.push(line);
          }
        });
      }
    }
    return out.map(function (l) {
      var words = l.words.map(function (w) { return { text: w.text, x: (w.left - base.left) * k, w: (w.right - w.left) * k }; });
      return { text: l.text, words: words, x: (l.left - base.left) * k, w: (l.right - l.left) * k, y: (l.top - base.top + (l.bottom - l.top) * 0.79) * k, st: l.st };
    });
  }

  // The standard PDF font cannot draw accents safely, so the text layer uses the plain letters
  function clean(t) {
    return t.normalize("NFD").replace(/[\u0300-\u036f]/g, "").replace(/[\u2018\u2019]/g, "'").replace(/[\u201C\u201D]/g, '"')
      .replace(/[\u2013\u2014]/g, "-").replace(/\u2026/g, "...").replace(/[\u203A\u2192]/g, ">").replace(/[^\u0020-\u007E]/g, "");
  }

  function drawText(doc, runs, ox, oy) {
    runs.forEach(function (r) {
      doc.setFont("helvetica", r.st.bold ? (r.st.italic ? "bolditalic" : "bold") : (r.st.italic ? "italic" : "normal"));
      doc.setFontSize(r.st.pt);
      doc.setTextColor(r.st.color[0], r.st.color[1], r.st.color[2]);
      var size = r.st.pt * 25.4 / 72;

      // Stretch or squeeze the letters a little, so that the text is as wide as on the screen
      function put(text, x, w) {
        var t = clean(text);
        if (!t) return;
        var cs = t.length > 1 ? (w - doc.getTextWidth(t)) / (t.length - 1) : 0;
        cs = Math.max(-0.1 * size, Math.min(0.2 * size, cs));
        doc.text(t, ox + x, oy + r.y, { charSpace: cs });
      }

      // Lines with accents are placed word by word, so that the spaces stay where they are on the screen
      if (/[^\u0020-\u007E]/.test(r.text)) r.words.forEach(function (w) { put(w.text, w.x, w.w); });
      else put(r.text, r.x, r.w);
    });
  }

  function shot(el) {
    return window.html2canvas(el, { scale: 2, useCORS: true, backgroundColor: "#ffffff", logging: false });
  }

  // Clickable areas of the links inside a block, in mm from the top left of the block
  function links(el, k) {
    var r = el.getBoundingClientRect(), out = [];
    Array.prototype.forEach.call(el.querySelectorAll("a[href]"), function (a) {
      var href = a.href;
      if (!/^https?:/.test(href)) return;
      Array.prototype.forEach.call(a.getClientRects(), function (q) {
        out.push({ url: href, x: (q.left - r.left) * k, y: (q.top - r.top) * k, w: q.width * k, h: q.height * k });
      });
    });
    return out;
  }

  function makePdf(msg, box) {
    var card = box.querySelector(".acdb-pdf");
    var jsPDF = window.jspdf.jsPDF;
    var k = (PAGE.w - 2 * PAGE.m) / card.offsetWidth;   // mm per pixel
    flattenRefs(card);
    flattenPhoto(card);

    var foot = card.querySelector(".pdf-foot");
    var list = blocks(card);
    var all = list.map(function (b) { return b.el; }).concat([foot]);

    // Draw one block after the other, so that the browser is not overloaded
    var shots = [], runs = all.map(function (el) { return textRuns(el, k); });
    box.classList.add("notext");
    return all.reduce(function (p, el) {
      return p.then(function () { return shot(el); }).then(function (c) {
        shots.push({ img: c.toDataURL("image/jpeg", 0.95), h: c.height / c.width * el.offsetWidth * k, w: el.offsetWidth * k, links: links(el, k), runs: runs[shots.length] });
      });
    }, Promise.resolve()).then(function () {
      var f = shots.pop();                                // footer
      var doc = new jsPDF({ unit: "mm", format: "a4", compress: true });
      var top = PAGE.m, bottom = PAGE.h - PAGE.m - f.h - 4;
      var y = top, pages = 1;

      function footer() {
        var fy = PAGE.h - PAGE.m - f.h;
        doc.addImage(f.img, "JPEG", PAGE.m, fy, f.w, f.h);
        drawText(doc, f.runs, PAGE.m, fy);
        f.links.forEach(function (l) { doc.link(PAGE.m + l.x, fy + l.y, l.w, l.h, { url: l.url }); });
      }

      list.forEach(function (b, i) {
        var s = shots[i];
        var need = s.h, j = i;
        while (list[j].keep && shots[j + 1]) { need += list[j + 1].gap + shots[j + 1].h; j++; }
        var gap = y === top ? 0 : b.gap;
        if (y + gap + need > bottom && y > top) {
          footer(); doc.addPage(); pages++; y = top; gap = 0;
        }
        y += gap;
        doc.addImage(s.img, "JPEG", PAGE.m, y, s.w, s.h);
        drawText(doc, s.runs, PAGE.m, y);
        s.links.forEach(function (l) { doc.link(PAGE.m + l.x, y + l.y, l.w, l.h, { url: l.url }); });
        y += s.h;
      });
      footer();

      doc.setFontSize(7);
      doc.setTextColor(120, 130, 135);
      for (var p = 1; p <= pages; p++) {
        doc.setPage(p);
        doc.text(p + " / " + pages, PAGE.w - PAGE.m, PAGE.h - 4, { align: "right" });
      }
      doc.setProperties({ title: msg.title, subject: "Animal Culture Database species card", author: "Animal Culture Database" });
      doc.save(msg.file || "ACDB_card.pdf");
    });
  }

  // Fallback: the browser print window, where the user chooses Save as PDF
  function printWindow(msg) {
    var frame = document.createElement("iframe");
    frame.style.cssText = "position:fixed;right:0;bottom:0;width:0;height:0;border:0;";
    document.body.appendChild(frame);
    var doc = frame.contentDocument;
    doc.open();
    doc.write(
      '<!doctype html><html><head><meta charset="utf-8"><base href="' + document.baseURI + '">' +
      "<title>" + msg.title + "</title>" +
      '<link rel="stylesheet" href="acdb.css"><link rel="stylesheet" href="card.css">' +
      "<style>@page { size: A4; margin: 10mm; } html, body { background: #ffffff !important; } * { -webkit-print-color-adjust: exact; print-color-adjust: exact; }</style>" +
      '</head><body class="acdb-print">' + msg.html +
      '<script src="photos.js"><\/script></body></html>'
    );
    doc.close();
    setTimeout(function () {
      frame.contentWindow.onafterprint = function () { setTimeout(function () { frame.remove(); }, 500); };
      frame.contentWindow.focus();
      frame.contentWindow.print();
    }, 2500);
  }

  function download(msg) {
    if (busy) return;
    busy = true;
    var o = overlay("Making your card");
    var box = document.createElement("div");
    box.className = "acdb-capture";
    box.innerHTML = msg.html;
    document.body.appendChild(box);

    function end() { box.remove(); o.remove(); busy = false; }

    libs()
      .then(function () { return photoReady(box); })
      .then(function () { return wait(150); })
      .then(function () { return makePdf(msg, box); })
      .then(end)
      .catch(function (e) {
        if (window.console) console.error("ACDB card:", e);
        end();
        printWindow(msg);
      });
  }

  (function wait() {
    if (window.Shiny && Shiny.addCustomMessageHandler) Shiny.addCustomMessageHandler("acdb_print", download);
    else setTimeout(wait, 50);
  })();
})();
