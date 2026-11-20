// Species photos and a short description from Wikipedia. Each .acdb-photo and .acdb-about has
// data-species with the names to try.
(function () {
  var sizes = { mini: 120, large: 960 };
  var cache = {};
  try { cache = JSON.parse(localStorage.getItem("acdb_wiki") || "{}"); } catch (e) {}

  function save() {
    try { localStorage.setItem("acdb_wiki", JSON.stringify(cache)); } catch (e) {}
  }

  // Ask for a larger version when the original is big enough
  function sized(p, box) {
    var size = sizes[box.getAttribute("data-size")] || 500;
    return p.w >= size ? p.src.replace(/\/\d+px-/, "/" + size + "px-") : p.src;
  }

  function show(box, p) {
    var img = box.querySelector("img");
    img.onload = function () { box.classList.add("has-photo"); };
    img.src = sized(p, box);
    var link = box.querySelector(".acdb-credit a");
    if (link) link.href = p.page;
  }

  function find(name) {
    if (name in cache) return Promise.resolve(cache[name]);
    var url = "https://en.wikipedia.org/api/rest_v1/page/summary/" + encodeURIComponent(name.replace(/ /g, "_"));
    return fetch(url)
      .then(function (r) { return r.ok ? r.json() : null; })
      .then(function (d) {
        var p = d && d.type === "standard" && d.content_urls ? {
          src: d.thumbnail ? d.thumbnail.source : "",
          w: d.originalimage ? d.originalimage.width : 0,
          page: d.content_urls.desktop.page,
          title: d.title,
          text: d.extract || ""
        } : null;
        cache[name] = p;
        save();
        return p;
      })
      .catch(function () { return null; });
  }

  function load(box) {
    if (box.getAttribute("data-done") || !box.getAttribute("data-species")) return;
    box.setAttribute("data-done", "1");
    var names = box.getAttribute("data-species").split("|");
    (function next(i) {
      if (i >= names.length) return;
      find(names[i]).then(function (p) { if (p && p.src) show(box, p); else next(i + 1); });
    })(0);
  }

  // First paragraph, shortened at a sentence, with the credit that Wikipedia asks for (CC BY-SA 4.0)
  function paragraph(text) {
    if (text.length <= 650) return { text: text, short: false };
    var cut = text.slice(0, 650), end = Math.max(cut.lastIndexOf(". "), cut.lastIndexOf(".\u201D "));
    return { text: end > 200 ? cut.slice(0, end + 1) : cut.replace(/\s+\S*$/, "") + "...", short: true };
  }

  function link(href, label) {
    var a = document.createElement("a");
    a.href = href; a.target = "_blank"; a.rel = "noopener"; a.textContent = label;
    return a;
  }

  function fill(box, p) {
    var para = paragraph(p.text);
    var txt = document.createElement("p");
    txt.className = "txt";
    txt.textContent = para.text;
    var credit = document.createElement("div");
    credit.className = "credit";
    credit.appendChild(document.createTextNode("Text from the Wikipedia article \u201C"));
    credit.appendChild(link(p.page, p.title));
    credit.appendChild(document.createTextNode("\u201D" + (para.short ? ", shortened" : "") + " ("));
    credit.appendChild(link("https://en.wikipedia.org/w/index.php?title=" + encodeURIComponent(p.title.replace(/ /g, "_")) + "&action=history", "authors"));
    credit.appendChild(document.createTextNode("), used under "));
    credit.appendChild(link("https://creativecommons.org/licenses/by-sa/4.0/", "CC BY-SA 4.0"));
    credit.appendChild(document.createTextNode("."));
    box.innerHTML = "";
    box.appendChild(txt);
    box.appendChild(credit);
    box.classList.add("done");
  }

  function loadAbout(box) {
    if (box.getAttribute("data-done") || !box.getAttribute("data-species")) return;
    box.setAttribute("data-done", "1");
    var names = box.getAttribute("data-species").split("|");
    (function next(i) {
      if (i >= names.length) { box.classList.add("none"); return; }
      find(names[i]).then(function (p) { if (p && p.text) fill(box, p); else next(i + 1); });
    })(0);
  }

  function scan() {
    document.querySelectorAll(".acdb-photo[data-species]").forEach(load);
    document.querySelectorAll(".acdb-about[data-species]").forEach(loadAbout);
  }

  new MutationObserver(scan).observe(document.documentElement, { childList: true, subtree: true });
  document.addEventListener("DOMContentLoaded", scan);
})();
