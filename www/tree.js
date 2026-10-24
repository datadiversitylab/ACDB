// Radial tree of the species in the ACDB, built from the taxonomy.
// Rings show which behavior domains each species has. Data arrive from the server.
(function () {
  var NS = "http://www.w3.org/2000/svg";
  var RANKS = ["phy", "cls", "ord", "fam", "gen"];
  var RADII = [0, 62, 112, 158, 198, 228];      // root, phylum, class, order, family, genus
  var TIP = 248;                                 // species
  var RING0 = 262, RING_STEP = 10;               // domain rings
  var data = null, highlight = null;

  function el(name, attrs, parent) {
    var e = document.createElementNS(NS, name);
    for (var k in attrs) e.setAttribute(k, attrs[k]);
    if (parent) parent.appendChild(e);
    return e;
  }

  function polar(r, a) { return [r * Math.sin(a), -r * Math.cos(a)]; }
  function pt(r, a) { var p = polar(r, a); return p[0].toFixed(1) + " " + p[1].toFixed(1); }

  function arr(x) { return x === null || x === undefined ? [] : [].concat(x); }

  // Nest the species by phylum > class > order > family > genus
  function build(tips) {
    var root = { name: "Animalia", level: 0, kids: [] };
    tips.forEach(function (t) {
      var node = root;
      RANKS.forEach(function (rk, i) {
        var name = t[rk];
        var kid = node.kids.filter(function (k) { return k.name === name; })[0];
        if (!kid) { kid = { name: name, level: i + 1, rank: rk, kids: [], cls: t.cls }; node.kids.push(kid); }
        node = kid;
      });
      node.kids.push({ name: t.sci, level: 6, tip: t, cls: t.cls, kids: [] });
    });
    (function sort(n) {
      n.kids.sort(function (a, b) { return a.name < b.name ? -1 : 1; });
      n.kids.forEach(sort);
    })(root);
    return root;
  }

  // Give each species an angle, with a small gap between classes
  function place(root) {
    var leaves = [];
    (function walk(n) { if (n.tip) leaves.push(n); n.kids.forEach(walk); })(root);
    var gap = 0.8, total = leaves.length, prev = null, pos = 0;
    leaves.forEach(function (l) {
      if (prev && prev.cls !== l.cls) pos += gap;
      l.pos = pos; pos += 1; prev = l;
    });
    var span = pos;
    leaves.forEach(function (l) { l.a = (l.pos / span) * 2 * Math.PI; });
    (function mid(n) {
      if (n.tip) return;
      n.kids.forEach(mid);
      n.a = (n.kids[0].a + n.kids[n.kids.length - 1].a) / 2;
    })(root);
  }

  function count(n) { return n.tip ? 1 : n.kids.reduce(function (s, k) { return s + count(k); }, 0); }

  function colorOf(cls) { return data.classColors[cls] || "#5F7480"; }

  function radius(n) { return n.tip ? TIP : RADII[n.level]; }

  function drawEdges(n, g) {
    if (!n.kids.length) return;
    var r = radius(n);
    var a0 = n.kids[0].a, a1 = n.kids[n.kids.length - 1].a;
    var color = n.level <= 1 ? "#9fb0b5" : colorOf(n.cls);
    if (r > 0 && n.kids.length > 1) {
      var large = (a1 - a0) > Math.PI ? 1 : 0;
      el("path", { d: "M" + pt(r, a0) + " A" + r + " " + r + " 0 " + large + " 1 " + pt(r, a1), fill: "none", stroke: color, "stroke-width": 1.6, opacity: 0.8 }, g);
    }
    n.kids.forEach(function (k) {
      var kc = k.level <= 1 || n.level === 0 ? "#9fb0b5" : colorOf(k.cls);
      el("path", { d: "M" + pt(r, k.a) + " L" + pt(radius(k), k.a), fill: "none", stroke: kc, "stroke-width": 1.6, opacity: 0.8 }, g);
      drawEdges(k, g);
    });
  }

  function showTip(ev, t) {
    var tip = document.getElementById("acdb_tip");
    var names = [t.sci].concat(t.sci.split(" ").slice(0, 2).join(" ")).filter(function (x, i, a) { return a.indexOf(x) === i; });
    var chips = arr(t.domains).map(function (d) {
      return '<span class="chip chip-' + data.domainColors[d] + '">' + d.charAt(0).toUpperCase() + d.slice(1) + "</span>";
    }).join("");
    tip.innerHTML =
      '<div class="acdb-photo mini ' + (t.cls === "Aves" ? "birds" : t.cls === "Mammalia" ? "mammals" : "other") + '" data-size="mini" data-species="' + names.join("|") + '"><img alt=""><div class="none"></div></div>' +
      "<div><b>" + t.common + "</b><i>" + t.sci + "</i><small>" + t.ord + " | " + t.groups + (t.groups === 1 ? " population" : " populations") + "</small>" + chips + "</div>";
    tip.style.display = "flex";
    moveTip(ev);
  }

  function moveTip(ev) {
    var tip = document.getElementById("acdb_tip");
    var x = ev.clientX + 16, y = ev.clientY + 16;
    if (x + tip.offsetWidth > window.innerWidth - 8) x = ev.clientX - tip.offsetWidth - 16;
    if (y + tip.offsetHeight > window.innerHeight - 8) y = ev.clientY - tip.offsetHeight - 16;
    tip.style.left = x + "px"; tip.style.top = y + "px";
  }

  function hideTip() { document.getElementById("acdb_tip").style.display = "none"; }

  function legend() {
    var box = document.getElementById("acdb_legend");
    box.innerHTML = "";
    var lab = document.createElement("span"); lab.className = "lab"; lab.textContent = "Highlight a domain";
    box.appendChild(lab);
    data.domains.forEach(function (d) {
      var b = document.createElement("button");
      b.type = "button";
      b.className = "chip chip-" + data.domainColors[d] + (highlight === d ? " on" : "");
      b.textContent = d.charAt(0).toUpperCase() + d.slice(1);
      b.onclick = function () { highlight = highlight === d ? null : d; legend(); paint(); };
      box.appendChild(b);
    });
  }

  // Dim species and dots that do not match the highlighted domain
  function paint() {
    var svg = document.getElementById("acdb_tree");
    svg.querySelectorAll(".sp").forEach(function (g) {
      var doms = g.getAttribute("data-domains").split("|");
      var on = !highlight || doms.indexOf(highlight) > -1;
      g.style.opacity = on ? 1 : 0.18;
      g.querySelectorAll(".dot").forEach(function (c) {
        var d = c.getAttribute("data-d");
        c.style.opacity = (!highlight || d === highlight) ? 1 : 0.2;
      });
    });
  }

  function draw(d) {
    data = d;
    data.tips.forEach(function (t) { t.domains = arr(t.domains); });
    var host = document.getElementById("acdb_tree");
    if (!host) return;
    host.innerHTML = "";
    var root = build(data.tips);
    place(root);

    var size = RING0 + RING_STEP * data.domains.length + 150;
    var svg = el("svg", { viewBox: [-size, -size, size * 2, size * 2].join(" "), width: "100%", role: "img", "aria-label": "Tree of the species in the ACDB" });
    host.appendChild(svg);

    // Guide circles for the domain rings
    data.domains.forEach(function (dm, i) {
      el("circle", { cx: 0, cy: 0, r: RING0 + i * RING_STEP, fill: "none", stroke: "#eef2f2", "stroke-width": 1 }, svg);
    });

    var edges = el("g", {}, svg);
    drawEdges(root, edges);

    // Clade nodes
    (function nodes(n) {
      if (n.tip) return;
      if (n.level >= 2 && n.level <= 3) {
        var p = polar(radius(n), n.a);
        var c = el("circle", { cx: p[0], cy: p[1], r: n.level === 2 ? 5 : 3.5, fill: "#ffffff", stroke: colorOf(n.cls), "stroke-width": 2 }, svg);
        var ti = el("title", {}, c);
        ti.textContent = (n.level === 2 ? "Class " : "Order ") + n.name + ": " + count(n) + " species";
      }
      n.kids.forEach(nodes);
    })(root);

    // Class labels on the inner rings
    (function labels(n) {
      if (n.level === 2) {
        var p = polar(radius(n) - 18, n.a);
        var t = el("text", { x: p[0], y: p[1], "text-anchor": "middle", "dominant-baseline": "middle", "font-size": 12, "font-weight": 700, fill: colorOf(n.cls) }, svg);
        t.textContent = n.name;
      }
      n.kids.forEach(labels);
    })(root);

    // Species
    (function tips(n) {
      if (n.tip) {
        var t = n.tip, a = n.a;
        var g = el("g", { "class": "sp", "data-domains": t.domains.join("|"), style: "cursor:pointer" }, svg);
        var p = polar(TIP, a);
        el("circle", { cx: p[0], cy: p[1], r: 4.2, fill: colorOf(t.cls) }, g);
        data.domains.forEach(function (dm, i) {
          var q = polar(RING0 + i * RING_STEP, a);
          var has = t.domains.indexOf(dm) > -1;
          el("circle", { "class": "dot", "data-d": dm, cx: q[0], cy: q[1], r: has ? 3.6 : 1.6, fill: has ? data.domainHex[dm] : "#dfe6e7" }, g);
        });
        var deg = a * 180 / Math.PI - 90, flip = deg > 90;
        var lr = RING0 + data.domains.length * RING_STEP + 4;
        var lp = polar(lr, a);
        var tx = el("text", {
          x: lp[0], y: lp[1], "font-size": 10.5, "font-style": "italic", fill: "#24343A",
          "text-anchor": flip ? "end" : "start", "dominant-baseline": "middle",
          transform: "rotate(" + (flip ? deg + 180 : deg) + " " + lp[0].toFixed(1) + " " + lp[1].toFixed(1) + ")"
        }, g);
        tx.textContent = t.sci;
        // A wide invisible wedge makes the species easy to hit
        var w = 0.5 * (2 * Math.PI / data.tips.length);
        el("path", { d: "M" + pt(TIP - 8, a - w) + " L" + pt(lr + 100, a - w) + " L" + pt(lr + 100, a + w) + " L" + pt(TIP - 8, a + w) + " Z", fill: "transparent" }, g);
        g.addEventListener("mouseenter", function (ev) { showTip(ev, t); });
        g.addEventListener("mousemove", moveTip);
        g.addEventListener("mouseleave", hideTip);
        g.addEventListener("click", function () {
          if (window.Shiny) Shiny.setInputValue("species_click", t.id, { priority: "event" });
        });
      }
      n.kids.forEach(tips);
    })(root);

    legend();
    paint();
  }

  (function wait() {
    if (window.Shiny && Shiny.addCustomMessageHandler) Shiny.addCustomMessageHandler("acdb_tree", draw);
    else setTimeout(wait, 50);
  })();
  window.acdbDrawTree = draw;
})();
