// What server.R did in the app, now in the browser: the map, the filters, the example population,
// the tree, the cards, and the species pages. The data come from assets/data.js (build_site.R).
(function () {
  var A = window.ACDB || { groups: [], species: {}, domains: [] };
  var root = document.body.getAttribute("data-root") || "";
  window.acdbAssets = root + "assets/";

  function $id(id) { return document.getElementById(id); }
  function esc(s) { return String(s == null ? "" : s).replace(/[&<>"]/g, function (c) { return { "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;" }[c]; }); }
  function cap(s) { s = String(s || ""); return s.charAt(0).toUpperCase() + s.slice(1); }
  function classLabel(c) { return (A.classNames && A.classNames[c]) || c; }
  function icon(name) { return '<i class="fas fa-' + name + '" role="presentation" aria-hidden="true"></i>'; }
  function chip(d) { return '<span class="chip chip-' + (A.domains.indexOf(d) % 10 + 1) + '">' + esc(cap(d)) + "</span>"; }
  function speciesUrl(id, gid) { return root + "species/" + encodeURIComponent(id) + ".html" + (gid ? "?g=" + gid : ""); }
  function plural(n, one, many) { return n === 1 ? one : many; }
  var byId = {};
  A.groups.forEach(function (g) { byId[g.id] = g; });

  window.acdbOpenSpecies = function (id) { window.location.href = speciesUrl(id); };

  // Photo box in the markup of photo_box() in build_site.R
  function photoBox(sci, cls, common, size) {
    var names = [sci, String(sci || "").split(" ").slice(0, 2).join(" ")].filter(function (x, i, a) { return x && a.indexOf(x) === i; });
    var panel = cls === "Aves" ? "birds" : cls === "Mammalia" ? "mammals" : "other";
    return '<div class="acdb-photo ' + panel + " " + (size || "") + '" data-species="' + esc(names.join("|")) + '" data-size="' + (size || "") + '">' +
      '<img alt="' + esc(common) + '"><div class="none">' + icon("paw") + "</div>" +
      '<span class="acdb-credit"><a href="#" target="_blank" rel="noopener">Photo: Wikipedia</a></span></div>';
  }

  // Photos in long lists wait until they are shown. Replacing the box lets photos.js see it
  function wake(scope) {
    Array.prototype.forEach.call(scope.querySelectorAll("[data-lazy-species]"), function (box) {
      var copy = box.cloneNode(true);
      copy.setAttribute("data-species", copy.getAttribute("data-lazy-species"));
      copy.removeAttribute("data-lazy-species");
      box.parentNode.replaceChild(copy, box);
    });
  }

  // Cards, rows, and list items with data-href open their page, unless a link or button inside was clicked
  document.addEventListener("click", function (ev) {
    var el = ev.target.closest("[data-href]");
    if (!el || ev.target.closest("a, button, input, select")) return;
    window.location.href = el.getAttribute("data-href");
  });

  // Keep the open page in view in the scrolling row of tabs on phones
  var activeTab = document.querySelector(".navbar-nav .active");
  if (activeTab && window.innerWidth < 992 && activeTab.scrollIntoView) activeTab.scrollIntoView({ inline: "center", block: "nearest" });

  // Species card, from the template on the species page. Links and images get full addresses
  function cardDate() {
    return new Date().toLocaleDateString("en-US", { year: "numeric", month: "long", day: "numeric" });
  }
  function cardFrom(doc, base) {
    var tpl = doc.getElementById("acdb-card");
    if (!tpl) return null;
    var box = document.createElement("div");
    box.innerHTML = tpl.innerHTML;
    Array.prototype.forEach.call(box.querySelectorAll("img[src]"), function (e) { e.setAttribute("src", new URL(e.getAttribute("src"), base).href); });
    Array.prototype.forEach.call(box.querySelectorAll("a[href]"), function (e) {
      if (e.getAttribute("href") !== "#") e.setAttribute("href", new URL(e.getAttribute("href"), base).href);
    });
    var date = box.querySelector(".card-date");
    if (date) date.textContent = cardDate();
    return box.innerHTML;
  }
  function loadCard(id) {
    var url = new URL(speciesUrl(id), window.location.href).href;
    return fetch(url).then(function (r) { return r.text(); }).then(function (text) {
      return cardFrom(new DOMParser().parseFromString(text, "text/html"), url);
    });
  }
  function printCard(id, html) {
    var sp = A.species[id] || { sci: id };
    if (!window.acdbPrint || !html) return;
    window.acdbPrint({ title: "ACDB card " + sp.sci, file: "ACDB_card_" + String(sp.sci).replace(/[^A-Za-z0-9]+/g, "_") + ".pdf", html: html });
  }

  // Explore -------------------------------------------------------------------------------------

  if ($id("featured")) {
    // Old links to the app: ?species=, #species=, and ?tab=
    var params = new URLSearchParams(window.location.search);
    var hash = window.location.hash.match(/species=([^&]+)/);
    var old = params.get("species") || (hash && decodeURIComponent(hash[1]));
    var tabs = { Map_tab: "map.html", Populations_tab: "populations.html", Tree_tab: "tree.html", Cards_tab: "cards.html", Stats_tab: "statistics.html",
                 Citations_tab: "citations.html", Papers_tab: "papers.html", About_tab: "about.html", Contributions_tab: "contribute.html" };
    if (old && A.species[old]) window.location.replace(speciesUrl(old));
    else if (tabs[params.get("tab")]) window.location.replace(tabs[params.get("tab")]);

    var examples = A.groups.filter(function (g) { return g.n > 0; });
    var example = null;
    var showExample = function () {
      var g = examples[Math.floor(Math.random() * examples.length)];
      if (!g) return;
      example = g;
      var sp = A.species[g.sp] || {};
      var iucn = g.iucn && A.iucnNames[g.iucn] ? '<span class="chip iucn-' + esc(g.iucn) + '">' + esc(A.iucnNames[g.iucn]) + "</span>" : "";
      $id("featured").innerHTML =
        '<div class="acdb-ex-body">' + photoBox(g.sci, g.cls, g.common, "large") +
        '<div class="acdb-ex-text">' +
        '<span class="chip chip-class">' + esc(classLabel(g.cls)) + "</span>" + iucn + g.domains.map(chip).join("") +
        "<h4>" + esc(g.name) + "</h4>" +
        '<div class="sp"><i>' + esc(g.sci) + "</i> | " + esc(sp.common) + "</div>" +
        '<div class="beh">' + esc(g.beh) + "</div>" +
        "<p>" + esc(g.lead) + "</p>" +
        '<div class="where">' + icon("map-marker-alt") + " " + esc(g.loc) + " | " + g.n + plural(g.n, " behavior recorded", " behaviors recorded") + "</div>" +
        '<a class="btn btn-primary" href="' + speciesUrl(g.sp, g.id) + '">See full details</a>' +
        '<button type="button" class="btn btn-default" id="example_card" style="margin-left: 8px;">' + icon("download") + " Get the card (PDF)</button>" +
        "</div></div>";
      $id("example_card").onclick = function () { loadCard(example.sp).then(function (html) { printCard(example.sp, html); }); };
    };
    showExample();
    $id("another").onclick = function (ev) { ev.preventDefault(); showExample(); };
  }

  // Map -------------------------------------------------------------------------------------------

  if ($id("population_map") && window.L) {
    var map = L.map("population_map", { minZoom: 2, maxBoundsViscosity: 1 });
    L.tileLayer("https://tile.openstreetmap.org/{z}/{x}/{y}.png", {
      noWrap: true, maxZoom: 19, attribution: '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors'
    }).addTo(map);
    map.setMaxBounds([[-85, -180], [85, 180]]);
    var layer = null, inData = [];

    var mapData = function () {
      var cls = $id("map_class").value, dom = $id("map_domain").value;
      return A.groups.filter(function (g) {
        return g.lat != null && g.lng != null && (cls === "All" || g.cls === cls) && (dom === "All" || g.domains.indexOf(dom) > -1);
      });
    };

    var popup = function (g) {
      return "<div style='font-family: Arial, sans-serif; font-size: 14px; line-height: 1.6; color: #333;'>" +
        "<b style='font-size: 16px; color: #1B7F79;'>" + esc(g.name) + "</b><br>" +
        "<b>Species:</b> <i>" + esc(g.sci) + "</i><br>" +
        "<b>Common name:</b> " + esc(g.common) + "<br>" +
        "<b>Recorded behaviors:</b> " + g.n + "<br>" +
        "<b>Location:</b> " + esc(g.loc) + "<br>" +
        "<a href='" + speciesUrl(g.sp, g.id) + "' style='color: #0F5F5A; text-decoration: underline;'>Open the species card</a></div>";
    };

    // Species in view. The list follows the map as people pan and zoom
    var side = function () {
      var b = map.getBounds();
      var d = inData.filter(function (g) { return g.lat >= b.getSouth() && g.lat <= b.getNorth() && g.lng >= b.getWest() && g.lng <= b.getEast(); });
      var ids = d.map(function (g) { return g.sp; }).filter(function (x, i, a) { return a.indexOf(x) === i; });
      ids.sort(function (x, y) { return (A.species[x].sort < A.species[y].sort) ? -1 : 1; });
      $id("side_note").textContent = ids.length + " species | " + d.length + plural(d.length, " population", " populations");
      if (d.length === 0) {
        $id("map_species").innerHTML = '<p class="acdb-hint" style="padding: 12px;">No populations in this view. Zoom out or move the map.</p>';
        return;
      }
      $id("map_species").innerHTML = ids.slice(0, 40).map(function (id) {
        var s = A.species[id], n = d.filter(function (g) { return g.sp === id; }).length;
        return '<div class="acdb-sp" data-href="' + speciesUrl(id) + '">' + photoBox(s.sci, s.cls, s.common, "mini") +
          '<div><b><a href="' + speciesUrl(id) + '">' + esc(s.common) + "</a></b><i>" + esc(s.sci) + "</i><small>" + n +
          plural(n, " population here | ", " populations here | ") + esc(classLabel(s.cls)) + "</small></div></div>";
      }).join("") + (ids.length > 40 ? '<p class="acdb-hint" style="padding: 8px 12px;">Zoom in to see the other species.</p>' : "");
    };
    var timer = null;
    map.on("moveend", function () { clearTimeout(timer); timer = setTimeout(side, 300); });

    var drawMap = function () {
      inData = mapData();
      if (layer) map.removeLayer(layer);
      layer = L.markerClusterGroup();
      inData.forEach(function (g) {
        var m = L.circleMarker([g.lat, g.lng], { radius: 7, color: "#ffffff", fillColor: "#E76F51", fillOpacity: 0.8, weight: 1.5 });
        m.bindTooltip(esc(g.name) + " (" + g.n + plural(g.n, " behavior)", " behaviors)"));
        m.bindPopup(popup(g));
        layer.addLayer(m);
      });
      map.addLayer(layer);
      if (inData.length > 0) {
        var lats = inData.map(function (g) { return g.lat; }), lngs = inData.map(function (g) { return g.lng; });
        map.fitBounds([[Math.min.apply(null, lats), Math.min.apply(null, lngs)], [Math.max.apply(null, lats), Math.max.apply(null, lngs)]]);
      } else {
        map.setView([20, 0], 2);
      }
      side();
    };
    $id("map_class").onchange = drawMap;
    $id("map_domain").onchange = drawMap;
    drawMap();
  }

  // Populations -----------------------------------------------------------------------------------

  if ($id("pop_cards")) {
    var cards = {};
    Array.prototype.forEach.call($id("pop_cards").children, function (c) { cards[c.getAttribute("data-id")] = c; });
    var shown = 12, list = [], allowed = {};
    var table = null;

    var popData = function () {
      var q = $id("pop_q").value.toLowerCase(), cls = $id("pop_class").value, dom = $id("pop_domain").value;
      var d = A.groups.filter(function (g) {
        if (cls !== "All" && g.cls !== cls) return false;
        if (dom !== "All" && g.domains.indexOf(dom) === -1) return false;
        if (q && [g.name, g.sci, g.common, g.loc].join(" ").toLowerCase().indexOf(q) === -1) return false;
        return true;
      });
      if ($id("pop_sort").value === "behaviors") d.sort(function (x, y) { return y.n - x.n || (x.name < y.name ? -1 : 1); });
      else d.sort(function (x, y) { return x.name.toLowerCase() < y.name.toLowerCase() ? -1 : 1; });
      return d;
    };

    var gallery = function () {
      var box = $id("pop_cards");
      Object.keys(cards).forEach(function (id) { cards[id].style.display = "none"; });
      list.slice(0, shown).forEach(function (g) {
        var c = cards[g.id];
        c.style.display = "";
        box.appendChild(c);
        wake(c);
      });
      $id("pop_empty").style.display = list.length === 0 ? "" : "none";
      $id("more").style.display = list.length > shown ? "" : "none";
    };

    var update = function () {
      list = popData();
      shown = 12;
      allowed = {};
      list.forEach(function (g) { allowed[g.id] = true; });
      $id("pop_note").textContent = "Showing " + list.length + " of " + A.groups.length + " populations";
      gallery();
      if (table) {
        table.order($id("pop_sort").value === "behaviors" ? [[5, "desc"], [1, "asc"]] : [[1, "asc"]]).draw();
      }
    };

    $id("more").onclick = function () { shown += 12; gallery(); };
    ["pop_class", "pop_domain", "pop_sort"].forEach(function (id) { $id(id).onchange = update; });
    $id("pop_q").oninput = update;

    // Download the populations that match the filters, with the columns of the app
    $id("pop_csv").onclick = function (ev) {
      ev.preventDefault();
      var q = function (x) { return x == null ? "NA" : typeof x === "number" ? String(x) : '"' + String(x).replace(/"/g, '""') + '"'; };
      var rows = [["group", "species", "common_name", "class", "order", "family", "domains", "behaviors", "location", "lat", "long", "region"].map(q).join(",")];
      list.forEach(function (g) {
        rows.push([g.name, g.sci, g.common, g.cls, g.ord, g.fam, g.domains.join("; "), g.n, g.loc, g.lat, g.lng, g.region].map(q).join(","));
      });
      var a = document.createElement("a");
      a.href = URL.createObjectURL(new Blob([rows.join("\n") + "\n"], { type: "text/csv" }));
      a.download = "ACDB_populations_" + new Date().toISOString().slice(0, 10) + ".csv";
      document.body.appendChild(a); a.click(); a.remove();
    };

    if (window.jQuery && jQuery.fn.dataTable) {
      jQuery.fn.dataTable.ext.search.push(function (settings, data, index) {
        if (settings.nTable.id !== "groups_table") return true;
        return !!allowed[settings.aoData[index].nTr.getAttribute("data-id")];
      });
      table = jQuery("#groups_table").DataTable({
        pageLength: 10, scrollX: true, order: [],
        columnDefs: [{ orderable: false, targets: [0, 3, 4] }],
        drawCallback: function () { wake($id("groups_table")); }
      });
      document.querySelector('a[href="#pop-table"]').addEventListener("shown.bs.tab", function () { table.columns.adjust(); });
    }
    update();
  }

  // Tree of life ----------------------------------------------------------------------------------

  if ($id("acdb_tree") && A.tree && window.acdbDrawTree) window.acdbDrawTree(A.tree);

  // Cards -----------------------------------------------------------------------------------------

  if ($id("card_preview")) {
    var select = $id("card_species"), cardHtml = null;
    var showCard = function () {
      var id = select.value;
      cardHtml = null;
      loadCard(id).then(function (html) {
        if (select.value !== id) return;
        cardHtml = html;
        $id("card_preview").innerHTML = html;
      });
    };
    var random = function () {
      var opts = select.options, i;
      do { i = Math.floor(Math.random() * opts.length); } while (opts.length > 1 && i === select.selectedIndex);
      select.selectedIndex = i;
      showCard();
    };
    select.onchange = showCard;
    $id("card_random").onclick = random;
    $id("card_print").onclick = function () { if (cardHtml) printCard(select.value, cardHtml); };
    random();
  }

  // Statistics ------------------------------------------------------------------------------------

  if ($id("stats_rank")) {
    $id("stats_rank").onchange = function () {
      var rank = this.value;
      Array.prototype.forEach.call(document.querySelectorAll(".acdb-rank"), function (d) {
        d.style.display = d.getAttribute("data-rank") === rank ? "" : "none";
      });
    };
  }

  // Citations and new papers ----------------------------------------------------------------------

  if (window.jQuery && jQuery.fn.dataTable) {
    if ($id("citations_table")) {
      jQuery("#citations_table").DataTable({ pageLength: 10, scrollX: true, order: [], columnDefs: [{ orderable: false, targets: 5 }] });
    }
    if ($id("papers_table")) {
      jQuery("#papers_table").DataTable({ pageLength: 10, order: [], language: { emptyTable: "No papers listed yet." } });
    }
  }

  // Species page ----------------------------------------------------------------------------------

  var mpage = document.querySelector(".acdb-mpage");
  if (mpage) {
    var sid = mpage.getAttribute("data-species");

    // The population that was clicked comes first and is marked
    var gid = new URLSearchParams(window.location.search).get("g");
    var picked = gid && $id("g" + gid);
    if (picked) {
      picked.classList.add("selected");
      picked.parentNode.insertBefore(picked, picked.parentNode.firstChild);
    }

    // Photos of relatives wait until their tab opens
    document.querySelector('a[href="#m-tree"]').addEventListener("shown.bs.tab", function () { wake($id("m-tree")); });

    // Map of every population of the species, drawn when its tab opens
    var modalMap = null;
    document.querySelector('a[href="#m-map"]').addEventListener("shown.bs.tab", function () {
      if (modalMap || !window.L) { if (modalMap) modalMap.invalidateSize(); return; }
      var pts = JSON.parse($id("acdb-points").textContent || "[]");
      modalMap = L.map("modal_map");
      L.tileLayer("https://tile.openstreetmap.org/{z}/{x}/{y}.png", {
        maxZoom: 19, attribution: '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors'
      }).addTo(modalMap);
      pts.forEach(function (p) {
        L.circleMarker([p.lat, p.lng], { radius: 9, color: "#ffffff", fillColor: "#E76F51", fillOpacity: 0.85, weight: 2 })
          .bindTooltip(esc(p.name)).bindPopup("<b>" + esc(p.name) + "</b><br>" + esc(p.loc)).addTo(modalMap);
      });
      if (pts.length === 1) modalMap.setView([pts[0].lat, pts[0].lng], 6);
      else if (pts.length > 1) {
        var la = pts.map(function (p) { return p.lat; }), lo = pts.map(function (p) { return p.lng; });
        modalMap.fitBounds([[Math.min.apply(null, la) - 1, Math.min.apply(null, lo) - 1], [Math.max.apply(null, la) + 1, Math.max.apply(null, lo) + 1]]);
      } else modalMap.setView([20, 0], 2);
    });

    $id("modal_pdf").onclick = function () { printCard(sid, cardFrom(document, window.location.href)); };

    $id("copy_link").onclick = function () {
      var btn = this, url = document.querySelector('link[rel="canonical"]').href;
      var done = function () { btn.textContent = "Link copied"; };
      if (navigator.clipboard && navigator.clipboard.writeText) navigator.clipboard.writeText(url).then(done, function () { window.prompt("Copy this link", url); });
      else window.prompt("Copy this link", url);
    };

    // Close goes back to where people came from on this site, or to the populations
    var close = function () {
      var fromHere = document.referrer && new URL(document.referrer).origin === window.location.origin;
      if (fromHere && window.history.length > 1) window.history.back();
      else window.location.href = root + "populations.html";
    };
    $id("modal_close").onclick = close;
    document.querySelector(".acdb-mclose").onclick = close;
    document.addEventListener("keydown", function (ev) { if (ev.key === "Escape") close(); });
  }
})();
