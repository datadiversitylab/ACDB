// The app inside another website (an iframe).
// With ?embed=1 the app drops what the host page already shows. It also tells the host page what
// people do (tab, species, downloads), so the host can count usage and keep its address in step.
(function () {
  // Websites that may send the app their address, when the browser hides the referrer
  var HOSTS = ["https://datadiversitylab.github.io"];

  var embedded = window.parent !== window;
  var params = new URLSearchParams(window.location.search);
  var state = { tab: null, species: null };
  var hostUrl = null;

  if (params.get("embed") === "1") document.documentElement.classList.add("acdb-embed");

  function post(msg) {
    if (embedded) window.parent.postMessage(msg, "*");
  }

  function sendState() { post({ type: "acdb:state", tab: state.tab, species: state.species }); }

  function track(name, data) { post({ type: "acdb:event", name: name, data: data || {} }); }

  // Address people can share: the host page when there is one, and the app otherwise
  function shareUrl(id) {
    if (hostUrl) return hostUrl + "#species=" + encodeURIComponent(id);
    return window.location.origin + window.location.pathname + "?species=" + encodeURIComponent(id);
  }

  window.acdbCopyLink = function (id, btn) {
    var url = shareUrl(id);
    var done = function () { btn.textContent = "Link copied"; track("link_copy", { species: id }); };
    if (navigator.clipboard && navigator.clipboard.writeText) navigator.clipboard.writeText(url).then(done, function () { window.prompt("Copy this link", url); });
    else window.prompt("Copy this link", url);
  };

  // The host sends its address, so that copied links point to the host page
  window.addEventListener("message", function (ev) {
    if (ev.source !== window.parent || !ev.data || ev.data.type !== "acdb:init") return;
    var allowed = HOSTS.slice();
    try { if (document.referrer) allowed.push(new URL(document.referrer).origin); } catch (e) {}
    if (allowed.indexOf(ev.origin) === -1) return;
    hostUrl = String(ev.data.url || "").split("#")[0] || null;
  });

  // Tab changes come from Shiny's own input
  $(document).on("shiny:inputchanged", function (ev) {
    if (ev.name !== "tabs" || !ev.value || ev.value === state.tab) return;
    state.tab = ev.value;
    track("tab", { tab: ev.value });
    sendState();
  });

  // Events from the server: species opened, card downloaded, and so on
  function register() {
    Shiny.addCustomMessageHandler("acdb_track", function (m) {
      track(m.name, m.data);
      if (m.name === "species_open") { state.species = m.data.species; sendState(); }
    });
  }

  // The species window closed
  function closed() {
    if (state.species) { state.species = null; sendState(); }
  }
  document.addEventListener("hidden.bs.modal", closed);
  $(document).on("hidden.bs.modal", closed);

  (function wait() {
    if (window.Shiny && Shiny.addCustomMessageHandler) register();
    else setTimeout(wait, 50);
  })();

  $(document).on("shiny:connected", function () { post({ type: "acdb:ready" }); });
})();
