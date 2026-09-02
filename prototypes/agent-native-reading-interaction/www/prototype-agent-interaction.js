(function () {
  const variants = [
    { key: "A", name: "Editorial seam" },
    { key: "B", name: "Companion rail" },
    { key: "C", name: "Context palette" },
    { key: "D", name: "Recommended hybrid" }
  ];
  const html = document.documentElement;
  const params = new URLSearchParams(window.location.search);
  let current = variants.findIndex(function (variant) {
    return variant.key === (params.get("variant") || "A").toUpperCase();
  });
  let mode = "queue";
  let scheduled = false;
  const dialogReturnFocus = new WeakMap();
  const mobileRail = window.matchMedia("(max-width: 575.98px)");

  if (current < 0) current = 0;

  function setHidden(selector, hidden) {
    const node = document.querySelector(selector);
    if (node) node.hidden = hidden;
  }

  function canRestoreFocus(node) {
    return Boolean(
      node &&
        node.isConnected &&
        typeof node.focus === "function" &&
        (!node.closest || !node.closest("[hidden], [aria-hidden='true']"))
    );
  }

  function focusWithoutScrolling(node) {
    if (!canRestoreFocus(node)) return;
    window.requestAnimationFrame(function () {
      if (canRestoreFocus(node)) node.focus({ preventScroll: true });
    });
  }

  function dialogFallback(dialog) {
    if (dialog.matches(".prototype-evidence-panel")) {
      const palette = document.querySelector(".prototype-context-palette");
      if (palette && !palette.hidden) {
        return palette.querySelector("[data-prototype-action='close-c']");
      }
    }
    if (dialog.matches(".prototype-context-palette")) {
      return document.querySelector("[data-prototype-action='open-c']");
    }
    return null;
  }

  function openDialog(selector, trigger) {
    const dialog = document.querySelector(selector);
    if (!dialog) return false;
    if (dialog.hidden) {
      dialogReturnFocus.set(dialog, trigger || document.activeElement);
    }
    dialog.hidden = false;
    const firstControl =
      dialog.querySelector("[data-prototype-action^='close']") ||
      dialog.querySelector(
        "button, [href], input, select, textarea, [tabindex]:not([tabindex='-1'])"
      );
    focusWithoutScrolling(firstControl);
    return true;
  }

  function closeDialog(selector, restoreFocus = true) {
    const dialog = document.querySelector(selector);
    if (!dialog) return false;
    const wasOpen = !dialog.hidden;
    const returnTarget = dialogReturnFocus.get(dialog);
    dialogReturnFocus.delete(dialog);
    dialog.hidden = true;
    if (wasOpen && restoreFocus) {
      focusWithoutScrolling(
        canRestoreFocus(returnTarget) ? returnTarget : dialogFallback(dialog)
      );
    }
    return wasOpen;
  }

  function closePalette(restoreFocus = true) {
    const closed = closeDialog(".prototype-context-palette", restoreFocus);
    setHidden(".prototype-c-proposal", true);
    return closed;
  }

  function syncCompanionRailAccessibility() {
    const rail = document.querySelector(".prototype-companion-rail");
    if (!rail) return;
    const closed =
      current === 1 &&
      mobileRail.matches &&
      html.dataset.agentRailOpen !== "true";
    rail.toggleAttribute("inert", closed);
    if (closed) rail.setAttribute("aria-hidden", "true");
    else rail.removeAttribute("aria-hidden");
  }

  function panelState() {
    if (current === 0) {
      const dock = document.querySelector(".prototype-conversation-dock");
      return dock && !dock.hidden ? "Conversation open" : "Conversation closed";
    }
    if (current === 1) {
      if (!mobileRail.matches) return "Rail open";
      return html.dataset.agentRailOpen === "true" ? "Rail open" : "Rail closed";
    }
    if (current === 2) {
      const palette = document.querySelector(".prototype-context-palette");
      return palette && !palette.hidden ? "Palette open" : "Palette closed";
    }
    if (mode === "queue") {
      return html.dataset.agentOrientationDismissed === "true"
        ? "Full queue"
        : "Orientation";
    }
    return html.dataset.agentRecommendedOpen === "true"
      ? "Conversation open"
      : "Conversation closed";
  }

  function updateState() {
    const variant = variants[current];
    const label = document.querySelector("[data-prototype-variant-label]");
    const state = document.querySelector("[data-prototype-state]");
    const orientationCoversReader =
      variant.key === "D" &&
      mode === "queue" &&
      html.dataset.agentOrientationDismissed !== "true";

    document.querySelectorAll("#reader_header, #reader_body").forEach(function (node) {
      if (orientationCoversReader) node.setAttribute("aria-hidden", "true");
      else node.removeAttribute("aria-hidden");
    });
    if (label) label.textContent = `${variant.key} \u00b7 ${variant.name}`;
    if (state) state.textContent = `${mode} \u00b7 ${panelState()}`;
    syncCompanionRailAccessibility();
  }

  function closePanels() {
    setHidden(".prototype-conversation-dock", true);
    closeDialog(".prototype-evidence-panel", false);
    closePalette(false);
    document.querySelectorAll(".prototype-action-proposal").forEach(function (node) {
      node.hidden = true;
    });
    html.dataset.agentRailOpen = "false";
    html.dataset.agentRecommendedOpen = "false";
    updateState();
  }

  function applyVariant(index) {
    current = (index + variants.length) % variants.length;
    const variant = variants[current];
    closePanels();
    html.dataset.agentVariant = variant.key;
    html.dataset.agentOrientationDismissed = "false";
    const url = new URL(window.location.href);
    url.searchParams.set("variant", variant.key);
    window.history.replaceState(
      window.history.state,
      "",
      `${url.pathname}${url.search}${url.hash}`
    );
    updateState();
  }

  function closeTopmostSurface() {
    if (closeDialog(".prototype-evidence-panel")) {
      updateState();
      return true;
    }
    if (closePalette()) {
      updateState();
      return true;
    }
    if (html.dataset.agentRecommendedOpen === "true") {
      html.dataset.agentRecommendedOpen = "false";
      setHidden(".prototype-d-proposal", true);
      focusWithoutScrolling(document.querySelector(".prototype-d-opener"));
      updateState();
      return true;
    }
    if (html.dataset.agentRailOpen === "true") {
      html.dataset.agentRailOpen = "false";
      setHidden(".prototype-b-proposal", true);
      focusWithoutScrolling(document.querySelector(".prototype-b-opener"));
      updateState();
      return true;
    }
    const dock = document.querySelector(".prototype-conversation-dock");
    if (dock && !dock.hidden) {
      dock.hidden = true;
      setHidden(".prototype-a-proposal", true);
      focusWithoutScrolling(
        document.querySelector("[data-prototype-action='open-a']")
      );
      updateState();
      return true;
    }
    return false;
  }

  function syncReaderContext() {
    const shell = document.querySelector(".app-shell");
    const nextMode = shell && shell.classList.contains("has-reader") ? "reader" : "queue";
    const titleNode = document.querySelector(".article-header h1");
    const title = titleNode ? titleNode.textContent.trim() : "the current story";

    if (nextMode !== mode) {
      mode = nextMode;
      closePanels();
    }
    html.dataset.agentMode = mode;
    document.querySelectorAll("[data-agent-story-title]").forEach(function (node) {
      if (node.textContent !== title) node.textContent = title;
    });
    updateState();
  }

  function scheduleSync() {
    if (scheduled) return;
    scheduled = true;
    window.requestAnimationFrame(function () {
      scheduled = false;
      syncReaderContext();
    });
  }

  function feedback(button) {
    const panel = button.closest(
      ".prototype-conversation-dock, .prototype-companion-rail, .prototype-context-palette, .prototype-recommended-rail"
    );
    const target = panel && panel.querySelector(".prototype-feedback");
    if (target) {
      target.textContent = "Prototype only \u00b7 no agent run or durable change occurred.";
    }
  }

  document.addEventListener("click", function (event) {
    const button = event.target.closest("[data-prototype-action]");
    if (!button) return;
    const action = button.dataset.prototypeAction;

    if (action === "previous-variant") applyVariant(current - 1);
    if (action === "next-variant") applyVariant(current + 1);
    if (action === "open-a") {
      if (variants[current].key === "D") html.dataset.agentRecommendedOpen = "true";
      else setHidden(".prototype-conversation-dock", false);
    }
    if (action === "carry-a") {
      if (variants[current].key === "D") {
        html.dataset.agentRecommendedOpen = "true";
        setHidden(".prototype-d-proposal", false);
      } else {
        setHidden(".prototype-conversation-dock", false);
        setHidden(".prototype-a-proposal", false);
      }
    }
    if (action === "close-a") {
      setHidden(".prototype-conversation-dock", true);
      setHidden(".prototype-a-proposal", true);
      focusWithoutScrolling(
        document.querySelector("[data-prototype-action='open-a']")
      );
    }
    if (action === "dismiss-proposal-a") {
      setHidden(".prototype-a-proposal", true);
      feedback(button);
    }
    if (action === "open-b") {
      html.dataset.agentRailOpen = "true";
      syncCompanionRailAccessibility();
      focusWithoutScrolling(
        document.querySelector("[data-prototype-action='close-b']")
      );
    }
    if (action === "close-b") {
      html.dataset.agentRailOpen = "false";
      setHidden(".prototype-b-proposal", true);
      syncCompanionRailAccessibility();
      focusWithoutScrolling(document.querySelector(".prototype-b-opener"));
    }
    if (action === "open-c") {
      openDialog(".prototype-context-palette", button);
    }
    if (action === "close-c") {
      closePalette();
    }
    if (action === "proposal-c") setHidden(".prototype-c-proposal", false);
    if (action === "dismiss-proposal-c") {
      setHidden(".prototype-c-proposal", true);
      feedback(button);
    }
    if (action === "proposal-b") setHidden(".prototype-b-proposal", false);
    if (action === "dismiss-proposal-b") {
      setHidden(".prototype-b-proposal", true);
      feedback(button);
    }
    if (action === "open-d") html.dataset.agentRecommendedOpen = "true";
    if (action === "close-d") {
      html.dataset.agentRecommendedOpen = "false";
      setHidden(".prototype-d-proposal", true);
      focusWithoutScrolling(document.querySelector(".prototype-d-opener"));
    }
    if (action === "proposal-d") setHidden(".prototype-d-proposal", false);
    if (action === "dismiss-proposal-d") {
      setHidden(".prototype-d-proposal", true);
      feedback(button);
    }
    if (action === "dismiss-orientation") {
      html.dataset.agentOrientationDismissed = "true";
    }
    if (action === "prototype-feedback") feedback(button);
    if (action === "show-evidence") {
      const panel = document.querySelector(".prototype-evidence-panel");
      const quote = document.querySelector("[data-prototype-evidence-quote]");
      if (quote) quote.textContent = button.dataset.prototypeEvidence || "";
      if (panel) openDialog(".prototype-evidence-panel", button);
    }
    if (action === "close-evidence") closeDialog(".prototype-evidence-panel");
    if (action === "select-story" && window.rillSelectEntry) {
      window.rillSelectEntry(
        button.dataset.entryId,
        Number.parseInt(button.dataset.entryPosition, 10)
      );
      closePalette();
      html.dataset.agentRailOpen = "false";
      html.dataset.agentRecommendedOpen = "false";
    }
    updateState();
  });

  document.addEventListener("submit", function (event) {
    const form = event.target.closest('[data-prototype-action="prototype-submit"]');
    if (!form) return;
    event.preventDefault();
    feedback(form);
    const textarea = form.querySelector("textarea");
    if (textarea) textarea.value = "";
  });

  document.addEventListener("keydown", function (event) {
    const interactive = event.target.closest(
      "input, textarea, select, button, a, [contenteditable='true'], [role='separator']"
    );
    if (interactive || event.altKey || event.ctrlKey || event.metaKey) return;
    if (event.key === "ArrowLeft") {
      event.preventDefault();
      applyVariant(current - 1);
    }
    if (event.key === "ArrowRight") {
      event.preventDefault();
      applyVariant(current + 1);
    }
  });

  document.addEventListener(
    "keydown",
    function (event) {
      if (
        event.key !== "Escape" ||
        event.defaultPrevented ||
        event.isComposing ||
        event.altKey ||
        event.ctrlKey ||
        event.metaKey ||
        event.shiftKey ||
        !closeTopmostSurface()
      ) {
        return;
      }
      event.preventDefault();
      event.stopPropagation();
    },
    true
  );

  document.addEventListener("DOMContentLoaded", function () {
    html.dataset.agentRailOpen = "false";
    html.dataset.agentRecommendedOpen = "false";
    applyVariant(current);
    syncReaderContext();
    const shell = document.querySelector(".app-shell");
    const observerRoot = shell || document.body;
    new MutationObserver(function (records) {
      const relevant = records.some(function (record) {
        if (record.type === "childList") return true;
        if (record.type !== "attributes" || record.attributeName !== "class") {
          return false;
        }
        const currentShell = shell || document.querySelector(".app-shell");
        return record.target === currentShell;
      });
      if (relevant) scheduleSync();
    }).observe(observerRoot, {
      childList: true,
      subtree: true,
      attributes: true,
      attributeFilter: ["class"]
    });
    if (typeof mobileRail.addEventListener === "function") {
      mobileRail.addEventListener("change", updateState);
    } else {
      mobileRail.addListener(updateState);
    }
  });
})();
