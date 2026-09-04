(function () {
  const themeStorageKey = "rill-theme-mode";
  const themeModes = new Set(["system", "light", "dark"]);
  const systemDarkMode = window.matchMedia("(prefers-color-scheme: dark)");
  const compactReaderMode = window.matchMedia("(max-width: 767.98px)");
  const mediumReaderMode = window.matchMedia(
    "(min-width: 768px) and (max-width: 1099.98px)"
  );
  const wideReaderMode = window.matchMedia("(min-width: 1100px)");
  const desktopReaderMode = window.matchMedia("(min-width: 768px)");
  const overlaidAgentMode = window.matchMedia("(max-width: 1499.98px)");

  function storedThemeMode() {
    try {
      const stored = window.localStorage.getItem(themeStorageKey);
      return themeModes.has(stored) ? stored : "system";
    } catch (_error) {
      return "system";
    }
  }

  function resolvedTheme(mode) {
    if (mode === "system") return systemDarkMode.matches ? "dark" : "light";
    return mode;
  }

  function syncThemeControl(mode) {
    const selected = document.querySelector(
      `input[name="rill_theme_mode"][value="${mode}"]`
    );
    if (selected) selected.checked = true;
  }

  function applyThemeMode(mode, options = {}) {
    const nextMode = themeModes.has(mode) ? mode : "system";
    const persist = options.persist !== false;
    const notify = options.notify === true;

    document.documentElement.dataset.rillThemeMode = nextMode;
    document.documentElement.dataset.bsTheme = resolvedTheme(nextMode);
    syncThemeControl(nextMode);

    if (persist) {
      try {
        window.localStorage.setItem(themeStorageKey, nextMode);
      } catch (_error) {
        // A private browsing policy may make storage unavailable.
      }
    }

    if (notify && window.Shiny) {
      window.Shiny.setInputValue("theme_mode", nextMode, {
        priority: "event"
      });
    }
  }

  applyThemeMode(storedThemeMode(), { persist: false });

  let activeEntryId = null;
  let activeDocumentId = null;
  let activeEntrySurface = null;
  let pendingEntrySurface = null;
  let pendingReaderFocus = false;
  let pendingOrientationDismissal = null;
  let pendingOrientationSelectionCardId = null;
  let pendingOrientationReturnFocus = null;
  let openedAt = 0;
  let lastHeartbeatAt = 0;
  let accumulatedReadingMs = 0;
  let readingTelemetryPaused = false;
  let milestones = new Set();
  let fileResetRegistered = false;
  let chatSubmissionRegistered = false;
  let queueBrowseRegistered = false;
  let orientationActionMessagesRegistered = false;
  let chatSubmissionIndex = 0;
  let orientationModalWasOpen = false;
  let compactSurface = null;
  let compactReturnSurface = "queue";
  let agentReturnFocus = null;
  let agentSidebarExpanded = false;
  let agentSidebarObserver = null;
  let agentFocusPending = false;
  let connectionState = "starting";
  let hasConnected = false;
  let initialLoadComplete = false;
  let systemStatusTimer = null;
  let systemEventsRegistered = false;
  let inputValidityRegistered = false;
  let readingStatusTrigger = null;

  function setAppBusy(busy) {
    const app = document.getElementById("rill-app");
    if (app) app.setAttribute("aria-busy", String(busy));
  }

  function setSystemStatus(
    state,
    title,
    detail,
    { action = false, assertive = false, hideAfter = null } = {}
  ) {
    const status = document.getElementById("rill-system-status");
    const titleNode = document.getElementById("rill-system-status-title");
    const detailNode = document.getElementById("rill-system-status-detail");
    const actionNode = document.getElementById("rill-system-status-action");
    if (!status || !titleNode || !detailNode || !actionNode) return;

    window.clearTimeout(systemStatusTimer);
    status.hidden = false;
    status.dataset.state = state;
    status.className = `rill-system-status is-${state}`;
    status.setAttribute("role", assertive ? "alert" : "status");
    status.setAttribute("aria-live", assertive ? "assertive" : "polite");
    titleNode.textContent = title;
    detailNode.textContent = detail;
    actionNode.hidden = !action;

    if (hideAfter !== null) {
      systemStatusTimer = window.setTimeout(function () {
        status.hidden = true;
      }, hideAfter);
    }
  }

  function handleShinyConnected() {
    if (connectionState === "connected") return;
    const restored = hasConnected || connectionState === "disconnected";
    hasConnected = true;
    connectionState = "connected";
    setAppBusy(false);
    if (restored) {
      setSystemStatus(
        "restored",
        "Connection restored",
        "Your Library and reading position are available again.",
        { hideAfter: 2400 }
      );
    } else {
      setSystemStatus(
        "loading",
        "Opening your Library",
        "Loading feeds, stories, and your reading position\u2026"
      );
    }
  }

  function handleShinyDisconnected() {
    connectionState = navigator.onLine ? "disconnected" : "offline";
    setAppBusy(true);
    if (connectionState === "offline") {
      setSystemStatus(
        "offline",
        "You\u2019re offline",
        "Your place is safe. Reconnect to continue with your Library.",
        { action: true, assertive: true }
      );
    } else {
      setSystemStatus(
        "disconnected",
        "Connection interrupted",
        "Your place is safe. Rill will try to reconnect.",
        { action: true, assertive: true }
      );
    }
  }

  function matchingNodes(root, selector) {
    const nodes = [];
    if (root instanceof Element && root.matches(selector)) nodes.push(root);
    if (typeof root.querySelectorAll === "function") {
      nodes.push(...root.querySelectorAll(selector));
    }
    return nodes;
  }

  function enhanceNativeFeedback(root = document) {
    matchingNodes(root, ".shiny-notification").forEach(function (notice) {
      const urgent = notice.matches(
        ".shiny-notification-error, .shiny-notification-warning"
      );
      notice.setAttribute("role", urgent ? "alert" : "status");
      notice.setAttribute("aria-live", urgent ? "assertive" : "polite");
      notice.setAttribute("aria-atomic", "true");
      const close = notice.querySelector(".shiny-notification-close");
      if (close) close.setAttribute("aria-label", "Dismiss notification");
    });

    matchingNodes(root, ".shiny-output-error").forEach(function (error) {
      error.setAttribute("role", "alert");
      error.setAttribute("aria-live", "assertive");
      error.setAttribute("aria-atomic", "true");
    });

    matchingNodes(root, ".shiny-progress .progress").forEach(
      function (progress) {
        progress.setAttribute("role", "progressbar");
        const container = progress.closest(".shiny-progress");
        const message = container && container.querySelector(".progress-message");
        progress.setAttribute(
          "aria-label",
          (message && message.textContent.trim()) || "Rill progress"
        );
      }
    );

    matchingNodes(root, ".popover").forEach(function (popover) {
      if (!popover.querySelector(".btn-read-action")) return;
      const title = popover.querySelector(".popover-header");
      if (title) {
        title.id = title.id || `${popover.id}-title`;
        popover.setAttribute("aria-labelledby", title.id);
      } else {
        popover.setAttribute("aria-label", "Reading status");
      }
      popover.setAttribute("role", "dialog");
    });
  }

  function registerSystemEvents() {
    if (systemEventsRegistered || !window.jQuery) return;
    const events = window.jQuery(document);
    events.on("shiny:connected.rillSystemStatus", handleShinyConnected);
    events.on("shiny:disconnected.rillSystemStatus", handleShinyDisconnected);
    events.on("shiny:busy.rillSystemStatus", function () {
      setAppBusy(true);
    });
    events.on("shiny:idle.rillSystemStatus", function () {
      if (connectionState !== "connected") return;
      setAppBusy(false);
      if (!initialLoadComplete) {
        initialLoadComplete = true;
        setSystemStatus(
          "ready",
          "Library ready",
          "Rill is ready for reading.",
          { hideAfter: 800 }
        );
      }
    });
    events.on("shiny:recalculating.rillSystemStatus", function (event) {
      if (event.target instanceof Element) {
        event.target.setAttribute("aria-busy", "true");
      }
    });
    events.on("shiny:recalculated.rillSystemStatus", function (event) {
      if (event.target instanceof Element) {
        event.target.setAttribute("aria-busy", "false");
      }
    });
    systemEventsRegistered = true;

    window.setTimeout(function () {
      const socket =
        window.Shiny &&
        window.Shiny.shinyapp &&
        window.Shiny.shinyapp.$socket;
      if (socket && socket.readyState === WebSocket.OPEN) {
        handleShinyConnected();
      }
    }, 0);
  }

  function setInputValidity(message) {
    const input = document.getElementById(message.id);
    if (!input) return;
    const feedbackId = `${message.id}-feedback`;
    let feedback = document.getElementById(feedbackId);
    const describedBy = new Set(
      (input.getAttribute("aria-describedby") || "")
        .split(/\s+/)
        .filter(Boolean)
    );

    if (message.invalid) {
      input.setAttribute("aria-invalid", "true");
      if (!feedback) {
        feedback = document.createElement("p");
        feedback.id = feedbackId;
        feedback.className = "rill-input-feedback";
        input.closest(".form-group, .shiny-input-container")?.append(feedback);
      }
      feedback.textContent = message.message || "Check this value.";
      describedBy.add(feedbackId);
    } else {
      input.removeAttribute("aria-invalid");
      if (feedback) feedback.remove();
      describedBy.delete(feedbackId);
    }

    if (describedBy.size) {
      input.setAttribute("aria-describedby", [...describedBy].join(" "));
    } else {
      input.removeAttribute("aria-describedby");
    }
  }

  function registerInputValidity() {
    if (inputValidityRegistered || !window.Shiny) return;
    window.Shiny.addCustomMessageHandler("rill-input-validity", setInputValidity);
    inputValidityRegistered = true;
  }

  window.rillRecoverConnection = function () {
    window.location.reload();
  };

  window.rillUiAudit = function () {
    const root = document.documentElement;
    const body = document.body;
    const app = document.getElementById("rill-app");
    const selected = document.getElementById("reader-document");
    const activeSurface = app && app.dataset.compactSurface;
    return {
      viewportWidth: window.innerWidth,
      horizontalOverflow:
        root.scrollWidth > window.innerWidth + 1 ||
        body.scrollWidth > window.innerWidth + 1,
      appBusy: app && app.getAttribute("aria-busy"),
      connectionState,
      activeSurface: activeSurface || "multi-pane",
      selectedEntryId: selected && selected.dataset.entryId,
      selectedDocumentId: selected && selected.dataset.documentId,
      focusedElement:
        document.activeElement &&
        (document.activeElement.id ||
          document.activeElement.getAttribute("aria-label") ||
          document.activeElement.textContent.trim().slice(0, 80)),
      visibleDialogs: matchingNodes(document, '[role="dialog"]').filter(
        function (dialog) {
          return dialog.getClientRects().length > 0;
        }
      ).length
    };
  };

  function eventId() {
    if (window.crypto && window.crypto.randomUUID) return window.crypto.randomUUID();
    return `event-${Date.now()}-${Math.random().toString(16).slice(2)}`;
  }

  function send(type, details = {}) {
    if (!window.Shiny || !activeEntryId) return;
    window.Shiny.setInputValue(
      "client_event",
      {
        event_id: eventId(),
        type,
        entry_id: activeEntryId,
        happened_at: new Date().toISOString(),
        ...details,
        nonce: Math.random()
      },
      { priority: "event" }
    );
  }

  function normalizeSelectionSurface(surface) {
    return surface === "orientation" ? "orientation" : "story_list";
  }

  function focusStory(entryId, { reveal = false } = {}) {
    function applyFocus(onlyIfLost = false) {
      if (onlyIfLost && !focusWasLost()) return;
      const selector = entryId
        ? `.story-card[data-entry-id="${CSS.escape(entryId)}"]`
        : ".story-card";
      const card =
        document.querySelector(selector) || document.querySelector(".story-card");
      if (!card) {
        const heading = document.querySelector(".story-pane .pane-header");
        if (!heading) return;
        heading.setAttribute("tabindex", "-1");
        heading.focus({ preventScroll: true });
        return;
      }

      if (reveal) card.scrollIntoView({ block: "nearest" });
      card.focus({ preventScroll: !reveal });
    }

    window.requestAnimationFrame(function () {
      applyFocus();
    });
    [150, 500].forEach(function (delay) {
      window.setTimeout(function () {
        applyFocus(true);
      }, delay);
    });
  }

  function focusOrientation() {
    function applyFocus(onlyIfLost = false) {
      if (onlyIfLost && !focusWasLost()) return;
      const orientation = document.getElementById("rill-orientation");
      if (!orientation) return;

      const target =
        orientation.querySelector("h1") ||
        orientation.querySelector(".orientation-browse") ||
        orientation;

      if (!target.matches("button, a, input, select, textarea, summary")) {
        target.setAttribute("tabindex", "-1");
      }
      target.focus({ preventScroll: true });
    }

    window.requestAnimationFrame(function () {
      applyFocus();
    });
    window.setTimeout(function () {
      applyFocus(true);
    }, 150);
  }

  function focusReader() {
    window.requestAnimationFrame(function () {
      const heading = document.querySelector(".article-header h1");
      const close = document.querySelector(".article-header .mobile-back");
      const target = desktopReaderMode.matches
        ? heading || close
        : close || heading;
      if (!target) return;

      if (target === heading) target.setAttribute("tabindex", "-1");
      target.focus({ preventScroll: true });
      pendingReaderFocus = false;
    });
  }

  function focusWasLost() {
    const active = document.activeElement;
    return Boolean(
      !active ||
        active === document.body ||
        active === document.documentElement ||
        active.closest("[inert]")
    );
  }

  function recoverReaderFocus() {
    window.setTimeout(function () {
      const documentNode = document.getElementById("reader-document");
      if (
        !documentNode ||
        documentNode.dataset.selectionSurface !== "orientation" ||
        (!pendingReaderFocus && !focusWasLost())
      ) {
        return;
      }

      pendingReaderFocus = true;
      focusReader();
    }, 150);
  }

  function deferOrientationReturnFocus(entryId) {
    pendingOrientationReturnFocus = entryId;
    window.setTimeout(function () {
      if (pendingOrientationReturnFocus !== entryId) return;
      pendingOrientationReturnFocus = null;
      if (document.getElementById("reader-document")) return;
      if (document.getElementById("rill-orientation")) {
        focusOrientation();
      } else {
        focusStory(entryId, {
          reveal: compactReaderMode.matches
        });
      }
    }, 300);
  }

  function restoreOrientationDismissFocus() {
    const pending = pendingOrientationDismissal;
    if (!pending) return;

    const dismissed = document.querySelector(
      `.orientation-step[data-card-id="${CSS.escape(pending.cardId)}"]`
    );
    if (dismissed) return;

    pendingOrientationDismissal = null;
    const nextCard = pending.fallbackCardIds
      .map(function (cardId) {
        return document.querySelector(
          `.orientation-step[data-card-id="${CSS.escape(cardId)}"]`
        );
      })
      .find(Boolean);
    const nextAction =
      nextCard &&
      (nextCard.querySelector(".orientation-read") ||
        nextCard.querySelector(".orientation-dismiss"));
    if (nextAction) {
      window.requestAnimationFrame(function () {
        nextAction.focus({ preventScroll: true });
      });
      return;
    }

    focusStory(null, {
      reveal: compactReaderMode.matches
    });
  }

  function focusOrientationDestinationSettings() {
    window.requestAnimationFrame(function () {
      const summary = document.querySelector(
        ".orientation-destination-settings summary"
      );
      if (summary) summary.focus({ preventScroll: true });
    });
  }

  function restoreOrientationModalFocus(event) {
    if (
      event.target.matches(
        "#shiny-modal[data-rill-orientation-confirmation]"
      )
    ) {
      orientationModalWasOpen = false;
      focusOrientationDestinationSettings();
    }
  }

  function syncOrientationModalFocus() {
    const modal = document.querySelector(
      "#shiny-modal[data-rill-orientation-confirmation]"
    );
    if (modal) {
      orientationModalWasOpen = true;
    } else if (orientationModalWasOpen) {
      orientationModalWasOpen = false;
      focusOrientationDestinationSettings();
    }
  }

  function ensureOrientationReturn(hasOrientation) {
    const controls = document.querySelector(".queue-controls");
    if (!controls) return;

    let button = controls.querySelector(".orientation-return");
    if (!button) {
      button = document.createElement("button");
      button.type = "button";
      button.className = "orientation-return";
      button.textContent = "Orientation";
      button.title = "Return to Orientation";
      button.setAttribute("aria-label", "Return to Orientation");
      button.addEventListener("click", function () {
        window.rillShowOrientation();
      });
      controls.prepend(button);
    }
    button.hidden = !hasOrientation;
  }

  const surfaceAccessibilityState = new WeakMap();

  function setSurfaceCovered(element, covered) {
    if (!element) return;

    if (covered) {
      if (!surfaceAccessibilityState.has(element)) {
        surfaceAccessibilityState.set(element, {
          inert: element.hasAttribute("inert"),
          ariaHidden: element.getAttribute("aria-hidden")
        });
      }
      element.setAttribute("inert", "");
      element.setAttribute("aria-hidden", "true");
      return;
    }

    const previous = surfaceAccessibilityState.get(element);
    if (!previous) return;
    element.toggleAttribute("inert", previous.inert);
    if (previous.ariaHidden === null) {
      element.removeAttribute("aria-hidden");
    } else {
      element.setAttribute("aria-hidden", previous.ariaHidden);
    }
    surfaceAccessibilityState.delete(element);
  }

  function setSidebarCovered(sidebar, covered) {
    const layout = sidebar && sidebar.parentElement;
    const elements = [
      sidebar,
      layout && layout.querySelector(":scope > .collapse-toggle"),
      layout && layout.querySelector(":scope > .bslib-sidebar-resize-handle")
    ].filter(Boolean);
    const containsFocus = elements.some(function (element) {
      return (
        element === document.activeElement ||
        element.contains(document.activeElement)
      );
    });
    elements.forEach(function (element) {
      setSurfaceCovered(element, covered);
    });
    return containsFocus;
  }

  function compactReaderAvailable(hasReader, hasOrientation) {
    return hasReader || hasOrientation;
  }

  function normalizedCompactSurface(surface, hasReader, hasOrientation) {
    if (surface === "library") return "library";
    if (
      surface === "reader" &&
      compactReaderAvailable(hasReader, hasOrientation)
    ) {
      return "reader";
    }
    return "queue";
  }

  function syncCompactSurfaceControls(shell, hasReader) {
    const libraryTrigger = document.querySelector(".compact-library-trigger");
    if (libraryTrigger) {
      libraryTrigger.setAttribute(
        "aria-expanded",
        String(shell.dataset.compactSurface === "library")
      );
    }

    document.querySelectorAll(".compact-reader-return").forEach(
      function (button) {
        button.hidden = !hasReader;
      }
    );
  }

  function focusLibrary() {
    window.requestAnimationFrame(function () {
      const heading = document.querySelector(".compact-library-header h1");
      if (!heading) return;
      heading.setAttribute("tabindex", "-1");
      heading.focus({ preventScroll: true });
    });
  }

  function focusCompactSurface(surface, hasReader) {
    if (surface === "library") {
      focusLibrary();
    } else if (surface === "reader") {
      if (hasReader) {
        focusReader();
      } else {
        focusOrientation();
      }
    } else {
      focusStory(activeEntryId, { reveal: false });
    }
  }

  function handleSkipLink(event) {
    const target = event.target;
    if (
      !(target instanceof Element) ||
      !target.closest(".rill-skip-link") ||
      !compactReaderMode.matches
    ) {
      return;
    }

    const shell = document.querySelector(".app-shell");
    if (!shell) return;
    event.preventDefault();
    const hasReader = Boolean(document.getElementById("reader-document"));
    const hasOrientation = Boolean(document.getElementById("rill-orientation"));
    const surface = normalizedCompactSurface(
      compactSurface || shell.dataset.compactSurface,
      hasReader,
      hasOrientation
    );
    focusCompactSurface(surface, hasReader);
  }

  function showCompactSurface(surface, options = {}) {
    if (!compactReaderMode.matches) return;

    const shell = document.querySelector(".app-shell");
    if (!shell) return;
    const hasReader = Boolean(document.getElementById("reader-document"));
    const hasOrientation = Boolean(document.getElementById("rill-orientation"));
    const nextSurface = normalizedCompactSurface(
      surface,
      hasReader,
      hasOrientation
    );
    if (options.remember === true && compactSurface !== nextSurface) {
      compactReturnSurface = compactSurface || "queue";
    }
    compactSurface = nextSurface;
    syncMobileSurfaces(shell, hasReader, hasOrientation);
    if (options.focus !== false) {
      focusCompactSurface(nextSurface, hasReader);
    }
  }

  function readingDwellSeconds(now = Date.now()) {
    const currentSegment = openedAt > 0 ? Math.max(0, now - openedAt) : 0;
    return Math.max(
      0,
      Math.round((accumulatedReadingMs + currentSegment) / 1000)
    );
  }

  function pauseReadingTelemetryClock(flushHeartbeat = false) {
    if (!activeEntryId || openedAt === 0) return;
    const now = Date.now();
    accumulatedReadingMs += Math.max(0, now - openedAt);
    openedAt = 0;
    if (flushHeartbeat) {
      const seconds = Math.max(
        0,
        Math.round((now - lastHeartbeatAt) / 1000)
      );
      if (seconds > 0) send("dwell_heartbeat", { dwell_seconds: seconds });
    }
    lastHeartbeatAt = now;
  }

  function resumeReadingTelemetryClock() {
    if (!activeEntryId || openedAt > 0) return;
    openedAt = Date.now();
    lastHeartbeatAt = openedAt;
  }

  function setReadingTelemetryPaused(paused) {
    const nextPaused = Boolean(paused);
    if (!readingTelemetryPaused && nextPaused) {
      pauseReadingTelemetryClock(true);
    }
    readingTelemetryPaused = nextPaused;
    if (!nextPaused && document.visibilityState === "visible") {
      resumeReadingTelemetryClock();
    }
  }

  function readingTelemetryActive() {
    return Boolean(
      activeEntryId &&
        !readingTelemetryPaused &&
        openedAt > 0 &&
        document.visibilityState === "visible"
    );
  }

  function restoreFocusFromCompactChrome(hasReader, hasOrientation) {
    const active = document.activeElement;
    if (
      !(active instanceof Element) ||
      !active.closest(
        ".compact-library-header, .compact-app-bar, .mobile-back"
      )
    ) {
      return;
    }
    if (hasReader) {
      focusReader();
    } else if (hasOrientation) {
      focusOrientation();
    } else {
      focusStory(activeEntryId, { reveal: false });
    }
  }

  function syncMobileSurfaces(shell, hasReader, hasOrientation) {
    if (!shell) return;

    const readerPane = document.querySelector(".reader-pane");
    const navigationSidebar = document.querySelector(".nav-sidebar");
    const storyPane = document.querySelector(".story-pane");
    if (!compactReaderMode.matches) {
      setReadingTelemetryPaused(false);
      delete shell.dataset.compactSurface;
      compactSurface = null;
      setSurfaceCovered(readerPane, false);
      setSidebarCovered(navigationSidebar, false);
      setSidebarCovered(storyPane, false);
      restoreFocusFromCompactChrome(hasReader, hasOrientation);
      return;
    }

    if (!compactSurface) {
      compactSurface = compactReaderAvailable(hasReader, hasOrientation)
        ? "reader"
        : "queue";
    }
    if (
      shell.classList.contains("orientation-queue-visible") &&
      !hasReader
    ) {
      compactSurface = "queue";
    } else if (
      hasOrientation &&
      !hasReader &&
      compactSurface !== "library"
    ) {
      compactSurface = "reader";
    }
    compactSurface = normalizedCompactSurface(
      compactSurface,
      hasReader,
      hasOrientation
    );
    shell.dataset.compactSurface = compactSurface;
    setReadingTelemetryPaused(compactSurface !== "reader");
    syncCompactSurfaceControls(shell, hasReader);

    const navigationHadFocus = setSidebarCovered(
      navigationSidebar,
      compactSurface !== "library"
    );
    const queueHadFocus = setSidebarCovered(
      storyPane,
      compactSurface !== "queue"
    );
    const readerHadFocus = Boolean(
      readerPane &&
        (readerPane === document.activeElement ||
          readerPane.contains(document.activeElement))
    );
    setSurfaceCovered(readerPane, compactSurface !== "reader");

    if (
      (navigationHadFocus || queueHadFocus || readerHadFocus) &&
      focusWasLost()
    ) {
      focusCompactSurface(compactSurface, hasReader);
    }
  }

  function syncReader() {
    const documentNode = document.getElementById("reader-document");
    const nextId = documentNode && documentNode.dataset.entryId;
    const nextDocumentId = documentNode && documentNode.dataset.documentId;
    const nextSurface =
      (documentNode && documentNode.dataset.selectionSurface) || "story_list";
    const orientation = document.getElementById("rill-orientation");
    const hasOrientation = Boolean(orientation);
    const hasReader = Boolean(nextId);
    const shell = document.querySelector(".app-shell");
    const selectionChanged = Boolean(
      nextId &&
        (nextId !== activeEntryId ||
          nextDocumentId !== activeDocumentId ||
          nextSurface !== activeEntrySurface)
    );
    localizeOrientationTimes();
    syncOrientationModalFocus();
    if (shell) {
      shell.classList.toggle("has-reader", hasReader);
      shell.classList.toggle("has-orientation", hasOrientation);
      if (!hasOrientation && !hasReader) {
        shell.classList.remove("orientation-queue-visible");
      }
    }
    if (compactReaderMode.matches && selectionChanged) {
      compactSurface = "reader";
    } else if (
      compactReaderMode.matches &&
      !nextId &&
      activeEntryId &&
      pendingEntrySurface === null
    ) {
      if (activeEntrySurface === "orientation" && hasOrientation) {
        if (shell) shell.classList.remove("orientation-queue-visible");
        compactSurface = "reader";
      } else {
        if (shell && hasOrientation) {
          shell.classList.add("orientation-queue-visible");
        }
        compactSurface = "queue";
      }
    }
    syncMobileSurfaces(shell, hasReader, hasOrientation);
    ensureOrientationReturn(hasOrientation);

    if (!nextId) {
      restoreOrientationDismissFocus();
      const previousId = activeEntryId;
      const previousSurface = activeEntrySurface;
      const changingSelection = pendingEntrySurface !== null;
      activeEntryId = null;
      activeDocumentId = null;
      activeEntrySurface = null;
      if (previousId && !changingSelection) {
        if (previousSurface === "orientation") {
          if (hasOrientation) {
            pendingOrientationReturnFocus = null;
            if (shell) shell.classList.remove("orientation-queue-visible");
            focusOrientation();
          } else {
            deferOrientationReturnFocus(previousId);
          }
        } else {
          focusStory(previousId);
        }
      }
      if (pendingOrientationReturnFocus !== null && hasOrientation) {
        pendingOrientationReturnFocus = null;
        if (shell) shell.classList.remove("orientation-queue-visible");
        focusOrientation();
      }
      if (
        hasOrientation &&
        shell &&
        shell.classList.contains("orientation-queue-visible") &&
        focusWasLost()
      ) {
        focusStory(null, {
          reveal: compactReaderMode.matches
        });
      }
      return;
    }
    if (
      nextId === activeEntryId &&
      nextDocumentId === activeDocumentId &&
      nextSurface === activeEntrySurface
    ) {
      const readerSurfaceVisible =
        !compactReaderMode.matches || compactSurface === "reader";
      if (
        readerSurfaceVisible &&
        (pendingReaderFocus ||
          (nextSurface === "orientation" && focusWasLost()))
      ) {
        pendingReaderFocus = true;
        focusReader();
      }
      return;
    }

    activeEntryId = nextId;
    activeDocumentId = nextDocumentId;
    activeEntrySurface = nextSurface;
    pendingReaderFocus =
      compactReaderMode.matches || activeEntrySurface === "orientation";
    pendingEntrySurface = null;
    pendingOrientationSelectionCardId = null;
    accumulatedReadingMs = 0;
    openedAt = Date.now();
    lastHeartbeatAt = openedAt;
    milestones = new Set();
    const scrollPane = document.querySelector(".reader-scroll");
    if (scrollPane) scrollPane.scrollTop = 0;
    send("article_impression", { scroll_percent: 0, dwell_seconds: 0 });
    if (pendingReaderFocus) {
      focusReader();
      recoverReaderFocus();
    }
  }

  function localizeOrientationTimes() {
    const formatter = new Intl.DateTimeFormat(undefined, {
      month: "short",
      day: "numeric",
      hour: "numeric",
      minute: "2-digit",
      timeZoneName: "short"
    });
    document.querySelectorAll(".orientation-evaluated time[datetime]").forEach(
      function (time) {
        const instant = new Date(time.dateTime);
        if (!Number.isNaN(instant.getTime())) {
          const localized = formatter.format(instant);
          if (time.textContent !== localized) time.textContent = localized;
        }
      }
    );
  }

  window.rillSelectEntry = function (
    id,
    position,
    surface = "story_list",
    provenance = null
  ) {
    if (!window.Shiny) return;
    pendingEntrySurface = normalizeSelectionSurface(surface);
    pendingOrientationSelectionCardId =
      pendingEntrySurface === "orientation" && provenance
        ? provenance.card_id || null
        : null;
    const payload = {
      id,
      position,
      surface: pendingEntrySurface,
      nonce: Math.random()
    };
    if (provenance) {
      [
        "document_id",
        "card_id",
        "revision_id",
        "basis_hash",
        "rationale_hash",
        "orientation_id"
      ].forEach(function (key) {
        if (Object.prototype.hasOwnProperty.call(provenance, key)) {
          payload[key] = provenance[key];
        }
      });
    }
    window.Shiny.setInputValue(
      "select_entry",
      payload,
      { priority: "event" }
    );
  };

  window.rillDismissOrientation = function (
    cardId,
    revisionId,
    rationaleHash
  ) {
    if (!window.Shiny || !cardId) return;
    const cards = Array.from(document.querySelectorAll(".orientation-step"));
    const dismissed = cards.find(function (card) {
      return card.dataset.cardId === cardId;
    });
    const index = cards.indexOf(dismissed);
    const fallbackCardIds = [];
    if (index >= 0 && cards[index + 1]) {
      fallbackCardIds.push(cards[index + 1].dataset.cardId);
    }
    if (index > 0) fallbackCardIds.push(cards[index - 1].dataset.cardId);
    const pending = { cardId, fallbackCardIds };
    pendingOrientationDismissal = pending;
    window.setTimeout(function () {
      if (pendingOrientationDismissal === pending) {
        pendingOrientationDismissal = null;
      }
    }, 5000);
    window.Shiny.setInputValue(
      "dismiss_orientation_card",
      {
        card_id: cardId,
        revision_id: revisionId,
        rationale_hash: rationaleHash,
        nonce: Math.random()
      },
      { priority: "event" }
    );
  };

  function revealOrientationQueue(_message) {
    const shell = document.querySelector(".app-shell");
    if (shell) {
      shell.classList.add("orientation-queue-visible");
      if (compactReaderMode.matches) compactSurface = "queue";
    }
    syncReader();
    const reveal = compactReaderMode.matches;
    focusStory(null, { reveal });
    window.setTimeout(function () {
      const currentShell = document.querySelector(".app-shell");
      if (
        currentShell &&
        currentShell.classList.contains("orientation-queue-visible") &&
        !document.getElementById("reader-document") &&
        focusWasLost()
      ) {
        focusStory(null, { reveal });
      }
    }, 150);
  }

  window.rillBrowseQueue = function () {
    if (!window.Shiny) return;
    window.Shiny.setInputValue(
      "browse_orientation_queue",
      { nonce: Math.random() },
      { priority: "event" }
    );
  };

  window.rillShowOrientation = function () {
    const shell = document.querySelector(".app-shell");
    if (shell) shell.classList.remove("orientation-queue-visible");
    if (compactReaderMode.matches) compactSurface = "reader";
    syncReader();
    focusOrientation();
  };

  window.rillOpenLibrary = function () {
    showCompactSurface("library", { remember: true });
  };

  window.rillCloseLibrary = function () {
    const destination = compactReturnSurface;
    compactReturnSurface = "queue";
    showCompactSurface(destination);
  };

  window.rillOpenQueue = function () {
    const shell = document.querySelector(".app-shell");
    if (shell && document.getElementById("rill-orientation")) {
      shell.classList.add("orientation-queue-visible");
    }
    showCompactSurface("queue", { remember: true });
  };

  window.rillReturnToReading = function () {
    const shell = document.querySelector(".app-shell");
    if (shell) shell.classList.remove("orientation-queue-visible");
    showCompactSurface("reader");
  };

  window.rillSelectFeed = function (id) {
    if (!window.Shiny) return;
    const shell = document.querySelector(".app-shell");
    if (shell && document.getElementById("rill-orientation")) {
      shell.classList.add("orientation-queue-visible");
    }
    showCompactSurface("queue", { focus: false });
    window.Shiny.setInputValue(
      "select_feed",
      { id, nonce: Math.random() },
      { priority: "event" }
    );
  };

  window.rillCloseReader = function () {
    if (!window.Shiny) return;
    pendingEntrySurface = null;
    window.Shiny.setInputValue(
      "close_reader",
      { nonce: Math.random() },
      { priority: "event" }
    );
  };

  window.rillTrack = function (type) {
    send(type, {
      dwell_seconds: readingDwellSeconds()
    });
  };

  function isEditableTarget(target) {
    if (!target || typeof target.closest !== "function") return false;
    return Boolean(
      target.isContentEditable ||
        target.closest("input, textarea, select, [contenteditable='true']")
    );
  }

  function moveStory(direction) {
    const shell = document.querySelector(".app-shell");
    if (
      compactReaderMode.matches &&
      shell &&
      shell.classList.contains("has-orientation") &&
      !shell.classList.contains("orientation-queue-visible") &&
      !activeEntryId
    ) {
      return false;
    }

    const cards = Array.from(document.querySelectorAll(".story-card"));
    if (!cards.length) return false;

    const selectedIndex = cards.findIndex(function (card) {
      return card.classList.contains("is-selected");
    });
    const nextIndex =
      selectedIndex < 0
        ? direction > 0
          ? 0
          : cards.length - 1
        : Math.max(0, Math.min(cards.length - 1, selectedIndex + direction));

    if (nextIndex === selectedIndex) return false;

    cards[nextIndex].scrollIntoView({ block: "nearest" });
    cards[nextIndex].click();
    return true;
  }

  function openOriginal() {
    const link = document.querySelector(".original-link");
    if (!link) return false;
    link.click();
    return true;
  }

  function toggleReaderAction(id) {
    const button = document.getElementById(id);
    if (!button) return false;
    button.click();
    return true;
  }

  function handleAskRillEscape(event) {
    if (
      event.defaultPrevented ||
      event.isComposing ||
      event.altKey ||
      event.ctrlKey ||
      event.metaKey ||
      event.shiftKey ||
      event.key.toLowerCase() !== "escape"
    ) {
      return;
    }

    const { layout } = readerAgentElements();
    if (!layout || layout.classList.contains("sidebar-collapsed")) return;
    setSidebarExpanded("reader_agent_sidebar", false);
    event.preventDefault();
  }

  function handleShortcut(event) {
    if (
      event.defaultPrevented ||
      event.isComposing ||
      event.altKey ||
      event.ctrlKey ||
      event.metaKey ||
      event.shiftKey
    ) {
      return;
    }

    const key = event.key.toLowerCase();
    if (isEditableTarget(event.target)) return;

    let handled = false;
    if (key === "j") handled = moveStory(1);
    if (key === "k") handled = moveStory(-1);
    if (key === "o") handled = openOriginal();
    if (key === "s") handled = toggleReaderAction("toggle_save");
    if (key === "f") handled = toggleReaderAction("toggle_star");
    if (key === "escape") {
      const shell = document.querySelector(".app-shell");
      if (
        compactReaderMode.matches &&
        compactSurface === "library"
      ) {
        window.rillCloseLibrary();
        handled = true;
      } else if (
        compactReaderMode.matches &&
        compactSurface === "queue" &&
        activeEntryId
      ) {
        window.rillReturnToReading();
        handled = true;
      } else if (
        compactReaderMode.matches &&
        compactSurface === "reader" &&
        activeEntryId
      ) {
        window.rillOpenQueue();
        handled = true;
      } else if (activeEntryId) {
        window.rillCloseReader();
        handled = true;
      } else if (
        shell &&
        shell.classList.contains("has-orientation") &&
        shell.classList.contains("orientation-queue-visible")
      ) {
        window.rillShowOrientation();
        handled = true;
      }
    }

    if (handled) event.preventDefault();
  }

  function watchScroll() {
    const scrollPane = document.querySelector(".reader-scroll");
    if (!scrollPane) return;
    scrollPane.addEventListener(
      "scroll",
      function () {
        if (!readingTelemetryActive()) return;
        const available = scrollPane.scrollHeight - scrollPane.clientHeight;
        if (available <= 0) return;
        const percent = Math.min(100, Math.round((scrollPane.scrollTop / available) * 100));
        [25, 50, 75, 100].forEach(function (milestone) {
          if (percent >= milestone && !milestones.has(milestone)) {
            milestones.add(milestone);
            send("scroll_milestone", {
              scroll_percent: milestone,
              dwell_seconds: readingDwellSeconds()
            });
          }
        });
      },
      { passive: true }
    );
  }

  function heartbeat() {
    if (!readingTelemetryActive()) return;
    const now = Date.now();
    const seconds = Math.round((now - lastHeartbeatAt) / 1000);
    lastHeartbeatAt = now;
    send("dwell_heartbeat", { dwell_seconds: seconds });
  }

  function registerFileReset() {
    if (fileResetRegistered || !window.Shiny) return;
    window.Shiny.addCustomMessageHandler("rill-reset-file", function (id) {
      const input = document.getElementById(id);
      if (!input) return;
      input.value = "";
      const container = input.closest(".shiny-input-container");
      const filename = container && container.querySelector("input[type='text']");
      if (filename) filename.value = "";
    });
    fileResetRegistered = true;
  }

  function registerQueueBrowse() {
    if (queueBrowseRegistered || !window.Shiny) return;
    window.Shiny.addCustomMessageHandler(
      "rill-browse-queue-ready",
      revealOrientationQueue
    );
    queueBrowseRegistered = true;
  }

  function registerOrientationActionMessages() {
    if (orientationActionMessagesRegistered || !window.Shiny) return;
    window.Shiny.addCustomMessageHandler(
      "rill-orientation-action-rejected",
      function (message) {
        const dismissal = pendingOrientationDismissal;
        const cardId =
          message.action === "dismiss"
            ? dismissal && dismissal.cardId
            : pendingOrientationSelectionCardId;
        pendingOrientationDismissal = null;
        pendingOrientationSelectionCardId = null;
        pendingEntrySurface = null;
        pendingReaderFocus = false;
        const card =
          cardId &&
          document.querySelector(
            `.orientation-step[data-card-id="${CSS.escape(cardId)}"]`
          );
        const action =
          card &&
          (card.querySelector(".orientation-read") ||
            card.querySelector(".orientation-dismiss"));
        if (action) {
          action.focus({ preventScroll: true });
        } else {
          focusOrientation();
        }
      }
    );
    window.Shiny.addCustomMessageHandler(
      "rill-selection-accepted",
      function (message) {
        pendingEntrySurface = null;
        pendingOrientationSelectionCardId = null;
        if (message.surface === "orientation") {
          pendingReaderFocus = true;
          focusReader();
          recoverReaderFocus();
        } else if (
          compactReaderMode.matches &&
          document.getElementById("reader-document")
        ) {
          showCompactSurface("reader");
        }
      }
    );
    window.Shiny.addCustomMessageHandler(
      "rill-focus-orientation-destination",
      function (_message) {
        focusOrientationDestinationSettings();
      }
    );
    orientationActionMessagesRegistered = true;
  }

  function registerChatSubmissionIds() {
    if (chatSubmissionRegistered || !window.jQuery || !window.Shiny) return;
    window.jQuery(document).on("shiny:inputchanged.rillChatSubmission", function (event) {
      if (
        event.name !== "reader_chat_user_input" ||
        !["", "shinychat.userInput"].includes(event.inputType)
      ) {
        return;
      }
      chatSubmissionIndex += 1;
      const sequence =
        event.value && event.value.seq != null
          ? event.value.seq
          : chatSubmissionIndex;
      window.Shiny.setInputValue(
        "reader_chat_submission_id",
        String(sequence),
        { priority: "event" }
      );
    });
    chatSubmissionRegistered = true;
  }

  function registerAppearanceControl() {
    const controls = document.querySelectorAll(
      'input[name="rill_theme_mode"]'
    );
    controls.forEach(function (control) {
      control.addEventListener("change", function () {
        if (control.checked) {
          applyThemeMode(control.value, { notify: true });
        }
      });
    });
    syncThemeControl(document.documentElement.dataset.rillThemeMode || "system");
  }

  function setSidebarExpanded(id, expanded) {
    const sidebar = document.getElementById(id);
    const layout = sidebar && sidebar.parentElement;
    const toggle = layout && layout.querySelector(":scope > .collapse-toggle");
    if (!layout || !toggle) return;
    const collapsed = layout.classList.contains("sidebar-collapsed");
    if (collapsed === expanded) toggle.click();
  }

  function readerAgentElements() {
    const sidebar = document.getElementById("reader_agent_sidebar");
    const layout = sidebar && sidebar.parentElement;
    const main = layout && layout.querySelector(":scope > .main");
    const toggle =
      layout && layout.querySelector(":scope > .collapse-toggle");
    return { sidebar, layout, main, toggle };
  }

  function focusAgentSidebar() {
    window.requestAnimationFrame(function () {
      const { sidebar } = readerAgentElements();
      const heading = sidebar && sidebar.querySelector("#reader-agent-title");
      if (!heading) return;
      heading.setAttribute("tabindex", "-1");
      heading.focus({ preventScroll: true });
    });
  }

  function restoreAgentFocus() {
    const target =
      agentReturnFocus && document.contains(agentReturnFocus)
        ? agentReturnFocus
        : document.querySelector(".article-header h1");
    agentReturnFocus = null;
    if (!target) return;
    if (!target.matches("button, a, input, select, textarea, summary")) {
      target.setAttribute("tabindex", "-1");
    }
    function applyFocus(onlyIfDisplaced = false) {
      const active = document.activeElement;
      if (
        onlyIfDisplaced &&
        active &&
        active !== document.body &&
        active !== document.documentElement &&
        !active.matches(".article-header h1, .collapse-toggle") &&
        !active.closest(".reader-agent-sidebar")
      ) {
        return;
      }
      target.focus({ preventScroll: true });
    }
    window.requestAnimationFrame(applyFocus);
    window.setTimeout(function () {
      applyFocus(true);
    }, 200);
  }

  function ensureAskRillSettled() {
    const { layout, toggle } = readerAgentElements();
    if (!layout || !toggle || !layout.classList.contains("transitioning")) {
      return;
    }
    const icon = toggle.querySelector(".collapse-icon");
    if (icon) icon.dispatchEvent(new Event("transitionend"));
  }

  function syncAskRillControls() {
    const { layout, main, toggle } = readerAgentElements();
    if (!layout || !toggle) return;
    const expanded = !layout.classList.contains("sidebar-collapsed");
    const label = expanded ? "Close Ask Rill" : "Open Ask Rill";
    toggle.setAttribute("aria-label", label);
    toggle.setAttribute("title", label);
    document.querySelectorAll(".reader-agent-trigger").forEach(
      function (trigger) {
        trigger.setAttribute("aria-expanded", String(expanded));
      }
    );
    const coversMain = expanded && overlaidAgentMode.matches;
    const mainHadFocus = Boolean(
      coversMain &&
        main &&
        (main === document.activeElement || main.contains(document.activeElement))
    );
    setSurfaceCovered(main, coversMain);
    if (mainHadFocus) agentFocusPending = true;
    if (agentSidebarExpanded && !expanded) restoreAgentFocus();
    if (
      expanded &&
      agentFocusPending &&
      !layout.classList.contains("transitioning")
    ) {
      agentFocusPending = false;
      focusAgentSidebar();
    }
    if (!expanded) agentFocusPending = false;
    agentSidebarExpanded = expanded;
  }

  function rememberAgentToggleFocus(event) {
    const { layout, toggle } = readerAgentElements();
    if (!layout || !toggle || !toggle.contains(event.target)) return;
    if (layout.classList.contains("sidebar-collapsed")) {
      agentReturnFocus = document.activeElement;
    }
    window.setTimeout(ensureAskRillSettled, 400);
  }

  function registerAgentSidebarControls() {
    const { layout } = readerAgentElements();
    if (!layout) return;
    if (agentSidebarObserver) agentSidebarObserver.disconnect();
    agentSidebarObserver = new MutationObserver(syncAskRillControls);
    agentSidebarObserver.observe(layout, {
      attributes: true,
      attributeFilter: ["class"]
    });
    syncAskRillControls();
  }

  window.rillOpenAskRill = function (trigger) {
    agentReturnFocus = trigger || document.activeElement;
    agentFocusPending = true;
    setSidebarExpanded("reader_agent_sidebar", true);
    window.setTimeout(syncAskRillControls, 0);
    window.setTimeout(ensureAskRillSettled, 400);
  };

  function syncResponsiveSidebarState() {
    if (mediumReaderMode.matches) {
      setSidebarExpanded("navigation_sidebar", false);
      setSidebarExpanded("story_sidebar", true);
      setSidebarExpanded("reader_agent_sidebar", false);
      return;
    }
    if (!wideReaderMode.matches) return;

    ["navigation_sidebar", "story_sidebar"].forEach(function (id) {
      setSidebarExpanded(id, true);
    });
  }

  function handleResponsiveLayoutChange() {
    window.setTimeout(syncResponsiveSidebarState, 0);
    syncReader();
    syncAskRillControls();
  }

  function handleAgentLayoutChange() {
    syncReader();
    syncAskRillControls();
  }

  function closeCompactLibraryAfterViewChange(event) {
    if (
      compactReaderMode.matches &&
      event.target.matches('input[name="view"]')
    ) {
      const shell = document.querySelector(".app-shell");
      if (shell && document.getElementById("rill-orientation")) {
        shell.classList.add("orientation-queue-visible");
      }
      showCompactSurface("queue", { focus: false });
    }
  }

  function initializeResponsiveSidebarState() {
    if (!mediumReaderMode.matches && !wideReaderMode.matches) return;

    window.requestAnimationFrame(function () {
      syncResponsiveSidebarState();
    });
    window.setTimeout(function () {
      syncResponsiveSidebarState();
    }, 250);
  }

  function watchSidebarInitialization() {
    const shell = document.querySelector(".app-shell");
    if (!shell) return;
    new MutationObserver(function (_records, observer) {
      const initialized = [
        "navigation_sidebar",
        "story_sidebar",
        "reader_agent_sidebar"
      ].every(function (id) {
        const sidebar = document.getElementById(id);
        const layout = sidebar && sidebar.parentElement;
        return layout && !layout.hasAttribute("data-bslib-sidebar-init");
      });
      if (!initialized) return;
      observer.disconnect();
      syncResponsiveSidebarState();
    }).observe(shell, { attributes: true, subtree: true });
  }

  function handleSystemThemeChange() {
    if (document.documentElement.dataset.rillThemeMode === "system") {
      applyThemeMode("system", { persist: false });
    }
  }

  if (typeof systemDarkMode.addEventListener === "function") {
    systemDarkMode.addEventListener("change", handleSystemThemeChange);
  } else {
    systemDarkMode.addListener(handleSystemThemeChange);
  }

  if (typeof desktopReaderMode.addEventListener === "function") {
    desktopReaderMode.addEventListener("change", handleResponsiveLayoutChange);
    mediumReaderMode.addEventListener("change", handleResponsiveLayoutChange);
    wideReaderMode.addEventListener("change", handleResponsiveLayoutChange);
    overlaidAgentMode.addEventListener("change", handleAgentLayoutChange);
  } else {
    desktopReaderMode.addListener(handleResponsiveLayoutChange);
    mediumReaderMode.addListener(handleResponsiveLayoutChange);
    wideReaderMode.addListener(handleResponsiveLayoutChange);
    overlaidAgentMode.addListener(handleAgentLayoutChange);
  }

  window.addEventListener("storage", function (event) {
    if (event.key === themeStorageKey) {
      applyThemeMode(event.newValue || "system", { persist: false });
    }
  });

  document.addEventListener("DOMContentLoaded", function () {
    registerAppearanceControl();
    initializeResponsiveSidebarState();
    watchSidebarInitialization();
    window.setTimeout(syncReader, 200);
    watchScroll();
    syncReader();
    registerFileReset();
    registerQueueBrowse();
    registerOrientationActionMessages();
    registerChatSubmissionIds();
    registerAgentSidebarControls();
    registerSystemEvents();
    registerInputValidity();
    enhanceNativeFeedback();
    new MutationObserver(syncReader).observe(document.body, {
      childList: true,
      subtree: true
    });
    new MutationObserver(function (records) {
      records.forEach(function (record) {
        if (record.type === "attributes") {
          enhanceNativeFeedback(record.target);
        }
        record.addedNodes.forEach(function (node) {
          if (node instanceof Element) enhanceNativeFeedback(node);
        });
      });
    }).observe(document.body, {
      attributes: true,
      attributeFilter: ["class"],
      childList: true,
      subtree: true
    });
    window.setInterval(heartbeat, 15000);
  });
  document.addEventListener("hide.bs.modal", restoreOrientationModalFocus);
  document.addEventListener("hidden.bs.modal", restoreOrientationModalFocus);
  document.addEventListener("shown.bs.popover", function (event) {
    const trigger = event.target;
    if (!(trigger instanceof Element)) return;
    if (!trigger.matches(".read-actions-trigger")) return;
    const popoverId = trigger.getAttribute("aria-describedby");
    if (!popoverId) return;
    trigger.setAttribute("aria-expanded", "true");
    trigger.setAttribute("aria-controls", popoverId);
    const popover = document.getElementById(popoverId);
    if (popover) {
      enhanceNativeFeedback(popover);
      readingStatusTrigger = trigger;
      window.requestAnimationFrame(function () {
        const action = popover.querySelector(".btn-read-action");
        if (action) action.focus({ preventScroll: true });
      });
    }
  });
  document.addEventListener("hidden.bs.popover", function (event) {
    const trigger = event.target;
    if (!(trigger instanceof Element)) return;
    if (!trigger.matches(".read-actions-trigger")) return;
    trigger.setAttribute("aria-expanded", "false");
    trigger.removeAttribute("aria-controls");
    if (readingStatusTrigger === trigger) {
      readingStatusTrigger = null;
      window.requestAnimationFrame(function () {
        trigger.focus({ preventScroll: true });
      });
    }
  });

  document.addEventListener("shiny:connected", function () {
    registerFileReset();
    registerQueueBrowse();
    registerOrientationActionMessages();
    registerChatSubmissionIds();
    registerAgentSidebarControls();
    registerInputValidity();
  });

  window.addEventListener("offline", function () {
    if (connectionState === "disconnected") handleShinyDisconnected();
  });
  window.addEventListener("online", function () {
    if (connectionState !== "offline") return;
    connectionState = "disconnected";
    setSystemStatus(
      "reconnecting",
      "Back online",
      "Reconnecting to your Library\u2026",
      { action: true }
    );
  });

  document.addEventListener("keydown", handleAskRillEscape, true);
  document.addEventListener("keydown", handleShortcut);
  document.addEventListener("click", handleSkipLink);
  document.addEventListener("change", closeCompactLibraryAfterViewChange);
  document.addEventListener("input", function (event) {
    if (
      event.target instanceof Element &&
      event.target.matches('[aria-invalid="true"]')
    ) {
      setInputValidity({ id: event.target.id, invalid: false });
    }
  });
  document.addEventListener("pointerdown", rememberAgentToggleFocus, true);
  document.addEventListener("keydown", function (event) {
    if (event.key === "Enter" || event.key === " ") {
      rememberAgentToggleFocus(event);
    }
  }, true);

  document.addEventListener("visibilitychange", function () {
    if (
      document.visibilityState === "hidden" &&
      activeEntryId &&
      !readingTelemetryPaused
    ) {
      pauseReadingTelemetryClock();
      send("page_hidden", {
        dwell_seconds: readingDwellSeconds()
      });
    } else if (
      document.visibilityState === "visible" &&
      activeEntryId &&
      !readingTelemetryPaused
    ) {
      resumeReadingTelemetryClock();
    }
  });
})();
