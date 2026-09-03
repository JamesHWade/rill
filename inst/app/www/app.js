(function () {
  const themeStorageKey = "rill-theme-mode";
  const themeModes = new Set(["system", "light", "dark"]);
  const systemDarkMode = window.matchMedia("(prefers-color-scheme: dark)");
  const desktopReaderMode = window.matchMedia("(min-width: 576px)");

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
  let milestones = new Set();
  let fileResetRegistered = false;
  let chatSubmissionRegistered = false;
  let queueBrowseRegistered = false;
  let orientationActionMessagesRegistered = false;
  let chatSubmissionIndex = 0;
  let orientationModalWasOpen = false;

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
          reveal: window.matchMedia("(max-width: 900px)").matches
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
      reveal: window.matchMedia("(max-width: 900px)").matches
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

  function syncMobileSurfaces(shell, hasReader, hasOrientation) {
    if (!shell) return;

    const readerPane = document.querySelector(".reader-pane");
    const navigationSidebar = document.querySelector(".nav-sidebar");
    const storyPane = document.querySelector(".story-pane");
    const orientationVisible = Boolean(
      !desktopReaderMode.matches &&
        !hasReader &&
        hasOrientation &&
        !shell.classList.contains("orientation-queue-visible")
    );

    const readerPaneHidden = Boolean(
      !desktopReaderMode.matches && !hasReader && !orientationVisible
    );
    setSurfaceCovered(readerPane, readerPaneHidden);
    const navigationHadFocus = setSidebarCovered(
      navigationSidebar,
      orientationVisible
    );
    const queueHadFocus = setSidebarCovered(storyPane, orientationVisible);

    if (orientationVisible && (navigationHadFocus || queueHadFocus)) {
      focusOrientation();
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
    localizeOrientationTimes();
    syncOrientationModalFocus();
    if (shell) {
      shell.classList.toggle("has-reader", hasReader);
      shell.classList.toggle("has-orientation", hasOrientation);
      if (!hasOrientation && !hasReader) {
        shell.classList.remove("orientation-queue-visible");
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
          reveal: window.matchMedia("(max-width: 900px)").matches
        });
      }
      return;
    }
    if (
      nextId === activeEntryId &&
      nextDocumentId === activeDocumentId &&
      nextSurface === activeEntrySurface
    ) {
      if (
        pendingReaderFocus ||
        (nextSurface === "orientation" && focusWasLost())
      ) {
        pendingReaderFocus = true;
        focusReader();
      }
      return;
    }

    activeEntryId = nextId;
    activeDocumentId = nextDocumentId;
    activeEntrySurface = nextSurface;
    pendingReaderFocus = activeEntrySurface === "orientation";
    pendingEntrySurface = null;
    pendingOrientationSelectionCardId = null;
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
    if (shell) shell.classList.add("orientation-queue-visible");
    syncReader();
    const reveal = window.matchMedia("(max-width: 900px)").matches;
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
    syncReader();
    focusOrientation();
  };

  window.rillSelectFeed = function (id) {
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
      dwell_seconds: Math.max(0, Math.round((Date.now() - openedAt) / 1000))
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
      window.matchMedia("(max-width: 900px)").matches &&
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

  function handleShortcut(event) {
    if (
      event.defaultPrevented ||
      event.isComposing ||
      event.altKey ||
      event.ctrlKey ||
      event.metaKey ||
      event.shiftKey ||
      isEditableTarget(event.target)
    ) {
      return;
    }

    const key = event.key.toLowerCase();
    let handled = false;
    if (key === "j") handled = moveStory(1);
    if (key === "k") handled = moveStory(-1);
    if (key === "o") handled = openOriginal();
    if (key === "s") handled = toggleReaderAction("toggle_save");
    if (key === "f") handled = toggleReaderAction("toggle_star");
    if (key === "escape") {
      const shell = document.querySelector(".app-shell");
      if (activeEntryId) {
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
        if (!activeEntryId) return;
        const available = scrollPane.scrollHeight - scrollPane.clientHeight;
        if (available <= 0) return;
        const percent = Math.min(100, Math.round((scrollPane.scrollTop / available) * 100));
        [25, 50, 75, 100].forEach(function (milestone) {
          if (percent >= milestone && !milestones.has(milestone)) {
            milestones.add(milestone);
            send("scroll_milestone", {
              scroll_percent: milestone,
              dwell_seconds: Math.round((Date.now() - openedAt) / 1000)
            });
          }
        });
      },
      { passive: true }
    );
  }

  function heartbeat() {
    if (!activeEntryId || document.visibilityState !== "visible") return;
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

  function initializeCompactSidebarState() {
    const compactDesktop = window.matchMedia(
      "(min-width: 576px) and (max-width: 1050px)"
    );
    if (!compactDesktop.matches) return;

    ["navigation_sidebar", "reader_agent_sidebar"].forEach(function (id) {
      const sidebar = document.getElementById(id);
      const layout = sidebar && sidebar.parentElement;
      const toggle = layout && layout.querySelector(":scope > .collapse-toggle");
      if (layout && toggle && !layout.classList.contains("sidebar-collapsed")) {
        toggle.click();
      }
    });
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
    desktopReaderMode.addEventListener("change", syncReader);
  } else {
    desktopReaderMode.addListener(syncReader);
  }

  window.addEventListener("storage", function (event) {
    if (event.key === themeStorageKey) {
      applyThemeMode(event.newValue || "system", { persist: false });
    }
  });

  document.addEventListener("DOMContentLoaded", function () {
    registerAppearanceControl();
    window.setTimeout(initializeCompactSidebarState, 100);
    window.setTimeout(syncReader, 200);
    watchScroll();
    syncReader();
    registerFileReset();
    registerQueueBrowse();
    registerOrientationActionMessages();
    registerChatSubmissionIds();
    new MutationObserver(syncReader).observe(document.body, {
      childList: true,
      subtree: true
    });
    window.setInterval(heartbeat, 15000);
  });
  document.addEventListener("hide.bs.modal", restoreOrientationModalFocus);
  document.addEventListener("hidden.bs.modal", restoreOrientationModalFocus);

  document.addEventListener("shiny:connected", function () {
    registerFileReset();
    registerQueueBrowse();
    registerOrientationActionMessages();
    registerChatSubmissionIds();
  });

  document.addEventListener("keydown", handleShortcut);

  document.addEventListener("visibilitychange", function () {
    if (document.visibilityState === "hidden" && activeEntryId) {
      send("page_hidden", {
        dwell_seconds: Math.round((Date.now() - openedAt) / 1000)
      });
    } else if (document.visibilityState === "visible") {
      lastHeartbeatAt = Date.now();
    }
  });
})();
