(function () {
  const themeStorageKey = "rill-theme-mode";
  const themeModes = new Set(["system", "light", "dark"]);
  const systemDarkMode = window.matchMedia("(prefers-color-scheme: dark)");

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
  let openedAt = 0;
  let lastHeartbeatAt = 0;
  let milestones = new Set();
  let fileResetRegistered = false;
  let chatSubmissionRegistered = false;
  let chatSubmissionIndex = 0;

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

  function focusStory(entryId) {
    window.requestAnimationFrame(function () {
      const card = document.querySelector(
        `.story-card[data-entry-id="${CSS.escape(entryId)}"]`
      );
      if (card) card.focus({ preventScroll: true });
    });
  }

  function syncReader() {
    const documentNode = document.getElementById("reader-document");
    const nextId = documentNode && documentNode.dataset.entryId;
    const shell = document.querySelector(".app-shell");
    if (shell) shell.classList.toggle("has-reader", Boolean(nextId));

    if (!nextId) {
      const previousId = activeEntryId;
      activeEntryId = null;
      if (previousId) focusStory(previousId);
      return;
    }
    if (nextId === activeEntryId) return;

    activeEntryId = nextId;
    openedAt = Date.now();
    lastHeartbeatAt = openedAt;
    milestones = new Set();
    const scrollPane = document.querySelector(".reader-scroll");
    if (scrollPane) scrollPane.scrollTop = 0;
    send("article_impression", { scroll_percent: 0, dwell_seconds: 0 });
  }

  window.rillSelectEntry = function (id, position) {
    window.Shiny.setInputValue(
      "select_entry",
      { id, position, nonce: Math.random() },
      { priority: "event" }
    );
  };

  window.rillSelectFeed = function (id) {
    window.Shiny.setInputValue(
      "select_feed",
      { id, nonce: Math.random() },
      { priority: "event" }
    );
  };

  window.rillCloseReader = function () {
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
    if (key === "escape" && activeEntryId) {
      window.rillCloseReader();
      handled = true;
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

  window.addEventListener("storage", function (event) {
    if (event.key === themeStorageKey) {
      applyThemeMode(event.newValue || "system", { persist: false });
    }
  });

  document.addEventListener("DOMContentLoaded", function () {
    registerAppearanceControl();
    window.setTimeout(initializeCompactSidebarState, 100);
    watchScroll();
    syncReader();
    registerFileReset();
    registerChatSubmissionIds();
    new MutationObserver(syncReader).observe(document.body, {
      childList: true,
      subtree: true
    });
    window.setInterval(heartbeat, 15000);
  });

  document.addEventListener("shiny:connected", function () {
    registerFileReset();
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
