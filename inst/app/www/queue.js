(function () {
  "use strict";

  const pending = new Map();
  let gesture = null;
  let suppressClickUntil = 0;
  let registered = false;
  let undoAction = null;
  let transition = null;
  let transitionTimer = null;
  let navigation = null;

  function clearTransition() {
    clearTimeout(transitionTimer);
    transition = null;
    const list = document.getElementById("story_list");
    list?.setAttribute("aria-busy", "false");
    list?.classList.remove("queue-changing");
  }

  function transitionId() {
    const bytes = new Uint8Array(16);
    try {
      window.crypto.getRandomValues(bytes);
    } catch {
      // Correlation only: this token never grants access or authority.
      bytes.forEach((_, index) => { bytes[index] = Math.floor(Math.random() * 256); });
    }
    return Array.from(bytes, byte => byte.toString(16).padStart(2, "0")).join("");
  }

  window.rillCancelQueueNavigation = function () {
    if (!navigation) return;
    navigation.observer.disconnect();
    clearTimeout(navigation.timer);
    navigation = null;
  };

  window.rillLoadNextStory = function (card) {
    const list = document.getElementById("story_list");
    const batch = list?.querySelector(".queue-batch");
    if (!batch?.querySelector("#queue_more") || !window.Shiny) return false;
    if (navigation) return true;
    const context = batch.dataset.queueContext;
    const id = card.dataset.entryId;
    const observer = new MutationObserver(() => {
      const current = list.querySelector(".queue-batch");
      const cards = Array.from(list.querySelectorAll(".story-card"));
      const index = cards.findIndex(item => item.dataset.entryId === id);
      if (current?.dataset.queueContext !== context || index < 0 ||
          !cards[index].classList.contains("is-selected")) {
        window.rillCancelQueueNavigation();
        return;
      }
      if (!cards[index + 1]) return;
      window.rillCancelQueueNavigation();
      cards[index + 1].scrollIntoView({block: "nearest"});
      cards[index + 1].click();
    });
    navigation = {observer, timer: setTimeout(window.rillCancelQueueNavigation, 15000)};
    observer.observe(list, {childList: true, subtree: true});
    window.Shiny.setInputValue("queue_more", Math.random(), {priority: "event"});
    return true;
  };

  function beginTransition(view) {
    if (!window.Shiny) return;
    clearTransition();
    window.rillCancelQueueNavigation();
    const id = transitionId();
    transition = {id, view, started: performance.now()};
    transitionTimer = setTimeout(clearTransition, 15000);
    const list = document.getElementById("story_list");
    list?.setAttribute("aria-busy", "true");
    list?.classList.add("queue-changing");
    window.Shiny.setInputValue("queue_view_request", {id, view}, {priority: "event"});
  }

  function acknowledgeTransition() {
    const batch = document.querySelector("#story_list .queue-batch");
    if (!transition || batch?.dataset.queueRequest !== transition.id ||
        batch.dataset.queueView !== transition.view) return;
    const completed = transition;
    const domReady = performance.now() - completed.started;
    window.requestAnimationFrame(() => window.requestAnimationFrame(() => {
      if (transition !== completed) return;
      clearTransition();
      const elapsed = performance.now() - completed.started;
      window.dispatchEvent(new CustomEvent("rill:queue-visible", {
        detail: {view: completed.view, elapsed_ms: elapsed, dom_ready_ms: domReady}
      }));
      if (document.querySelector(".app-shell")?.dataset.operationalTelemetry === "true") {
        window.Shiny.setInputValue("queue_view_visible", {
          id: completed.id, elapsed_ms: elapsed, dom_ready_ms: domReady
        }, {priority: "event"});
      }
    }));
  }

  function reconcileQueue(event) {
    if (event.name !== "story_list" || !event.value?.html || event.value.deps?.length) return;
    const list = document.getElementById("story_list");
    const current = list?.querySelector(":scope > .queue-batch");
    const template = document.createElement("template");
    template.innerHTML = event.value.html;
    const next = template.content.querySelector(".queue-batch");
    if (!current || !next || current.dataset.queueContext !== next.dataset.queueContext) return;
    event.preventDefault();
    const position = capturePosition();
    const active = document.activeElement;
    const focusRow = active?.closest(".story-row");
    const focusId = focusRow?.dataset.entryId;
    const focusMore = active?.id === "queue_more";
    const oldCount = current.querySelectorAll(".story-row").length;
    const focusClass = active?.classList.contains("story-save") ? ".story-save" :
      active?.classList.contains("story-read") ? ".story-read" : ".story-card";
    const existing = new Map(Array.from(current.querySelectorAll(":scope > .story-row"))
      .map(row => [row.dataset.entryId, row]));
    const children = Array.from(next.children, incoming => {
      const previous = existing.get(incoming.dataset.entryId);
      if (previous && previous.dataset.queueVersion === incoming.dataset.queueVersion) {
        previous.dataset.queueIndex = incoming.dataset.queueIndex;
        return previous;
      }
      return incoming;
    });
    // Move only changed or reordered nodes, preserving focus and decoded images.
    children.forEach((child, index) => {
      if (current.children[index] !== child) current.insertBefore(child, current.children[index] || null);
    });
    while (current.children.length > children.length) current.lastElementChild.remove();
    Object.assign(current.dataset, next.dataset);
    restorePosition(position);
    if (focusId && !active?.isConnected) rowFor(focusId)?.querySelector(focusClass)?.focus({preventScroll: true});
    if (focusMore && !active?.isConnected) {
      const target = current.querySelector("#queue_more") ||
        current.querySelectorAll(".story-card")[oldCount] ||
        current.querySelector(".story-card");
      target?.focus({preventScroll: true});
    }
    list.classList.remove("recalculating");
    acknowledgeTransition();
  }

  function queueRows() {
    return Array.from(document.querySelectorAll("#story_list .story-row"));
  }

  function rowFor(id) {
    return queueRows().find(row => row.dataset.entryId === id);
  }

  function capturePosition(excludeId) {
    const list = document.getElementById("story_list");
    if (!list) return null;
    const top = list.getBoundingClientRect().top;
    return {
      scrollTop: list.scrollTop,
      anchors: queueRows().filter(row =>
        row.dataset.entryId !== excludeId && row.getBoundingClientRect().bottom > top
      ).map(row => ({id: row.dataset.entryId, offset: row.getBoundingClientRect().top - top})),
    };
  }

  function restorePosition(position) {
    const list = document.getElementById("story_list");
    if (!list || !position) return;
    const anchor = position.anchors.find(item => rowFor(item.id));
    if (anchor) {
      const offset = rowFor(anchor.id).getBoundingClientRect().top - list.getBoundingClientRect().top;
      list.scrollTop += offset - anchor.offset;
    } else {
      list.scrollTop = position.scrollTop;
    }
  }

  function reveal(row, distance = 0, armed = false) {
    if (!row?.isConnected) return;
    row.style.setProperty("--swipe-distance", `${distance}px`);
    row.classList.toggle("is-swipe-armed", armed);
    const tray = row.querySelector(".story-swipe-tray");
    const open = distance < -40;
    tray.inert = !open;
    tray.setAttribute("aria-hidden", String(!open));
    tray.querySelector("button").tabIndex = open ? 0 : -1;
  }

  function closeSwipes(except) {
    queueRows().forEach(row => {
      if (row !== except && !row.classList.contains("is-pending")) reveal(row);
    });
  }

  function notice(message, undo = null, entryId = null, error = false) {
    const box = document.getElementById("queue-notice");
    if (!box) return;
    box.hidden = false;
    box.classList.toggle("is-error", error);
    document.getElementById("queue-notice-message").textContent = message;
    document.getElementById("queue-undo").hidden = !undo;
    undoAction = undo ? {id: undo, entryId} : null;
  }

  function connected() {
    return window.Shiny && window.rillUiAudit?.().connectionState === "connected";
  }

  function busyRow(row, busy) {
    if (!row) return;
    row.classList.toggle("is-pending", busy);
    row.setAttribute("aria-busy", String(busy));
    row.querySelectorAll("button").forEach(button => { button.disabled = busy; });
  }

  function submit(button) {
    const id = button.dataset.entryId;
    if (Array.from(pending.values()).some(item => item.entryId === id)) return;
    const row = rowFor(id);
    if (!connected()) {
      reveal(row);
      notice("Reconnect to Rill before changing this story.", null, null, true);
      return;
    }
    const requestId = window.crypto?.randomUUID?.() || `queue-${Date.now()}-${Math.random()}`;
    const request = {
      entryId: id,
      position: capturePosition(id),
      focus: row?.contains(document.activeElement),
      focusClass: button.classList.contains("story-save") ? ".story-save" : ".story-read",
    };
    pending.set(requestId, request);
    busyRow(row, true);
    window.Shiny.setInputValue("queue_action", {
      id: requestId, entry_id: id, action: button.dataset.queueAction,
    }, {priority: "event"});
  }

  function receive(result) {
    const request = pending.get(result.id);
    pending.delete(result.id);
    window.requestAnimationFrame(() => {
      const row = rowFor(result.entry_id || request?.entryId);
      busyRow(row, false);
      reveal(row);
      if (request) restorePosition(request.position);
      const focusLost = !document.activeElement || document.activeElement === document.body ||
        !document.activeElement.isConnected || document.activeElement.classList.contains("story-card");
      if (request?.focus && (focusLost || document.activeElement.id === "queue-undo")) {
        const target = row?.querySelector(request.focusClass || ".story-card") ||
          queueRows().find(item => item.getBoundingClientRect().bottom >
            document.getElementById("story_list").getBoundingClientRect().top)?.querySelector(".story-card") ||
          document.querySelector(".queue-filter");
        target?.focus({preventScroll: true});
      }
      notice(result.message, result.undo, result.entry_id, !result.ok);
    });
  }

  function initialize() {
    if (!registered && window.Shiny) {
      window.Shiny.addCustomMessageHandler("rill-queue-action-result", receive);
      registered = true;
    }
    syncNavigation();
  }

  window.rillQueueView = function (view) {
    const radio = Array.from(document.querySelectorAll('input[name="view"]'))
      .find(input => input.value === view);
    if (!radio) return;
    if (!radio.checked) radio.click();
    window.rillOpenQueue();
    syncNavigation();
  };

  function syncNavigation() {
    const view = document.querySelector('input[name="view"]:checked')?.value || "unread";
    document.querySelectorAll("[data-queue-nav]").forEach(button => {
      const active = button.dataset.queueNav === (view === "saved" ? "saved" : "queue");
      if (active) button.setAttribute("aria-current", "page");
      else button.removeAttribute("aria-current");
    });
  }

  document.addEventListener("click", event => {
    if (event.detail !== 0 && performance.now() < suppressClickUntil && event.target.closest(".story-row")) {
      event.preventDefault();
      event.stopImmediatePropagation();
      return;
    }
    if (event.target.closest(".story-card, .queue-filter, [data-queue-nav]")) {
      window.rillCancelQueueNavigation();
    }
    if (event.target.matches('input[name="view"]')) window.rillOpenQueue();
    if (event.target.closest("#queue_more")) {
      window.Shiny?.setInputValue("queue_more", Math.random(), {priority: "event"});
      return;
    }
    const button = event.target.closest("button[data-queue-action]");
    if (button) {
      event.preventDefault();
      submit(button);
    } else if (event.target.closest("#queue-undo") && undoAction) {
      if (!connected()) {
        notice("Reconnect to Rill before undoing this action.", undoAction.id, undoAction.entryId, true);
        return;
      }
      if (pending.has(undoAction.id)) return;
      pending.set(undoAction.id, {
        entryId: undoAction.entryId, position: capturePosition(undoAction.entryId),
        focus: true, focusClass: ".story-card",
      });
      window.Shiny.setInputValue("queue_undo", {id: undoAction.id, nonce: Math.random()}, {priority: "event"});
    } else if (event.target.closest("#queue-notice-dismiss")) {
      document.getElementById("queue-notice").hidden = true;
      undoAction = null;
    } else {
      closeSwipes();
    }
  }, true);

  document.addEventListener("keydown", event => {
    if (event.key === "Escape" && event.target.closest(".story-row")) {
      const row = event.target.closest(".story-row");
      if (parseFloat(row.style.getPropertyValue("--swipe-distance")) < 0) {
        reveal(row);
        row.querySelector(".story-card").focus({preventScroll: true});
        event.preventDefault();
        event.stopImmediatePropagation();
      }
    }
  }, true);

  document.addEventListener("pointerdown", event => {
    suppressClickUntil = 0;
    if (gesture) {
      cancelGesture();
      return;
    }
    const row = event.target.closest(".story-row");
    if (!row || event.pointerType === "mouse" || !event.isPrimary ||
        row.classList.contains("is-pending") || row.classList.contains("is-read") || document.querySelector(".modal.show") ||
        window.getSelection()?.toString() ||
        event.target.closest("a, input, textarea, select, [contenteditable], .story-actions, .story-swipe-tray") ||
        event.clientX < 24 || event.clientX > window.innerWidth - 24) return;
    closeSwipes(row);
    gesture = {
      row, id: event.pointerId, x: event.clientX, y: event.clientY,
      start: parseFloat(row.style.getPropertyValue("--swipe-distance")) || 0,
      distance: 0, horizontal: false,
      threshold: Math.min(72, Math.max(48, row.getBoundingClientRect().width * 0.2)),
    };
  });

  document.addEventListener("pointermove", event => {
    if (!gesture || event.pointerId !== gesture.id) return;
    const dx = event.clientX - gesture.x;
    const dy = event.clientY - gesture.y;
    if (!gesture.horizontal) {
      if (Math.abs(dy) > 12 && Math.abs(dy) > Math.abs(dx)) {
        cancelGesture();
        return;
      }
      if (Math.abs(dx) < 12 || Math.abs(dx) < Math.abs(dy) * 1.5) return;
      if (window.getSelection()?.toString()) { cancelGesture(); return; }
      gesture.horizontal = true;
      gesture.row.classList.add("is-swiping");
      gesture.row.setPointerCapture(event.pointerId);
    }
    gesture.distance = Math.min(0, Math.max(-gesture.threshold - 36, gesture.start + dx));
    reveal(gesture.row, gesture.distance, -gesture.distance >= gesture.threshold);
    if (event.cancelable) event.preventDefault();
  }, {passive: false});

  document.addEventListener("pointerup", event => {
    if (!gesture || event.pointerId !== gesture.id) return;
    const finished = gesture;
    gesture = null;
    finished.row.classList.remove("is-swiping");
    if (!finished.horizontal) return;
    suppressClickUntil = performance.now() + 350;
    if (-finished.distance >= finished.threshold) {
      submit(finished.row.querySelector(".story-swipe-button"));
    } else {
      reveal(finished.row);
    }
  });

  function cancelGesture() {
    if (!gesture) return;
    gesture.row.classList.remove("is-swiping");
    reveal(gesture.row);
    gesture = null;
  }

  document.addEventListener("pointercancel", cancelGesture);
  document.addEventListener("touchstart", event => {
    if (event.touches.length > 1) cancelGesture();
  }, {passive: true});
  document.addEventListener("error", event => {
    if (event.target.matches?.(".story-preview-image")) event.target.hidden = true;
  }, true);
  document.addEventListener("change", event => {
    if (event.target.matches('input[name="view"]')) {
      beginTransition(event.target.value);
      window.rillOpenQueue();
      syncNavigation();
    }
  });
  document.addEventListener("DOMContentLoaded", () => {
    initialize();
    const list = document.getElementById("story_list");
    if (list) new MutationObserver(acknowledgeTransition).observe(list, {childList: true, subtree: true});
    if (window.jQuery) {
      window.jQuery(document).on("shiny:connected.rillQueue", initialize);
      window.jQuery(document).on("shiny:value.rillQueue", reconcileQueue);
      window.jQuery(document).on("shiny:disconnected.rillQueue", () => {
        cancelGesture();
        clearTransition();
        window.rillCancelQueueNavigation();
        if (pending.size) {
          pending.forEach(request => busyRow(rowFor(request.entryId), false));
          pending.clear();
          notice("Connection lost. Check the story's status after reconnecting.", null, null, true);
        }
      });
    }
  });
})();
