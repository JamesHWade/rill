# Four agent interaction models on Rill's existing reader route, switchable
# via ?variant=. This remains standalone prototype code.

prototype_root <- function() {
  root <- getOption("rill.prototype.root")
  if (is.null(root)) {
    cli::cli_abort("The prototype source directory has not been configured.")
  }
  root
}

prototype_file <- function(...) {
  file.path(prototype_root(), ...)
}

prototype_data_destination <- function() {
  destination <- getOption("rill.prototype.data_destination")
  if (is.null(destination)) {
    cli::cli_abort("The prototype Data Destination has not been configured.")
  }
  destination
}

agent_interaction_prototype_head <- function() {
  shiny::tagList(
    shiny::tags$link(
      rel = "icon",
      type = "image/png",
      href = "rill-assets/rill-duck.png"
    ),
    shiny::includeCSS(
      prototype_file("www", "prototype-agent-interaction.css")
    ),
    shiny::includeScript(
      prototype_file("www", "prototype-agent-interaction.js")
    )
  )
}

agent_interaction_prototype_queue <- function() {
  shiny::tags$section(
    class = "prototype-orientation prototype-variant-a",
    `aria-labelledby` = "prototype-orientation-title",
    shiny::tags$header(
      class = "prototype-section-heading",
      shiny::tags$div(
        shiny::tags$p(class = "prototype-kicker", "Orientation"),
        shiny::tags$h2(
          id = "prototype-orientation-title",
          "Two things worth your attention"
        )
      ),
      shiny::tags$span(class = "prototype-maintained", "Maintained \u00b7 2")
    ),
    prototype_orientation_card(
      entry_id = "sample-entry-1",
      position = 1L,
      kind = "A boundary worth inspecting",
      title = "Keep the source, reading copy, and interaction record apart",
      body = paste(
        "The story makes the separation explicit and turns it into a",
        "reader-facing design constraint."
      ),
      why = "Shown because \u00b7 it bears directly on Rill's source-first model",
      evidence = paste(
        "Rill keeps the source feed, a cleaned reading copy, and your",
        "interactions as separate records."
      )
    ),
    prototype_orientation_card(
      entry_id = "sample-entry-5",
      position = 5L,
      kind = "A practical connection",
      title = "Deployment changes the shape of a personal reader",
      body = paste(
        "The app/runtime split is concrete enough to compare with the",
        "durability decisions now in view."
      ),
      why = "My read \u00b7 compare this with the current Hosted Rill plan",
      evidence = paste(
        "Neon provides durable Postgres while Connect Cloud runs the",
        "application."
      )
    )
  )
}

prototype_orientation_card <- function(
  entry_id,
  position,
  kind,
  title,
  body,
  why,
  evidence
) {
  shiny::tags$article(
    class = "prototype-orientation-card",
    shiny::tags$p(class = "prototype-card-kind", kind),
    shiny::tags$h3(title),
    shiny::tags$p(class = "prototype-card-body", body),
    shiny::tags$div(
      class = "prototype-card-footer",
      shiny::tags$span(why),
      shiny::tags$button(
        type = "button",
        class = "prototype-citation",
        `data-prototype-action` = "show-evidence",
        `data-prototype-evidence` = evidence,
        `aria-label` = "Inspect Source Evidence",
        "[1]"
      )
    ),
    shiny::tags$button(
      type = "button",
      class = "prototype-open-story",
      `data-prototype-action` = "select-story",
      `data-entry-id` = entry_id,
      `data-entry-position` = position,
      "Read story",
      bsicons::bs_icon("arrow-right")
    )
  )
}

prototype_recommended_orientation <- function() {
  shiny::tags$section(
    class = "prototype-orientation-canvas prototype-variant-d",
    `aria-labelledby` = "prototype-recommended-orientation-title",
    shiny::tags$header(
      class = "prototype-orientation-canvas-header",
      shiny::tags$div(
        shiny::tags$p(class = "prototype-kicker", "Orientation"),
        shiny::tags$h1(
          id = "prototype-recommended-orientation-title",
          "One question is worth carrying into today's reading"
        ),
        shiny::tags$p(
          class = "prototype-orientation-thesis",
          "What must stay separate as Rill becomes agent-native?"
        )
      ),
      shiny::tags$div(
        class = "prototype-orientation-maintenance",
        shiny::tags$span("Maintained from the current unread Documents"),
        shiny::tags$small(
          "Reevaluates only when source evidence or reading state changes."
        ),
        shiny::tags$button(
          type = "button",
          class = "prototype-orientation-browse",
          `data-prototype-action` = "dismiss-orientation",
          bsicons::bs_icon("list-ul"),
          "Browse the full unread queue"
        )
      )
    ),
    shiny::tags$p(
      class = "prototype-orientation-intro",
      "Two unread stories approach that decision from different sides. Read ",
      "them as a short path rather than two independent recommendations."
    ),
    shiny::tags$div(
      class = "prototype-orientation-path",
      prototype_recommended_orientation_step(
        number = "01",
        step = "Establish the boundary",
        source = "The R Blog",
        source_title = "A calmer way to keep up with R",
        age = "15m",
        interpretation = paste(
          "Start with the Document that makes source material, its reading",
          "copy, and the interaction record visibly distinct."
        ),
        why = paste(
          "Why now \u00b7 this is the clearest test of Rill's source-first",
          "product model."
        ),
        evidence = paste(
          "Rill keeps the source feed, a cleaned reading copy, and your",
          "interactions as separate records."
        ),
        entry_id = "sample-entry-1",
        position = 1L,
        button_label = "Read the anchor Document"
      ),
      shiny::tags$div(
        class = "prototype-orientation-connector",
        shiny::tags$span("Then test the same boundary in operation"),
        bsicons::bs_icon("arrow-down")
      ),
      prototype_recommended_orientation_step(
        number = "02",
        step = "Stress-test it in deployment",
        source = "Posit Blog",
        source_title = "Deploying a small stateful application",
        age = "2d",
        interpretation = paste(
          "Compare the conceptual boundary with a deployment story that",
          "forces an explicit decision about what becomes durable."
        ),
        why = paste(
          "Why now \u00b7 it turns the same boundary into an operational",
          "choice for Hosted Rill."
        ),
        evidence = paste(
          "Neon provides durable Postgres while Connect Cloud runs the",
          "application."
        ),
        entry_id = "sample-entry-5",
        position = 5L,
        button_label = "Read the contrasting Document"
      )
    )
  )
}

prototype_recommended_orientation_step <- function(
  number,
  step,
  source,
  source_title,
  age,
  interpretation,
  why,
  evidence,
  entry_id,
  position,
  button_label
) {
  shiny::tags$article(
    class = "prototype-orientation-step",
    shiny::tags$div(class = "prototype-orientation-step-number", number),
    shiny::tags$div(
      class = "prototype-orientation-step-body",
      shiny::tags$p(class = "prototype-card-kind", step),
      shiny::tags$div(
        class = "prototype-orientation-source",
        shiny::tags$span(source),
        shiny::tags$strong(source_title),
        shiny::tags$span(age)
      ),
      shiny::tags$p(
        class = "prototype-orientation-interpretation",
        interpretation
      ),
      shiny::tags$div(
        class = "prototype-orientation-reason",
        shiny::tags$span(why),
        shiny::tags$button(
          type = "button",
          class = "prototype-citation prototype-citation-text",
          `data-prototype-action` = "show-evidence",
          `data-prototype-evidence` = evidence,
          paste0("Inspect Source Evidence [", number, "]")
        )
      ),
      shiny::tags$button(
        type = "button",
        class = "prototype-orientation-read",
        `data-prototype-action` = "select-story",
        `data-entry-id` = entry_id,
        `data-entry-position` = position,
        button_label,
        bsicons::bs_icon("arrow-right")
      )
    )
  )
}

agent_interaction_prototype_reader <- function() {
  shiny::tagList(
    prototype_recommended_orientation(),
    shiny::tags$section(
      class = paste(
        "prototype-reader-seam",
        "prototype-variant-a",
        "prototype-variant-d"
      ),
      `aria-label` = "Agent interaction",
      shiny::tags$div(
        class = "prototype-connection-cue",
        shiny::tags$div(
          shiny::tags$p(
            class = "prototype-kicker",
            "Connection cue \u00b7 My read"
          ),
          shiny::tags$p(
            "This story's boundary between source, reading copy, and interaction ",
            "record connects directly to the product model you are shaping. ",
            shiny::tags$button(
              type = "button",
              class = "prototype-citation",
              `data-prototype-action` = "show-evidence",
              `data-prototype-evidence` = paste(
                "Rill keeps the source feed, a cleaned reading copy, and your",
                "interactions as separate records."
              ),
              `aria-label` = "Inspect Source Evidence",
              "[1]"
            )
          )
        ),
        shiny::tags$div(
          class = "prototype-cue-actions",
          prototype_action_button("Ask about this", "chat-left-text", "open-a"),
          prototype_action_button("Connect", "diagram-3", "open-a"),
          prototype_action_button("Carry forward", "arrow-up-right", "carry-a")
        )
      ),
      shiny::tags$div(
        class = "prototype-conversation-dock",
        hidden = NA,
        shiny::tags$header(
          shiny::tags$div(
            shiny::tags$p(class = "prototype-kicker", "Conversation"),
            shiny::tags$strong(
              "About ",
              shiny::tags$span(
                `data-agent-story-title` = NA,
                "the current story"
              )
            )
          ),
          prototype_icon_button("x-lg", "Close Conversation", "close-a")
        ),
        shiny::tags$div(
          class = "prototype-conversation-context",
          shiny::tags$span("Research Scope \u00b7 this Document"),
          shiny::tags$span(
            paste("Data Destination \u00b7", prototype_data_destination())
          )
        ),
        shiny::tags$div(
          class = "prototype-suggested-prompts",
          shiny::tags$button(
            type = "button",
            `data-prototype-action` = "prototype-feedback",
            "What breaks if these records collapse?"
          ),
          shiny::tags$button(
            type = "button",
            `data-prototype-action` = "prototype-feedback",
            "Connect this to earlier reading"
          )
        ),
        prototype_reading_artifact_proposal(
          class = "prototype-a-proposal",
          dismiss_action = "dismiss-proposal-a"
        ),
        prototype_composer("Ask about this story\u2026"),
        shiny::tags$p(
          class = "prototype-feedback",
          `aria-live` = "polite"
        )
      )
    )
  )
}

prototype_action_button <- function(label, icon, action) {
  shiny::tags$button(
    type = "button",
    class = "prototype-action-button",
    `data-prototype-action` = action,
    bsicons::bs_icon(icon),
    label
  )
}

prototype_reading_artifact_proposal <- function(class, dismiss_action) {
  shiny::tags$div(
    class = paste("prototype-action-proposal", class),
    hidden = NA,
    shiny::tags$p(class = "prototype-kicker", "Action Proposal"),
    shiny::tags$strong("Create an open-question Reading Artifact"),
    shiny::tags$p(
      "What boundaries must survive when Rill adds an agent runtime?"
    ),
    shiny::tags$div(
      shiny::tags$button(
        type = "button",
        `data-prototype-action` = "prototype-feedback",
        "Apply"
      ),
      shiny::tags$button(
        type = "button",
        `data-prototype-action` = dismiss_action,
        "Dismiss"
      )
    )
  )
}

prototype_icon_button <- function(icon, label, action) {
  shiny::tags$button(
    type = "button",
    class = "prototype-icon-button",
    `data-prototype-action` = action,
    `aria-label` = label,
    title = label,
    bsicons::bs_icon(icon)
  )
}

prototype_composer <- function(placeholder) {
  shiny::tags$form(
    class = "prototype-composer",
    `data-prototype-action` = "prototype-submit",
    shiny::tags$label(
      class = "prototype-composer-field",
      shiny::tags$span(class = "visually-hidden", "Message"),
      shiny::tags$textarea(
        rows = 2L,
        placeholder = placeholder,
        `aria-label` = "Message"
      )
    ),
    shiny::tags$button(
      type = "submit",
      `aria-label` = "Send message",
      bsicons::bs_icon("arrow-up")
    )
  )
}

agent_interaction_prototype_shell <- function() {
  shiny::tagList(
    prototype_companion_rail(),
    prototype_context_palette(),
    prototype_recommended_interaction(),
    prototype_evidence_panel(),
    prototype_switcher()
  )
}

prototype_companion_rail <- function() {
  shiny::tagList(
    shiny::tags$button(
      type = "button",
      class = "prototype-b-opener prototype-variant-b",
      `data-prototype-action` = "open-b",
      bsicons::bs_icon("chat-square-text"),
      "Companion"
    ),
    shiny::tags$aside(
      class = "prototype-companion-rail prototype-variant-b",
      `aria-label` = "Reading companion",
      shiny::tags$header(
        shiny::tags$div(
          shiny::tags$p(class = "prototype-kicker", "Rill agent"),
          shiny::tags$h2("Reading companion")
        ),
        prototype_icon_button("x-lg", "Close companion", "close-b")
      ),
      shiny::tags$div(
        class = "prototype-rail-queue",
        shiny::tags$p(
          class = "prototype-rail-state",
          "Orientation \u00b7 2 selections"
        ),
        shiny::tags$h3("Start with the source boundaries"),
        shiny::tags$p(
          "The clearest thread in today's queue is how reading copies, state, ",
          "and runtime durability remain separate."
        ),
        shiny::tags$button(
          type = "button",
          class = "prototype-rail-story",
          `data-prototype-action` = "select-story",
          `data-entry-id` = "sample-entry-1",
          `data-entry-position` = 1L,
          "Open the anchor story",
          bsicons::bs_icon("arrow-right")
        ),
        shiny::tags$hr(),
        shiny::tags$p(class = "prototype-kicker", "Then compare"),
        shiny::tags$p(
          "The deployment story turns that boundary into an operational ",
          "choice."
        )
      ),
      shiny::tags$div(
        class = "prototype-rail-reader",
        shiny::tags$div(
          class = "prototype-rail-context",
          shiny::tags$span("Current Document"),
          shiny::tags$strong(`data-agent-story-title` = NA, "Current story")
        ),
        shiny::tags$div(
          class = "prototype-message prototype-message-agent",
          shiny::tags$p(class = "prototype-message-label", "My read"),
          shiny::tags$p(
            "The source argues for separating the material being read from ",
            "the record of reading it."
          ),
          shiny::tags$button(
            type = "button",
            class = "prototype-citation prototype-citation-text",
            `data-prototype-action` = "show-evidence",
            `data-prototype-evidence` = paste(
              "Rill keeps the source feed, a cleaned reading copy, and your",
              "interactions as separate records."
            ),
            "Inspect Source Evidence [1]"
          )
        ),
        shiny::tags$div(
          class = "prototype-rail-actions",
          prototype_action_button("Explain", "lightbulb", "prototype-feedback"),
          prototype_action_button("Connect", "diagram-3", "prototype-feedback"),
          prototype_action_button(
            "Carry forward",
            "arrow-up-right",
            "proposal-b"
          )
        ),
        prototype_reading_artifact_proposal(
          class = "prototype-b-proposal",
          dismiss_action = "dismiss-proposal-b"
        ),
        prototype_composer("Ask about this Document\u2026")
      ),
      shiny::tags$p(
        class = "prototype-feedback",
        `aria-live` = "polite"
      )
    )
  )
}

prototype_recommended_interaction <- function() {
  shiny::tagList(
    shiny::tags$button(
      type = "button",
      class = "prototype-d-opener prototype-variant-d",
      `data-prototype-action` = "open-d",
      bsicons::bs_icon("chat-left-text"),
      "Ask Rill about this"
    ),
    shiny::tags$aside(
      class = "prototype-recommended-rail prototype-variant-d",
      `aria-label` = "Document Conversation",
      shiny::tags$header(
        shiny::tags$div(
          shiny::tags$p(class = "prototype-kicker", "Conversation"),
          shiny::tags$h2("Read with Rill")
        ),
        prototype_icon_button("x-lg", "Close Conversation", "close-d")
      ),
      shiny::tags$div(
        class = "prototype-recommended-context",
        shiny::tags$span("Current Document"),
        shiny::tags$strong(`data-agent-story-title` = NA, "Current story")
      ),
      shiny::tags$div(
        class = "prototype-conversation-context",
        shiny::tags$span("Research Scope \u00b7 this Document"),
        shiny::tags$span(
          paste("Data Destination \u00b7", prototype_data_destination())
        )
      ),
      shiny::tags$section(
        class = "prototype-recommended-cue",
        shiny::tags$p(
          class = "prototype-message-label",
          "Connection cue \u00b7 My read"
        ),
        shiny::tags$p(
          "The source's separation of material, reading copy, and interaction ",
          "record is the design boundary worth testing here."
        ),
        shiny::tags$button(
          type = "button",
          class = "prototype-citation prototype-citation-text",
          `data-prototype-action` = "show-evidence",
          `data-prototype-evidence` = paste(
            "Rill keeps the source feed, a cleaned reading copy, and your",
            "interactions as separate records."
          ),
          "Inspect Source Evidence [1]"
        )
      ),
      shiny::tags$div(
        class = "prototype-recommended-prompts",
        shiny::tags$p(class = "prototype-kicker", "Start from the Document"),
        shiny::tags$button(
          type = "button",
          `data-prototype-action` = "prototype-feedback",
          "What would break if these records collapsed?"
        ),
        shiny::tags$button(
          type = "button",
          `data-prototype-action` = "prototype-feedback",
          "Connect this with earlier reading"
        )
      ),
      shiny::tags$div(
        class = "prototype-recommended-actions",
        prototype_action_button(
          "Research public web",
          "globe2",
          "prototype-feedback"
        ),
        prototype_action_button(
          "Carry forward",
          "arrow-up-right",
          "proposal-d"
        )
      ),
      prototype_reading_artifact_proposal(
        class = "prototype-d-proposal",
        dismiss_action = "dismiss-proposal-d"
      ),
      prototype_composer("Ask about this Document\u2026"),
      shiny::tags$p(
        class = "prototype-feedback",
        `aria-live` = "polite"
      )
    )
  )
}

prototype_context_palette <- function() {
  shiny::tagList(
    shiny::tags$button(
      type = "button",
      class = "prototype-c-opener prototype-variant-c",
      `data-prototype-action` = "open-c",
      bsicons::bs_icon("stars"),
      shiny::tags$span(class = "prototype-c-opener-long", "Rill agent"),
      shiny::tags$span(class = "prototype-c-opener-short", "Agent")
    ),
    shiny::tags$section(
      class = "prototype-context-palette prototype-variant-c",
      hidden = NA,
      role = "dialog",
      `aria-labelledby` = "prototype-palette-title",
      shiny::tags$header(
        shiny::tags$div(
          shiny::tags$p(class = "prototype-kicker", "Current reading context"),
          shiny::tags$h2(id = "prototype-palette-title", "What would help?")
        ),
        prototype_icon_button("x-lg", "Close agent palette", "close-c")
      ),
      shiny::tags$div(
        class = "prototype-palette-queue",
        shiny::tags$p(
          class = "prototype-palette-intro",
          "Orientation is ready without replacing your queue."
        ),
        shiny::tags$button(
          type = "button",
          class = "prototype-palette-orientation",
          `data-prototype-action` = "select-story",
          `data-entry-id` = "sample-entry-1",
          `data-entry-position` = 1L,
          shiny::tags$span("Orientation \u00b7 boundary"),
          shiny::tags$strong(
            "Inspect source, copy, and interaction separation"
          ),
          shiny::tags$small("Shown because it bears on Rill's product model")
        ),
        shiny::tags$button(
          type = "button",
          class = "prototype-palette-orientation",
          `data-prototype-action` = "select-story",
          `data-entry-id` = "sample-entry-5",
          `data-entry-position` = 5L,
          shiny::tags$span("Orientation \u00b7 connection"),
          shiny::tags$strong("Compare deployment with durability decisions"),
          shiny::tags$small("My read \u00b7 two source-linked stories")
        )
      ),
      shiny::tags$div(
        class = "prototype-palette-reader",
        shiny::tags$p(
          class = "prototype-palette-intro",
          "About ",
          shiny::tags$strong(`data-agent-story-title` = NA, "this Document")
        ),
        shiny::tags$div(
          class = "prototype-palette-actions",
          prototype_palette_action(
            "Explain a passage",
            "Stay inside the current Document",
            "quote"
          ),
          prototype_palette_action(
            "Connect to earlier reading",
            "Use bounded active Reader Context",
            "diagram-3"
          ),
          prototype_palette_action(
            "Research the public web",
            "Requires explicit Research Scope expansion",
            "globe2"
          ),
          prototype_palette_action(
            "Carry something forward",
            "Preview a Reading Artifact before Apply",
            "arrow-up-right",
            action = "proposal-c"
          )
        ),
        prototype_reading_artifact_proposal(
          class = "prototype-c-proposal",
          dismiss_action = "dismiss-proposal-c"
        ),
        prototype_composer("Ask about this story\u2026")
      ),
      shiny::tags$p(
        class = "prototype-feedback",
        `aria-live` = "polite"
      )
    )
  )
}

prototype_palette_action <- function(
  title,
  detail,
  icon,
  action = "prototype-feedback"
) {
  shiny::tags$button(
    type = "button",
    class = "prototype-palette-action",
    `data-prototype-action` = action,
    bsicons::bs_icon(icon),
    shiny::tags$span(shiny::tags$strong(title), shiny::tags$small(detail))
  )
}

prototype_evidence_panel <- function() {
  shiny::tags$aside(
    class = "prototype-evidence-panel",
    hidden = NA,
    role = "dialog",
    `aria-labelledby` = "prototype-evidence-title",
    shiny::tags$header(
      shiny::tags$div(
        shiny::tags$p(class = "prototype-kicker", "Source Evidence"),
        shiny::tags$h2(id = "prototype-evidence-title", "Exact stored passage")
      ),
      prototype_icon_button("x-lg", "Close evidence", "close-evidence")
    ),
    shiny::tags$blockquote(`data-prototype-evidence-quote` = NA),
    shiny::tags$dl(
      shiny::tags$div(
        shiny::tags$dt("Document"),
        shiny::tags$dd("Bundled demo reading copy")
      ),
      shiny::tags$div(
        shiny::tags$dt("Original Source"),
        shiny::tags$dd("Demo-linked public page")
      ),
      shiny::tags$div(
        shiny::tags$dt("Acquisition"),
        shiny::tags$dd("Bundled sample \u00b7 Rill")
      ),
      shiny::tags$div(
        shiny::tags$dt("Limitation"),
        shiny::tags$dd("Prototype copy; not evidence for real-world claims")
      )
    )
  )
}

prototype_switcher <- function() {
  shiny::tags$nav(
    class = "prototype-switcher",
    `aria-label` = "Prototype variants",
    prototype_icon_button("arrow-left", "Previous variant", "previous-variant"),
    shiny::tags$div(
      shiny::tags$strong(
        `data-prototype-variant-label` = NA,
        "A \u00b7 Editorial seam"
      ),
      shiny::tags$small(
        `data-prototype-state` = NA,
        `aria-live` = "polite",
        "queue \u00b7 closed"
      )
    ),
    prototype_icon_button("arrow-right", "Next variant", "next-variant")
  )
}

prototype_port <- function(
  value = Sys.getenv("RILL_PROTOTYPE_PORT", unset = "7348")
) {
  value <- trimws(value)
  valid_text <- length(value) == 1L &&
    !is.na(value) &&
    grepl("^[0-9]+$", value)
  port <- if (valid_text) strtoi(value) else NA_integer_

  if (is.na(port) || port < 1L || port > 65535L) {
    cli::cli_abort(
      c(
        paste(
          "{.envvar RILL_PROTOTYPE_PORT} must be an integer from",
          "1 to 65535."
        ),
        "x" = "Received {.val {value}}."
      ),
      class = "rill_prototype_port_error"
    )
  }

  port
}

prototype_story_sidebar_ui <- function() {
  sidebar <- story_sidebar_ui()
  sidebar$children <- append(
    sidebar$children,
    list(agent_interaction_prototype_queue()),
    after = 1L
  )
  sidebar
}

prototype_reader_pane_ui <- function() {
  shiny::tags$div(
    class = "reader-scroll",
    shiny::uiOutput("reader_header", container = shiny::tags$div),
    agent_interaction_prototype_reader(),
    shiny::uiOutput("reader_body", container = shiny::tags$div)
  )
}

prototype_ui <- function(config) {
  bslib::page_fillable(
    theme = bslib::bs_theme(
      version = 5,
      brand = brand.yml::read_brand_yml(
        rill_package_file("app", "_brand.yml")
      )
    ),
    fillable_mobile = TRUE,
    shiny::tags$head(
      shiny::tags$meta(
        name = "viewport",
        content = "width=device-width, initial-scale=1"
      ),
      shiny::tags$title("Rill \u2014 agent interaction prototype"),
      shiny::includeScript(rill_package_file("app", "www", "app.js")),
      shiny::includeCSS(rill_package_file("app", "www", "styles.css")),
      agent_interaction_prototype_head()
    ),
    bslib::as_fill_carrier(
      shiny::tags$main(
        class = "app-shell agent-interaction-prototype",
        bslib::layout_sidebar(
          bslib::layout_sidebar(
            prototype_reader_pane_ui(),
            sidebar = prototype_story_sidebar_ui(),
            fillable = TRUE,
            fill = TRUE,
            border = FALSE,
            border_radius = FALSE,
            padding = 0,
            gap = 0,
            height = "100%",
            class = "reader-pane"
          ),
          sidebar = navigation_sidebar_ui(config),
          fillable = TRUE,
          fill = TRUE,
          border = FALSE,
          border_radius = FALSE,
          padding = 0,
          gap = 0,
          height = "100%",
          class = "reading-shell"
        ),
        agent_interaction_prototype_shell()
      )
    )
  )
}

prototype_app <- function(prototype_dir) {
  prototype_dir <- normalizePath(prototype_dir, mustWork = TRUE)
  options(rill.prototype.root = prototype_dir)
  Sys.setenv(
    DATABASE_URL = "",
    OTEL_EXPORTER_OTLP_ENDPOINT = "",
    OTEL_INSTRUMENTATION_GENAI_CAPTURE_MESSAGE_CONTENT = "false",
    RILL_CAPTURE_TOKEN = "",
    RILL_REFRESH_ON_START = "false"
  )

  config <- rill_config()
  options(
    rill.prototype.data_destination = rill_agent_data_destination(
      config$agent_model
    )
  )
  init_telemetry(config)
  store <- rill_store(config)
  store_interrupt_agent_runs(store, recovery = "process_restart")
  shiny::addResourcePath(
    "rill-assets",
    rill_package_file("app", "www")
  )
  shiny::onStop(\() rill_store_close(store))

  shiny::shinyApp(
    ui = prototype_ui(config),
    server = rill_server(config, store)
  )
}

run_agent_interaction_prototype <- function(prototype_dir) {
  port <- prototype_port()
  url <- paste0("http://127.0.0.1:", port, "/?variant=D")

  cli::cli_inform(c(
    "i" = "Agent interaction prototype: {url}",
    "i" = paste(
      "Switch variants with the bottom control or",
      "{.code ?variant=A|B|C|D}."
    )
  ))

  shiny::runApp(
    prototype_app(prototype_dir),
    host = "127.0.0.1",
    port = port,
    launch.browser = FALSE
  )
}
