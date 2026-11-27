group_management_ui <- function(feeds, groups) {
  active <- feeds[
    feeds$status == "active" & feeds$source_kind == "subscription",
    ,
    drop = FALSE
  ]
  choices <- stats::setNames(groups$group_id, groups$name)
  shiny::tags$section(
    class = "feed-tool-section group-tools",
    shiny::tags$h3("Groups"),
    shiny::tags$div(
      class = "feed-tool-subsection",
      shiny::tags$h4("Change Groups for several feeds"),
      shiny::selectizeInput(
        "bulk_group_feeds",
        "Feeds",
        choices = feed_manager_choices(active)[-1],
        multiple = TRUE,
        options = list(closeAfterSelect = TRUE),
        width = "100%"
      ),
      shiny::selectizeInput(
        "bulk_groups",
        "Groups",
        choices = choices,
        multiple = TRUE,
        options = list(closeAfterSelect = TRUE),
        width = "100%"
      ),
      shiny::tags$div(
        class = "feed-tool-actions",
        shiny::actionButton("add_feed_groups", "Add to Groups"),
        shiny::actionButton("remove_feed_groups", "Remove from Groups")
      )
    ),
    shiny::tags$div(
      class = "feed-tool-subsection",
      shiny::tags$h4("Create a Group"),
      shiny::textInput("new_group_name", "Name"),
      shiny::actionButton("create_group", "Create Group")
    ),
    shiny::tags$div(
      class = "feed-tool-subsection",
      shiny::tags$h4("Rename or delete a Group"),
      shiny::selectInput(
        "managed_group",
        "Group",
        choices = c("Choose a Group" = "", choices)
      ),
      shiny::textInput("group_name", "New name"),
      shiny::tags$div(
        class = "feed-tool-actions",
        shiny::actionButton("rename_group", "Rename Group"),
        shiny::actionButton(
          "delete_group",
          "Delete Group",
          class = "btn-delete-group"
        )
      ),
      shiny::uiOutput("group_delete_confirmation")
    )
  )
}

group_delete_confirmation_ui <- function(name) {
  shiny::tags$div(
    class = "group-delete-confirmation",
    role = "alert",
    shiny::tags$p(
      paste0(
        "Delete ",
        name,
        "? Its feeds leave the Group but stay in your Library with their ",
        "reading state."
      )
    ),
    shiny::tags$div(
      class = "feed-tool-actions",
      shiny::actionButton(
        "confirm_delete_group",
        paste("Delete", name),
        class = "btn-delete-group"
      ),
      shiny::actionButton("cancel_delete_group", "Cancel")
    )
  )
}

group_reading_control_ui <- function() {
  shiny::tags$details(
    class = "group-reading-control",
    shiny::tags$summary("Combine Groups"),
    shiny::selectizeInput(
      "reading_groups",
      "Groups to read",
      choices = character(),
      multiple = TRUE,
      options = list(closeAfterSelect = TRUE)
    ),
    shiny::radioButtons(
      "reading_group_match",
      "Include feeds in",
      choices = c("Any selected Group" = "any", "All selected Groups" = "all"),
      selected = "any"
    ),
    shiny::actionButton(
      "apply_reading_groups",
      "Read Groups",
      onclick = "rillOpenQueue()"
    )
  )
}
