#' Chores Dashboard Module UI
#'
#' Main chores dashboard with multiple view modes.
#'
#' @param id Module namespace ID
#'
#' @return A Shiny UI definition
#'
#' @keywords internal
mod_chores_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::div(
    class = "chores-view-container",

    # Navigation header
    htmltools::div(
      class = "chores-nav",
      style = "display: flex; align-items: center; justify-content: space-between; flex-direction: row; margin-bottom: 1rem;",

      # Title and view toggle
      htmltools::div(
        class = "chores-header-left",
        style = "display: flex; align-items: center; gap: 1rem;",
        htmltools::h2(
          class = "chores-title",
          style = "margin: 0; font-size: 1.5rem;",
          "Family Chores"
        ),
        htmltools::div(
          class = "btn-group btn-group-sm",
          role = "group",
          shiny::actionButton(ns("view_person"), "By Person", class = "btn btn-outline-primary active"),
          shiny::actionButton(ns("view_chore"), "By Chore", class = "btn btn-outline-primary"),
          shiny::actionButton(ns("view_leaderboard"), "Leaderboard", class = "btn btn-outline-primary")
        )
      ),

      # Action buttons
      htmltools::div(
        class = "chores-header-right",
        style = "display: flex; gap: 0.5rem;",
        shiny::actionButton(
          ns("manage_family"),
          "",
          icon = bsicons::bs_icon("people"),
          class = "btn-outline-secondary",
          title = "Manage Family"
        ),
        shiny::actionButton(
          ns("add_chore"),
          "Add Chore",
          icon = bsicons::bs_icon("plus"),
          class = "btn-primary"
        )
      )
    ),

    # Date filter row
    htmltools::div(
      class = "chores-filter-row",
      style = "display: flex; align-items: center; gap: 0.5rem; margin-bottom: 1rem;",
      shiny::actionButton(ns("filter_today"), "Today", class = "btn btn-sm btn-outline-secondary active"),
      shiny::actionButton(ns("filter_week"), "This Week", class = "btn btn-sm btn-outline-secondary"),
      htmltools::div(class = "flex-grow-1"),
      shiny::uiOutput(ns("summary_stats"))
    ),

    # Main content area
    shiny::uiOutput(ns("chores_content"))
  )
}

#' Chores Dashboard Module Server
#'
#' @param id Module namespace ID
#' @param family_members Reactive containing family members list (optional)
#'
#' @keywords internal
mod_chores_server <- function(id, family_members = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # State
    view_mode <- shiny::reactiveVal("person")  # person, chore, leaderboard
    date_filter <- shiny::reactiveVal("today")  # today, week
    refresh_trigger <- shiny::reactiveVal(0)

    # Load data reactives
    members <- shiny::reactive({
      refresh_trigger()
      get_family_members(active_only = TRUE)
    })

    chores <- shiny::reactive({
      refresh_trigger()
      get_chores(active_only = TRUE)
    })

    assignments <- shiny::reactive({
      refresh_trigger()
      filter <- date_filter()

      if (filter == "today") {
        get_assignments_for_date(Sys.Date())
      } else {
        # This week (Monday to Sunday)
        today <- Sys.Date()
        week_start <- today - as.numeric(format(today, "%u")) + 1
        week_end <- week_start + 6

        # Get all assignments for the week
        all_assignments <- data.frame()
        for (d in seq(week_start, week_end, by = "day")) {
          day_assignments <- get_assignments_for_date(as.Date(d, origin = "1970-01-01"))
          if (nrow(day_assignments) > 0) {
            all_assignments <- rbind(all_assignments, day_assignments)
          }
        }
        all_assignments
      }
    })

    leaderboard <- shiny::reactive({
      refresh_trigger()
      period <- if (date_filter() == "today") "day" else "week"
      get_leaderboard(period)
    })

    # View mode switching
    shiny::observeEvent(input$view_person, {
      view_mode("person")
      update_view_buttons("person")
    })

    shiny::observeEvent(input$view_chore, {
      view_mode("chore")
      update_view_buttons("chore")
    })

    shiny::observeEvent(input$view_leaderboard, {
      view_mode("leaderboard")
      update_view_buttons("leaderboard")
    })

    update_view_buttons <- function(active) {
      # Update button states via JS
      shiny::runjs(sprintf("
        document.querySelectorAll('#%s .btn-group .btn').forEach(function(btn) {
          btn.classList.remove('active');
        });
        document.getElementById('%s').classList.add('active');
      ", ns(""), ns(paste0("view_", active))))
    }

    # Date filter switching
    shiny::observeEvent(input$filter_today, {
      date_filter("today")
      shiny::runjs(sprintf("
        document.getElementById('%s').classList.add('active');
        document.getElementById('%s').classList.remove('active');
      ", ns("filter_today"), ns("filter_week")))
    })

    shiny::observeEvent(input$filter_week, {
      date_filter("week")
      shiny::runjs(sprintf("
        document.getElementById('%s').classList.remove('active');
        document.getElementById('%s').classList.add('active');
      ", ns("filter_today"), ns("filter_week")))
    })

    # Summary stats
    output$summary_stats <- shiny::renderUI({
      a <- assignments()
      if (is.null(a) || nrow(a) == 0) {
        return(htmltools::span(class = "text-muted", "No chores assigned"))
      }

      completed <- sum(a$status == "completed")
      total <- nrow(a)

      htmltools::div(
        class = "d-flex align-items-center gap-2",
        htmltools::span(
          class = if (completed == total) "text-success fw-bold" else "text-muted",
          paste0(completed, "/", total, " completed")
        ),
        if (completed == total && total > 0) {
          bsicons::bs_icon("check-circle-fill", class = "text-success")
        }
      )
    })

    # Main content rendering
    output$chores_content <- shiny::renderUI({
      mode <- view_mode()
      m <- members()
      a <- assignments()

      if (is.null(m) || nrow(m) == 0) {
        return(no_members_placeholder(ns))
      }

      switch(mode,
        "person" = render_by_person(m, a, ns),
        "chore" = render_by_chore(a, ns),
        "leaderboard" = render_leaderboard(leaderboard(), ns),
        render_by_person(m, a, ns)
      )
    })

    # Quick complete handlers - dynamic observers
    shiny::observe({
      a <- assignments()
      if (is.null(a) || nrow(a) == 0) return()

      lapply(a$id, function(assignment_id) {
        complete_id <- paste0("complete_", assignment_id)

        shiny::observeEvent(input[[complete_id]], {
          current_status <- a[a$id == assignment_id, "status"]
          if (length(current_status) > 0 && current_status == "pending") {
            complete_assignment(assignment_id)
            shiny::showNotification("Chore completed! +points", type = "message", duration = 2)
          } else if (length(current_status) > 0 && current_status == "completed") {
            uncomplete_assignment(assignment_id)
            shiny::showNotification("Chore uncompleted", type = "warning", duration = 2)
          }
          refresh_trigger(refresh_trigger() + 1)
        }, ignoreInit = TRUE, once = FALSE)
      })
    })

    # Add chore modal
    shiny::observeEvent(input$add_chore, {
      m <- members()
      shiny::showModal(shiny::modalDialog(
        title = "Add New Chore",
        chore_form(ns, NULL, m),
        size = "m",
        footer = htmltools::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(ns("save_new_chore"), "Create Chore", class = "btn-primary")
        ),
        easyClose = TRUE
      ))
    })

    # Save new chore
    shiny::observeEvent(input$save_new_chore, {
      title <- trimws(input$chore_title)
      if (nchar(title) == 0) {
        shiny::showNotification("Title is required", type = "error")
        return()
      }

      # Create the chore
      chore_id <- create_chore(
        title = title,
        description = input$chore_description,
        points = as.integer(input$chore_points),
        frequency = input$chore_frequency,
        category = input$chore_category,
        icon_emoji = input$chore_icon
      )

      # Create assignments for selected members
      selected_members <- input$chore_assign_to
      if (!is.null(selected_members) && length(selected_members) > 0) {
        for (member_id in selected_members) {
          create_assignment(chore_id, as.integer(member_id), Sys.Date())
        }
      }

      refresh_trigger(refresh_trigger() + 1)
      shiny::removeModal()
      shiny::showNotification(paste("Chore", shQuote(title), "created!"), type = "message")
    })

    # Manage family modal
    shiny::observeEvent(input$manage_family, {
      shiny::showModal(shiny::modalDialog(
        title = "Manage Family Members",
        mod_family_ui(ns("family_inline")),
        size = "m",
        footer = shiny::modalButton("Done"),
        easyClose = TRUE
      ))

      # Initialize family module server if not already
      mod_family_server("family_inline")
    })

    # Return refresh trigger for external use
    refresh_trigger
  })
}

# =============================================================================
# RENDER HELPERS
# =============================================================================

#' Render By Person View
#'
#' @keywords internal
render_by_person <- function(members, assignments, ns) {
  if (nrow(members) == 0) {
    return(no_members_placeholder(ns))
  }

  htmltools::div(
    class = "chores-by-person",
    lapply(seq_len(nrow(members)), function(i) {
      member <- members[i, ]

      # Filter assignments for this member
      member_assignments <- if (!is.null(assignments) && nrow(assignments) > 0) {
        assignments[assignments$member_id == member$id, ]
      } else {
        data.frame()
      }

      member_column(member, member_assignments, ns)
    })
  )
}

#' Member Column
#'
#' @keywords internal
member_column <- function(member, assignments, ns) {
  display_name <- if (!is.null(member$display_name) && !is.na(member$display_name) && nchar(member$display_name) > 0) {
    member$display_name
  } else {
    member$name
  }

  # Get member stats
  streak <- tryCatch(get_current_streak(member$id), error = function(e) 0)
  today_points <- tryCatch(get_today_points(member$id), error = function(e) 0)

  completed <- if (nrow(assignments) > 0) sum(assignments$status == "completed") else 0
  total <- nrow(assignments)

  htmltools::div(
    class = "member-column",
    style = htmltools::css(`--member-color` = member$color),

    # Header
    htmltools::div(
      class = "member-header",
      htmltools::span(class = "member-avatar-lg", member$avatar_emoji),
      htmltools::div(
        class = "member-info",
        htmltools::span(class = "member-name fw-medium", display_name),
        htmltools::div(
          class = "member-stats d-flex gap-2",
          if (streak > 0) htmltools::span(class = "streak-badge", paste0("\U0001F525", streak)),
          htmltools::span(class = "points-badge", paste0(today_points, " pts"))
        )
      ),
      htmltools::div(
        class = "member-progress ms-auto",
        htmltools::span(
          class = if (completed == total && total > 0) "text-success fw-bold" else "",
          paste0(completed, "/", total)
        )
      )
    ),

    # Chores list
    htmltools::div(
      class = "member-chores",
      if (nrow(assignments) == 0) {
        htmltools::div(
          class = "text-muted text-center py-3 small",
          "No chores assigned"
        )
      } else {
        lapply(seq_len(nrow(assignments)), function(j) {
          chore_card(assignments[j, ], ns, show_member = FALSE)
        })
      }
    )
  )
}

#' Render By Chore View
#'
#' @keywords internal
render_by_chore <- function(assignments, ns) {
  if (is.null(assignments) || nrow(assignments) == 0) {
    return(htmltools::div(
      class = "text-center text-muted py-5",
      htmltools::p("No chores assigned for this period."),
      htmltools::p(class = "small", "Click 'Add Chore' to create one!")
    ))
  }

  # Group by chore
  chore_ids <- unique(assignments$chore_id)

  htmltools::div(
    class = "chores-by-chore",
    lapply(chore_ids, function(chore_id) {
      chore_assignments <- assignments[assignments$chore_id == chore_id, ]
      chore_info <- chore_assignments[1, ]

      htmltools::div(
        class = "chore-group",
        htmltools::div(
          class = "chore-group-header",
          htmltools::span(class = "chore-icon", chore_info$chore_icon),
          htmltools::span(class = "chore-title fw-medium", chore_info$chore_title),
          htmltools::span(class = "chore-points text-success", paste0("+", chore_info$chore_points))
        ),
        htmltools::div(
          class = "chore-assignees",
          lapply(seq_len(nrow(chore_assignments)), function(j) {
            a <- chore_assignments[j, ]
            htmltools::div(
              class = paste("chore-assignee-row", if (a$status == "completed") "completed"),
              style = htmltools::css(`--member-color` = a$member_color),
              shiny::tags$input(
                type = "checkbox",
                class = "form-check-input chore-checkbox",
                id = ns(paste0("complete_", a$id)),
                checked = if (a$status == "completed") "checked" else NULL,
                onclick = sprintf("Shiny.setInputValue('%s', Math.random())", ns(paste0("complete_", a$id)))
              ),
              htmltools::span(class = "assignee-avatar", a$member_avatar),
              htmltools::span(class = "assignee-name", a$member_display_name %||% a$member_name)
            )
          })
        )
      )
    })
  )
}

#' Render Leaderboard View
#'
#' @keywords internal
render_leaderboard <- function(leaderboard, ns) {
  if (is.null(leaderboard) || nrow(leaderboard) == 0) {
    return(htmltools::div(
      class = "text-center text-muted py-5",
      htmltools::p("No data yet!"),
      htmltools::p(class = "small", "Complete some chores to see the leaderboard.")
    ))
  }

  htmltools::div(
    class = "leaderboard-view",
    lapply(seq_len(nrow(leaderboard)), function(i) {
      member <- leaderboard[i, ]
      is_first <- i == 1 && member$total_points > 0

      htmltools::div(
        class = paste("leaderboard-row", if (is_first) "first-place"),
        style = htmltools::css(`--member-color` = member$color),

        # Rank
        htmltools::div(
          class = "leaderboard-rank",
          if (is_first) "\U0001F3C6" else i
        ),

        # Member info
        htmltools::span(class = "member-avatar-lg", member$avatar_emoji),
        htmltools::div(
          class = "member-info flex-grow-1",
          htmltools::span(class = "member-name fw-medium", member$display_name %||% member$name),
          htmltools::span(class = "text-muted small", paste(member$completions, "chores"))
        ),

        # Points
        htmltools::div(
          class = "leaderboard-points",
          htmltools::span(class = "points-value fw-bold", member$total_points),
          htmltools::span(class = "points-label text-muted small", "pts")
        ),

        # Streak
        if (tryCatch(get_current_streak(member$id), error = function(e) 0) > 0) {
          htmltools::span(
            class = "streak-badge ms-2",
            paste0("\U0001F525", get_current_streak(member$id))
          )
        }
      )
    })
  )
}

#' Chore Card
#'
#' @keywords internal
chore_card <- function(assignment, ns, show_member = TRUE) {
  is_completed <- assignment$status == "completed"

  htmltools::div(
    class = paste("chore-card", if (is_completed) "completed"),
    `data-assignment-id` = assignment$id,

    # Checkbox
    shiny::tags$input(
      type = "checkbox",
      class = "form-check-input chore-checkbox",
      id = ns(paste0("complete_", assignment$id)),
      checked = if (is_completed) "checked" else NULL,
      onclick = sprintf("Shiny.setInputValue('%s', Math.random())", ns(paste0("complete_", assignment$id)))
    ),

    # Icon and title
    htmltools::span(class = "chore-icon", assignment$chore_icon),
    htmltools::span(
      class = "chore-title",
      assignment$chore_title
    ),

    # Points
    htmltools::span(class = "chore-points", paste0("+", assignment$chore_points)),

    # Member avatar (if showing)
    if (show_member) {
      htmltools::span(
        class = "chore-member-avatar",
        style = htmltools::css(`background-color` = paste0(assignment$member_color, "30")),
        assignment$member_avatar
      )
    }
  )
}

#' No Members Placeholder
#'
#' @keywords internal
no_members_placeholder <- function(ns) {
  htmltools::div(
    class = "text-center py-5",
    htmltools::div(style = "font-size: 3rem;", "\U0001F46A"),
    htmltools::h4("No family members yet"),
    htmltools::p(class = "text-muted", "Add family members to start tracking chores!"),
    shiny::actionButton(
      ns("manage_family"),
      "Add Family Members",
      icon = bsicons::bs_icon("person-plus"),
      class = "btn-primary mt-2"
    )
  )
}

#' Chore Form
#'
#' @keywords internal
chore_form <- function(ns, chore = NULL, members = NULL) {
  is_edit <- !is.null(chore)

  # Defaults
  title_val <- if (is_edit) chore$title else ""
  desc_val <- if (is_edit) chore$description else ""
  points_val <- if (is_edit) chore$points else 10
  freq_val <- if (is_edit) chore$frequency else "daily"
  cat_val <- if (is_edit) chore$category else "general"
  icon_val <- if (is_edit) chore$icon_emoji else "\U0001F9F9"

  # Icon options
  icon_choices <- c(
    "\U0001F9F9",   # broom
    "\U0001F37D",   # plate/fork/knife
    "\U0001F6CF",   # bed
    "\U0001F6BF",   # shower
    "\U0001F9FA",   # sponge
    "\U0001F9F4",   # lotion
    "\U0001F9F5",   # thread
    "\U0001F6AE",   # trash
    "\U0001F3E0",   # house
    "\U0001F331",   # plant
    "\U0001F436",   # dog
    "\U0001F431"    # cat
  )

  htmltools::tagList(
    htmltools::div(
      class = "mb-3",
      htmltools::tags$label(class = "form-label", "Chore Name *"),
      htmltools::tags$input(
        type = "text",
        class = "form-control",
        id = ns("chore_title"),
        value = title_val,
        placeholder = "e.g., Take out trash"
      )
    ),
    htmltools::div(
      class = "row mb-3",
      htmltools::div(
        class = "col-6",
        htmltools::tags$label(class = "form-label", "Points"),
        htmltools::tags$input(
          type = "number",
          class = "form-control",
          id = ns("chore_points"),
          value = points_val,
          min = 1,
          max = 100
        )
      ),
      htmltools::div(
        class = "col-6",
        htmltools::tags$label(class = "form-label", "Frequency"),
        htmltools::tags$select(
          class = "form-select",
          id = ns("chore_frequency"),
          htmltools::tags$option(value = "daily", selected = if (freq_val == "daily") "selected", "Daily"),
          htmltools::tags$option(value = "weekly", selected = if (freq_val == "weekly") "selected", "Weekly"),
          htmltools::tags$option(value = "monthly", selected = if (freq_val == "monthly") "selected", "Monthly"),
          htmltools::tags$option(value = "once", selected = if (freq_val == "once") "selected", "One-time")
        )
      )
    ),
    htmltools::div(
      class = "mb-3",
      htmltools::tags$label(class = "form-label", "Category"),
      htmltools::tags$select(
        class = "form-select",
        id = ns("chore_category"),
        htmltools::tags$option(value = "general", selected = if (cat_val == "general") "selected", "General"),
        htmltools::tags$option(value = "kitchen", selected = if (cat_val == "kitchen") "selected", "Kitchen"),
        htmltools::tags$option(value = "bedroom", selected = if (cat_val == "bedroom") "selected", "Bedroom"),
        htmltools::tags$option(value = "bathroom", selected = if (cat_val == "bathroom") "selected", "Bathroom"),
        htmltools::tags$option(value = "outdoor", selected = if (cat_val == "outdoor") "selected", "Outdoor")
      )
    ),
    htmltools::div(
      class = "mb-3",
      htmltools::tags$label(class = "form-label", "Icon"),
      htmltools::div(
        class = "icon-picker d-flex flex-wrap gap-2",
        lapply(icon_choices, function(icon) {
          selected <- icon == icon_val
          htmltools::tags$button(
            type = "button",
            class = paste("btn icon-option", if (selected) "btn-primary" else "btn-outline-secondary"),
            style = "font-size: 1.25rem; width: 2.5rem; height: 2.5rem;",
            onclick = sprintf(
              "document.getElementById('%s').value = '%s'; this.parentNode.querySelectorAll('.icon-option').forEach(b => b.classList.remove('btn-primary')); this.classList.add('btn-primary');",
              ns("chore_icon"), icon
            ),
            icon
          )
        }),
        htmltools::tags$input(type = "hidden", id = ns("chore_icon"), value = icon_val)
      )
    ),
    htmltools::div(
      class = "mb-3",
      htmltools::tags$label(class = "form-label", "Description (optional)"),
      htmltools::tags$textarea(
        class = "form-control",
        id = ns("chore_description"),
        rows = 2,
        placeholder = "Additional details...",
        desc_val
      )
    ),
    if (!is.null(members) && nrow(members) > 0) {
      htmltools::div(
        class = "mb-3",
        htmltools::tags$label(class = "form-label", "Assign to (for today)"),
        htmltools::div(
          class = "member-checkboxes",
          lapply(seq_len(nrow(members)), function(i) {
            m <- members[i, ]
            htmltools::div(
              class = "form-check",
              htmltools::tags$input(
                type = "checkbox",
                class = "form-check-input",
                id = ns(paste0("assign_", m$id)),
                name = ns("chore_assign_to"),
                value = m$id
              ),
              htmltools::tags$label(
                class = "form-check-label",
                `for` = ns(paste0("assign_", m$id)),
                htmltools::span(m$avatar_emoji, " ", m$display_name %||% m$name)
              )
            )
          })
        )
      )
    }
  )
}
