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
      style = "display: flex; align-items: center; justify-content: space-between; flex-wrap: wrap; gap: 1rem; margin-bottom: 1rem;",

      # Title
      htmltools::h2(
        class = "chores-title",
        style = "margin: 0; font-size: 1.5rem;",
        "Family Chores"
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

    # Date filter row with summary
    htmltools::div(
      class = "chores-filter-row",
      style = "display: flex; align-items: center; gap: 1rem; margin-bottom: 1rem;",
      shiny::radioButtons(
        ns("date_filter"),
        label = NULL,
        choices = c("Today" = "today", "This Week" = "week"),
        selected = "today",
        inline = TRUE
      ) |> htmltools::tagAppendAttributes(class = "date-filter-radio"),
      htmltools::div(class = "flex-grow-1"),
      shiny::uiOutput(ns("summary_stats"))
    ),

    # Main content with navset_pill for view switching
    bslib::navset_pill(
      id = ns("view_mode"),
      bslib::nav_panel(
        title = "By Person",
        value = "person",
        shiny::uiOutput(ns("content_person"))
      ),
      bslib::nav_panel(
        title = "By Chore",
        value = "chore",
        shiny::uiOutput(ns("content_chore"))
      ),
      bslib::nav_panel(
        title = "Leaderboard",
        value = "leaderboard",
        shiny::uiOutput(ns("content_leaderboard"))
      )
    )
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

    # State - only need refresh trigger now, view_mode and date_filter come from inputs
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
      filter <- input$date_filter

      if (is.null(filter) || filter == "today") {
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

    leaderboard_data <- shiny::reactive({
      refresh_trigger()
      period <- if (is.null(input$date_filter) || input$date_filter == "today") "day" else "week"
      get_leaderboard(period)
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

    # Content rendering for each tab - bslib handles tab switching automatically
    output$content_person <- shiny::renderUI({
      m <- members()
      a <- assignments()
      if (is.null(m) || nrow(m) == 0) {
        return(no_members_placeholder(ns))
      }
      render_by_person(m, a, ns)
    })

    output$content_chore <- shiny::renderUI({
      m <- members()
      a <- assignments()
      if (is.null(m) || nrow(m) == 0) {
        return(no_members_placeholder(ns))
      }
      render_by_chore(a, ns)
    })

    output$content_leaderboard <- shiny::renderUI({
      m <- members()
      if (is.null(m) || nrow(m) == 0) {
        return(no_members_placeholder(ns))
      }
      render_leaderboard(leaderboard_data(), ns)
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
  desc_val <- if (is_edit && !is.na(chore$description)) chore$description else ""
  points_val <- if (is_edit) chore$points else 10
  freq_val <- if (is_edit) chore$frequency else "daily"
  cat_val <- if (is_edit) chore$category else "general"
  icon_val <- if (is_edit) chore$icon_emoji else "\U0001F9F9"

 # Icon options - named vector for radioButtons
  icon_choices <- c(
    "\U0001F9F9" = "\U0001F9F9",
    "\U0001F37D" = "\U0001F37D",
    "\U0001F6CF" = "\U0001F6CF",
    "\U0001F6BF" = "\U0001F6BF",
    "\U0001F9FA" = "\U0001F9FA",
    "\U0001F9F4" = "\U0001F9F4",
    "\U0001F9F5" = "\U0001F9F5",
    "\U0001F6AE" = "\U0001F6AE",
    "\U0001F3E0" = "\U0001F3E0",
    "\U0001F331" = "\U0001F331",
    "\U0001F436" = "\U0001F436",
    "\U0001F431" = "\U0001F431"
  )

  # Member choices for assignment (if members exist)
  member_choices <- if (!is.null(members) && nrow(members) > 0) {
    choices <- setNames(
      as.character(members$id),
      paste(members$avatar_emoji, members$display_name %||% members$name)
    )
    choices
  } else {
    NULL
  }

  htmltools::tagList(
    shiny::textInput(
      ns("chore_title"),
      "Chore Name *",
      value = title_val,
      placeholder = "e.g., Take out trash"
    ),
    htmltools::div(
      class = "row",
      htmltools::div(
        class = "col-6",
        shiny::numericInput(
          ns("chore_points"),
          "Points",
          value = points_val,
          min = 1,
          max = 100
        )
      ),
      htmltools::div(
        class = "col-6",
        shiny::selectInput(
          ns("chore_frequency"),
          "Frequency",
          choices = c(
            "Daily" = "daily",
            "Weekly" = "weekly",
            "Monthly" = "monthly",
            "One-time" = "once"
          ),
          selected = freq_val
        )
      )
    ),
    shiny::selectInput(
      ns("chore_category"),
      "Category",
      choices = c(
        "General" = "general",
        "Kitchen" = "kitchen",
        "Bedroom" = "bedroom",
        "Bathroom" = "bathroom",
        "Outdoor" = "outdoor"
      ),
      selected = cat_val
    ),
    # Icon picker using radioButtons (CSS in styles.css)
    shiny::radioButtons(
      ns("chore_icon"),
      "Icon",
      choices = icon_choices,
      selected = icon_val,
      inline = TRUE
    ) |> htmltools::tagAppendAttributes(class = "icon-radio-picker"),
    shiny::textAreaInput(
      ns("chore_description"),
      "Description (optional)",
      value = desc_val,
      rows = 2,
      placeholder = "Additional details..."
    ),
    if (!is.null(member_choices)) {
      shiny::checkboxGroupInput(
        ns("chore_assign_to"),
        "Assign to (for today)",
        choices = member_choices
      )
    }
  )
}
