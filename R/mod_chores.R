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
          ns("manage_chores"),
          "",
          icon = bsicons::bs_icon("list-check"),
          class = "btn-outline-secondary",
          title = "Manage All Chores"
        ),
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
    generated_icon <- shiny::reactiveVal(NULL)  # For AI-generated icon base64
    created_observers <- shiny::reactiveVal(character())  # Track created checkbox observers

    # Clean up on session end (observers auto-cleanup, this clears our tracking)
    session$onSessionEnded(function() {
      created_observers(character())
    })

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

      existing <- created_observers()

      lapply(a$id, function(assignment_id) {
        # Use a separate input name for click events to avoid conflict with checkbox binding
        click_id <- paste0("toggle_click_", assignment_id)

        # Only create observer if not already created
        if (click_id %in% existing) return()

        created_observers(c(created_observers(), click_id))

        shiny::observeEvent(input[[click_id]], {
          # Try to toggle - functions are atomic and idempotent
          # First try to complete, if that fails try to uncomplete
          result <- complete_assignment(assignment_id)
          if (result$success) {
            shiny::showNotification("Chore completed! +points", type = "message", duration = 2)
          } else {
            # Assignment wasn't pending, try to uncomplete
            result <- uncomplete_assignment(assignment_id)
            if (result$success) {
              shiny::showNotification("Chore uncompleted", type = "warning", duration = 2)
            }
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
      generated_icon(NULL)  # Reset when opening modal
    })

    # Generate AI icon
    shiny::observeEvent(input$generate_icon, {
      title <- trimws(input$chore_title)
      if (nchar(title) == 0) {
        shiny::showNotification("Enter a chore name first", type = "warning")
        return()
      }

      shiny::withProgress(message = "Generating icon...", {
        result <- generate_icon(title, type = "chore")
        if (result$success) {
          generated_icon(result$base64)
          shiny::showNotification("Icon generated!", type = "message")
        } else {
          shiny::showNotification(paste("Generation failed:", result$error), type = "error")
        }
      })
    })

    # Icon preview output
    output$icon_preview <- shiny::renderUI({
      icon_data <- generated_icon()
      if (is.null(icon_data) || nchar(icon_data) == 0) {
        # Show placeholder when no icon generated
        return(htmltools::div(
          class = "icon-preview-placeholder",
          bsicons::bs_icon("image", size = "2rem", class = "text-muted"),
          htmltools::span(class = "small text-muted d-block mt-1", "No icon yet")
        ))
      }

      htmltools::div(
        class = "icon-preview",
        htmltools::img(
          src = paste0("data:image/png;base64,", icon_data),
          class = "generated-icon-img",
          alt = "Generated icon"
        ),
        shiny::actionButton(
          ns("clear_generated_icon"),
          "",
          icon = bsicons::bs_icon("x-circle"),
          class = "btn-link btn-sm text-muted p-0 clear-icon-btn",
          title = "Remove generated icon"
        )
      )
    })

    # Clear generated icon
    shiny::observeEvent(input$clear_generated_icon, {
      generated_icon(NULL)
    })

    # Use emoji instead of AI icon
    shiny::observeEvent(input$use_emoji_icon, {
      generated_icon(NULL)  # Clear any generated icon
      shiny::showNotification(
        paste("Using emoji:", input$chore_icon),
        type = "message",
        duration = 2
      )
    })

    # Save new chore
    shiny::observeEvent(input$save_new_chore, {
      title <- trimws(input$chore_title)
      if (nchar(title) == 0) {
        shiny::showNotification("Title is required", type = "error")
        return()
      }

      # Build recurrence rule from form inputs
      recurrence_type <- input$recurrence_type
      recurrence_rule <- NULL

      if (!is.null(recurrence_type) && recurrence_type != "once") {
        # Build the recurrence rule
        rule_params <- list(
          type = recurrence_type,
          interval = as.integer(input$recurrence_interval %||% 1),
          end_type = input$end_type %||% "never",
          start_date = Sys.Date()
        )

        # Add end conditions
        if (rule_params$end_type == "after") {
          rule_params$end_count <- as.integer(input$end_count %||% 10)
        } else if (rule_params$end_type == "by_date") {
          rule_params$end_date <- input$end_date
        }

        # Type-specific options
        if (recurrence_type == "daily") {
          rule_params$by_weekday <- isTRUE(input$weekdays_only)
        } else if (recurrence_type == "weekly") {
          rule_params$by_days <- input$recurrence_days
        } else if (recurrence_type == "monthly") {
          if (input$monthly_type == "day_of_month") {
            rule_params$by_month_day <- as.integer(input$month_day %||% 1)
          } else {
            rule_params$by_week_num <- as.integer(input$week_num %||% 1)
            rule_params$by_weekday_name <- input$weekday_name %||% "monday"
          }
        }

        recurrence_rule <- do.call(create_recurrence_rule, rule_params)
      }

      # Create the chore (use generated icon if available)
      icon_data <- generated_icon()
      chore_id <- create_chore(
        title = title,
        description = input$chore_description,
        points = as.integer(input$chore_points),
        frequency = recurrence_type %||% "daily",
        category = input$chore_category,
        icon_emoji = input$chore_icon,
        icon_base64 = if (!is.null(icon_data) && nchar(icon_data) > 0) icon_data else NULL,
        recurrence_rule = recurrence_rule
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

      # Show confirmation with recurrence description
      if (!is.null(recurrence_rule)) {
        desc <- format_recurrence(recurrence_rule)
        shiny::showNotification(
          paste("Chore", shQuote(title), "created!", desc),
          type = "message",
          duration = 4
        )
      } else {
        shiny::showNotification(paste("Chore", shQuote(title), "created!"), type = "message")
      }
    })

    # Manage all chores modal
    shiny::observeEvent(input$manage_chores, {
      shiny::showModal(shiny::modalDialog(
        title = htmltools::div(
          class = "d-flex align-items-center gap-2",
          bsicons::bs_icon("list-check"),
          "Manage Chores"
        ),
        size = "l",
        easyClose = TRUE,
        footer = shiny::modalButton("Done"),
        shiny::uiOutput(ns("manage_chores_content"))
      ))
    })

    # Render manage chores content
    output$manage_chores_content <- shiny::renderUI({
      refresh_trigger()  # React to changes
      all_chores <- get_chores(active_only = TRUE)
      m <- members()

      if (is.null(all_chores) || nrow(all_chores) == 0) {
        return(htmltools::div(
          class = "text-center text-muted py-4",
          htmltools::p("No chores defined yet."),
          htmltools::p(class = "small", "Click 'Add Chore' to create one!")
        ))
      }

      # Get today's assignments to show status
      today_assignments <- get_assignments_for_date(Sys.Date())

      htmltools::div(
        class = "manage-chores-list",
        lapply(seq_len(nrow(all_chores)), function(i) {
          chore <- all_chores[i, ]

          # Check who has this chore assigned today
          chore_today <- today_assignments[today_assignments$chore_id == chore$id, ]
          assigned_member_ids <- if (nrow(chore_today) > 0) chore_today$member_id else integer(0)

          htmltools::div(
            class = "manage-chore-item d-flex align-items-center gap-3 p-3 border-bottom",
            # Icon and title
            htmltools::div(
              class = "d-flex align-items-center gap-2 flex-grow-1",
              render_chore_icon(chore$icon_base64, chore$icon_emoji, size = "2rem"),
              htmltools::div(
                htmltools::div(class = "fw-medium", chore$title),
                htmltools::div(
                  class = "small text-muted",
                  paste0(chore$points, " pts • ",
                         if (!is.null(chore$recurrence_rule) && !is.na(chore$recurrence_rule) &&
                             nchar(chore$recurrence_rule) > 0) {
                           format_recurrence(chore$recurrence_rule)
                         } else {
                           chore$frequency
                         })
                )
              )
            ),
            # Quick assign checkboxes for today
            htmltools::div(
              class = "d-flex gap-2 align-items-center",
              htmltools::span(class = "small text-muted me-2", "Today:"),
              if (!is.null(m) && nrow(m) > 0) {
                lapply(seq_len(nrow(m)), function(j) {
                  member <- m[j, ]
                  is_assigned <- member$id %in% assigned_member_ids
                  input_id <- paste0("assign_", chore$id, "_", member$id)
                  htmltools::div(
                    class = "form-check form-check-inline",
                    title = member$name,
                    shiny::tags$input(
                      type = "checkbox",
                      class = "form-check-input",
                      id = ns(input_id),
                      checked = if (is_assigned) "checked" else NULL,
                      onclick = sprintf(
                        "Shiny.setInputValue('%s', {chore_id: %d, member_id: %d, checked: this.checked, nonce: Math.random()})",
                        ns("quick_assign"), chore$id, member$id
                      )
                    ),
                    htmltools::tags$label(
                      class = "form-check-label",
                      `for` = ns(input_id),
                      member$avatar_emoji
                    )
                  )
                })
              }
            ),
            # Delete button
            shiny::actionButton(
              ns(paste0("delete_chore_", chore$id)),
              "",
              icon = bsicons::bs_icon("trash"),
              class = "btn-outline-danger btn-sm",
              title = "Delete chore",
              onclick = sprintf(
                "Shiny.setInputValue('%s', {id: %d, nonce: Math.random()})",
                ns("delete_chore"), chore$id
              )
            )
          )
        })
      )
    })

    # Handle quick assign toggle
    shiny::observeEvent(input$quick_assign, {
      data <- input$quick_assign
      if (is.null(data)) return()

      if (data$checked) {
        # Create assignment for today
        create_assignment(data$chore_id, data$member_id, Sys.Date())
        shiny::showNotification("Chore assigned!", type = "message", duration = 2)
      } else {
        # Remove assignment for today
        existing <- db_query("
          SELECT id FROM chore_assignments
          WHERE chore_id = ? AND member_id = ? AND assigned_date = ? AND status = 'pending'
        ", params = list(data$chore_id, data$member_id, as.character(Sys.Date())))

        if (nrow(existing) > 0) {
          db_execute("DELETE FROM chore_assignments WHERE id = ?",
                     params = list(existing$id[1]))
          shiny::showNotification("Assignment removed", type = "warning", duration = 2)
        }
      }
      refresh_trigger(refresh_trigger() + 1)
    }, ignoreInit = TRUE)

    # Handle chore deletion
    shiny::observeEvent(input$delete_chore, {
      data <- input$delete_chore
      if (is.null(data)) return()

      # Soft delete the chore
      delete_chore(data$id)
      shiny::showNotification("Chore deleted", type = "warning", duration = 2)
      refresh_trigger(refresh_trigger() + 1)
    }, ignoreInit = TRUE)

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

#' Get Member Display Name
#'
#' Returns display_name if set, otherwise falls back to name.
#'
#' @param member A single-row member data frame or list with name and display_name.
#'
#' @return The display name string.
#'
#' @keywords internal
get_member_display_name <- function(member) {
  display_name <- member$display_name
  if (!is.null(display_name) && !is.na(display_name) && nchar(display_name) > 0) {
    display_name
  } else {
    member$name
  }
}

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
  display_name <- get_member_display_name(member)

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
          render_chore_icon(chore_info$chore_icon_base64, chore_info$chore_icon),
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
                onclick = sprintf("Shiny.setInputValue('%s', Math.random())", ns(paste0("toggle_click_", a$id)))
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
      streak <- tryCatch(get_current_streak(member$id), error = function(e) 0)

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
        if (streak > 0) {
          htmltools::span(class = "streak-badge ms-2", paste0("\U0001F525", streak))
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

    # Checkbox - uses toggle_click_ input to avoid Shiny checkbox binding conflicts
    shiny::tags$input(
      type = "checkbox",
      class = "form-check-input chore-checkbox",
      id = ns(paste0("complete_", assignment$id)),
      checked = if (is_completed) "checked" else NULL,
      onclick = sprintf("Shiny.setInputValue('%s', Math.random())", ns(paste0("toggle_click_", assignment$id)))
    ),

    # Icon and title
    render_chore_icon(assignment$chore_icon_base64, assignment$chore_icon),
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

  # Icon options - self-named vector for radioButtons (name = value for emojis)
  icon_emojis <- c("\U0001F9F9", "\U0001F37D", "\U0001F6CF", "\U0001F6BF",
                   "\U0001F9FA", "\U0001F9F4", "\U0001F9F5", "\U0001F6AE",
                   "\U0001F3E0", "\U0001F331", "\U0001F436", "\U0001F431")
  icon_choices <- setNames(icon_emojis, icon_emojis)

  # Member choices for assignment (if members exist)
  member_choices <- if (!is.null(members) && nrow(members) > 0) {
    setNames(
      as.character(members$id),
      paste(members$avatar_emoji, members$display_name %||% members$name)
    )
  } else {
    NULL
  }

  # Day name choices for weekly recurrence
  day_choices <- c(
    "Monday" = "monday",
    "Tuesday" = "tuesday",
    "Wednesday" = "wednesday",
    "Thursday" = "thursday",
    "Friday" = "friday",
    "Saturday" = "saturday",
    "Sunday" = "sunday"
  )

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
          ns("chore_category"),
          "Category",
          choices = c(
            "General" = "general",
            "Kitchen" = "kitchen",
            "Bedroom" = "bedroom",
            "Bathroom" = "bathroom",
            "Living Room" = "living_room",
            "Laundry" = "laundry",
            "Outdoor/Yard" = "outdoor",
            "Yard/Garden" = "yard",
            "Garage" = "garage",
            "Pets" = "pets",
            "Car/Vehicle" = "car",
            "Errands" = "errands",
            "Meals/Cooking" = "meals",
            "Organization" = "organization",
            "Maintenance" = "maintenance"
          ),
          selected = cat_val
        )
      )
    ),
    # Recurrence section
    htmltools::div(
      class = "recurrence-section mb-3 p-3 border rounded",
      htmltools::tags$label(class = "form-label fw-medium", "Repeats"),
      htmltools::div(
        class = "row g-2",
        htmltools::div(
          class = "col-6",
          shiny::selectInput(
            ns("recurrence_type"),
            NULL,
            choices = c(
              "Daily" = "daily",
              "Weekly" = "weekly",
              "Monthly" = "monthly",
              "One-time" = "once"
            ),
            selected = freq_val
          )
        ),
        htmltools::div(
          class = "col-6",
          shiny::numericInput(
            ns("recurrence_interval"),
            NULL,
            value = 1,
            min = 1,
            max = 99
          ) |> htmltools::tagAppendAttributes(
            class = "interval-input",
            placeholder = "Every X"
          )
        )
      ),
      # Daily options
      shiny::conditionalPanel(
        condition = sprintf("input['%s'] == 'daily'", ns("recurrence_type")),
        ns = ns,
        htmltools::div(
          class = "mt-2",
          shiny::checkboxInput(
            ns("weekdays_only"),
            "Weekdays only (Mon-Fri)",
            value = FALSE
          )
        )
      ),
      # Weekly options
      shiny::conditionalPanel(
        condition = sprintf("input['%s'] == 'weekly'", ns("recurrence_type")),
        ns = ns,
        htmltools::div(
          class = "mt-2",
          htmltools::tags$label(class = "form-label small", "On these days:"),
          shiny::checkboxGroupInput(
            ns("recurrence_days"),
            NULL,
            choices = day_choices,
            selected = NULL,
            inline = TRUE
          ) |> htmltools::tagAppendAttributes(class = "day-picker")
        )
      ),
      # Monthly options
      shiny::conditionalPanel(
        condition = sprintf("input['%s'] == 'monthly'", ns("recurrence_type")),
        ns = ns,
        htmltools::div(
          class = "mt-2",
          shiny::radioButtons(
            ns("monthly_type"),
            NULL,
            choices = c(
              "Day of month" = "day_of_month",
              "Specific weekday" = "nth_weekday"
            ),
            selected = "day_of_month",
            inline = TRUE
          ),
          # Day of month option
          shiny::conditionalPanel(
            condition = sprintf("input['%s'] == 'day_of_month'", ns("monthly_type")),
            ns = ns,
            htmltools::div(
              class = "d-flex align-items-center gap-2 mt-2",
              htmltools::span("On day"),
              shiny::numericInput(
                ns("month_day"),
                NULL,
                value = 1,
                min = 1,
                max = 31,
                width = "80px"
              ),
              htmltools::span("of the month")
            )
          ),
          # Nth weekday option
          shiny::conditionalPanel(
            condition = sprintf("input['%s'] == 'nth_weekday'", ns("monthly_type")),
            ns = ns,
            htmltools::div(
              class = "d-flex align-items-center gap-2 mt-2 flex-wrap",
              htmltools::span("On the"),
              shiny::selectInput(
                ns("week_num"),
                NULL,
                choices = c(
                  "1st" = "1",
                  "2nd" = "2",
                  "3rd" = "3",
                  "4th" = "4",
                  "Last" = "-1"
                ),
                selected = "1",
                width = "80px"
              ),
              shiny::selectInput(
                ns("weekday_name"),
                NULL,
                choices = day_choices,
                selected = "monday",
                width = "120px"
              )
            )
          )
        )
      ),
      # End condition
      htmltools::div(
        class = "mt-3 pt-2 border-top",
        htmltools::tags$label(class = "form-label small", "Ends"),
        shiny::radioButtons(
          ns("end_type"),
          NULL,
          choices = c(
            "Never" = "never",
            "After" = "after",
            "On date" = "by_date"
          ),
          selected = "never",
          inline = TRUE
        ),
        shiny::conditionalPanel(
          condition = sprintf("input['%s'] == 'after'", ns("end_type")),
          ns = ns,
          htmltools::div(
            class = "d-flex align-items-center gap-2 mt-2",
            shiny::numericInput(
              ns("end_count"),
              NULL,
              value = 10,
              min = 1,
              max = 999,
              width = "80px"
            ),
            htmltools::span("occurrences")
          )
        ),
        shiny::conditionalPanel(
          condition = sprintf("input['%s'] == 'by_date'", ns("end_type")),
          ns = ns,
          htmltools::div(
            class = "mt-2",
            shiny::dateInput(
              ns("end_date"),
              NULL,
              value = Sys.Date() + 365,
              min = Sys.Date()
            )
          )
        )
      )
    ),
    # Icon section - AI generated is primary, emoji is fallback
    htmltools::div(
      class = "icon-section mb-3",
      htmltools::tags$label(class = "form-label", "Icon"),

      # AI icon area (primary)
      htmltools::div(
        class = "ai-icon-section mb-2",
        shiny::uiOutput(ns("icon_preview")),
        if (gemini_available()) {
          htmltools::div(
            class = "d-flex gap-2 align-items-center mt-2",
            shiny::actionButton(
              ns("generate_icon"),
              htmltools::tagList(bsicons::bs_icon("stars"), "Generate Icon"),
              class = "btn-outline-primary btn-sm"
            ),
            htmltools::span(
              class = "small text-muted",
              "AI-generated based on chore name"
            )
          )
        } else {
          htmltools::div(
            class = "text-muted small",
            bsicons::bs_icon("info-circle"),
            " Set GEMINI_API_KEY for AI icons"
          )
        }
      ),

      # Emoji fallback (collapsed by default)
      htmltools::tags$details(
        class = "emoji-fallback mt-2",
        htmltools::tags$summary(
          class = "text-muted small cursor-pointer",
          "Or choose an emoji instead..."
        ),
        htmltools::div(
          class = "pt-2",
          shiny::radioButtons(
            ns("chore_icon"),
            NULL,
            choices = icon_choices,
            selected = icon_val,
            inline = TRUE
          ) |> htmltools::tagAppendAttributes(class = "icon-radio-picker"),
          shiny::actionButton(
            ns("use_emoji_icon"),
            "Use this emoji",
            class = "btn-outline-secondary btn-sm mt-2"
          )
        )
      )
    ),
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

#' Render Chore Icon
#'
#' Renders either a base64-encoded image icon or falls back to emoji.
#'
#' @param icon_base64 Base64-encoded PNG image (can be NULL or NA).
#' @param icon_emoji Fallback emoji icon.
#' @param size Icon size (CSS value, default: "1.5rem").
#'
#' @return HTML element for the icon.
#'
#' @keywords internal
render_chore_icon <- function(icon_base64, icon_emoji, size = "1.5rem") {
  has_base64 <- !is.null(icon_base64) && !is.na(icon_base64) && nchar(icon_base64) > 0

  if (has_base64) {
    htmltools::img(
      src = paste0("data:image/png;base64,", icon_base64),
      class = "chore-icon-img",
      style = htmltools::css(width = size, height = size),
      alt = ""
    )
  } else {
    htmltools::span(class = "chore-icon", icon_emoji)
  }
}
