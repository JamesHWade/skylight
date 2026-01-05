#' Quick Add Event Module UI
#'
#' Creates a modal dialog for quickly adding events.
#'
#' @param id Module namespace ID
#'
#' @return A Shiny UI definition (empty, modal is shown dynamically)
#'
#' @keywords internal
mod_quick_add_ui <- function(id) {
  ns <- shiny::NS(id)
  # Modal is created dynamically when triggered
  htmltools::tagList()
}

#' Quick Add Event Module Server
#'
#' Handles quick event creation via modal dialog.
#'
#' @param id Module namespace ID
#' @param calendars Reactive containing available calendars
#' @param refresh_trigger Reactive value to trigger calendar refresh
#'
#' @return Reactive that fires when an event is created
#'
#' @keywords internal
mod_quick_add_server <- function(id, calendars, refresh_trigger) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Track the date for the new event
    selected_date_for_event <- shiny::reactiveVal(NULL)
    generated_event_icon <- shiny::reactiveVal(NULL)  # For AI-generated icon

    # Listen for quick add trigger from JavaScript
    shiny::observeEvent(input$quick_add_trigger, {
      trigger_data <- input$quick_add_trigger
      if (!is.null(trigger_data) && !is.null(trigger_data$date)) {
        selected_date_for_event(as.Date(trigger_data$date))
        generated_event_icon(NULL)  # Reset icon
        show_quick_add_modal()
      }
    }, ignoreInit = TRUE)

    # Show the quick add modal
    show_quick_add_modal <- function() {
      date <- selected_date_for_event()
      if (is.null(date)) return()

      # Get available calendars
      cal_list <- tryCatch(calendars(), error = function(e) NULL)
      calendar_choices <- if (!is.null(cal_list) && nrow(cal_list) > 0) {
        choices <- setNames(cal_list$id, cal_list$name)
        # Put primary calendar first
        primary_idx <- which(cal_list$primary)
        if (length(primary_idx) > 0) {
          choices <- c(choices[primary_idx], choices[-primary_idx])
        }
        choices
      } else {
        c("Primary" = "primary")
      }

      shiny::showModal(
        shiny::modalDialog(
          title = htmltools::div(
            class = "d-flex align-items-center gap-2",
            bsicons::bs_icon("calendar-plus", size = "1.5rem"),
            htmltools::span("Quick Add Event"),
            htmltools::span(
              class = "text-muted ms-2 small",
              format(date, "%A, %B %d")
            )
          ),
          size = "m",
          easyClose = TRUE,
          footer = htmltools::div(
            class = "d-flex justify-content-end gap-2",
            shiny::modalButton("Cancel"),
            shiny::actionButton(
              ns("create_event"),
              "Create Event",
              class = "btn-primary",
              icon = bsicons::bs_icon("plus-circle")
            )
          ),

          # Form content
          htmltools::div(
            class = "quick-add-form",

            # Event title with icon generation
            htmltools::div(
              class = "d-flex gap-2 align-items-end mb-3",
              htmltools::div(
                class = "flex-grow-1",
                shiny::textInput(
                  ns("event_title"),
                  "Event Title",
                  placeholder = "Enter event title...",
                  width = "100%"
                )
              ),
              htmltools::div(
                class = "icon-generate-section",
                shiny::actionButton(
                  ns("generate_event_icon"),
                  htmltools::tagList(bsicons::bs_icon("stars")),
                  class = "btn-outline-primary btn-sm",
                  disabled = if (gemini_available()) NULL else "disabled",
                  title = if (gemini_available()) "Generate AI icon" else "GEMINI_API_KEY not configured"
                ),
                shiny::uiOutput(ns("event_icon_preview"))
              )
            ),

            # All day toggle and time inputs
            htmltools::div(
              class = "d-flex align-items-center gap-3 mb-3",
              shiny::checkboxInput(
                ns("all_day"),
                "All Day",
                value = FALSE,
                width = "auto"
              )
            ),

            # Time inputs (shown when not all-day)
            shiny::conditionalPanel(
              condition = sprintf("!input['%s']", ns("all_day")),
              ns = ns,
              htmltools::div(
                class = "row g-3 mb-3",
                htmltools::div(
                  class = "col-6",
                  shiny::textInput(
                    ns("start_time"),
                    "Start Time",
                    value = "09:00",
                    width = "100%"
                  )
                ),
                htmltools::div(
                  class = "col-6",
                  shiny::textInput(
                    ns("end_time"),
                    "End Time",
                    value = "10:00",
                    width = "100%"
                  )
                )
              )
            ),

            # Calendar selector
            shiny::selectInput(
              ns("calendar"),
              "Calendar",
              choices = calendar_choices,
              width = "100%"
            ),

            # Location (optional)
            shiny::textInput(
              ns("location"),
              "Location (optional)",
              placeholder = "Add location...",
              width = "100%"
            ),

            # Recurrence options
            htmltools::div(
              class = "recurrence-section mt-3",
              htmltools::tags$label(class = "form-label", "Repeat"),
              htmltools::div(
                class = "row g-2",
                htmltools::div(
                  class = "col-6",
                  shiny::selectInput(
                    ns("recurrence_type"),
                    NULL,
                    choices = c(
                      "Does not repeat" = "once",
                      "Daily" = "daily",
                      "Weekly" = "weekly",
                      "Monthly" = "monthly"
                    ),
                    selected = "once"
                  )
                ),
                htmltools::div(
                  class = "col-6",
                  shiny::conditionalPanel(
                    condition = sprintf("input['%s'] != 'once'", ns("recurrence_type")),
                    ns = ns,
                    shiny::numericInput(
                      ns("recurrence_interval"),
                      NULL,
                      value = 1,
                      min = 1,
                      max = 99
                    )
                  )
                )
              ),

              # Daily options: weekdays only
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

              # Weekly options: day picker
              shiny::conditionalPanel(
                condition = sprintf("input['%s'] == 'weekly'", ns("recurrence_type")),
                ns = ns,
                htmltools::div(
                  class = "mt-2",
                  htmltools::tags$label(class = "form-label small", "On these days:"),
                  shiny::checkboxGroupInput(
                    ns("recurrence_days"),
                    NULL,
                    choices = c(
                      "Mon" = "monday", "Tue" = "tuesday", "Wed" = "wednesday",
                      "Thu" = "thursday", "Fri" = "friday", "Sat" = "saturday",
                      "Sun" = "sunday"
                    ),
                    selected = NULL,
                    inline = TRUE
                  )
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
                      shiny::numericInput(ns("month_day"), NULL, value = 1, min = 1, max = 31, width = "80px"),
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
                        ns("week_num"), NULL,
                        choices = c("1st" = "1", "2nd" = "2", "3rd" = "3", "4th" = "4", "Last" = "-1"),
                        width = "80px"
                      ),
                      shiny::selectInput(
                        ns("weekday_name"), NULL,
                        choices = c(
                          "Monday" = "monday", "Tuesday" = "tuesday", "Wednesday" = "wednesday",
                          "Thursday" = "thursday", "Friday" = "friday", "Saturday" = "saturday",
                          "Sunday" = "sunday"
                        ),
                        width = "120px"
                      )
                    )
                  )
                )
              ),

              # End condition (when repeating)
              shiny::conditionalPanel(
                condition = sprintf("input['%s'] != 'once'", ns("recurrence_type")),
                ns = ns,
                htmltools::div(
                  class = "mt-3",
                  htmltools::tags$label(class = "form-label small", "Ends"),
                  shiny::radioButtons(
                    ns("end_type"),
                    NULL,
                    choices = c("Never" = "never", "After" = "after", "On date" = "by_date"),
                    selected = "never",
                    inline = TRUE
                  ),
                  shiny::conditionalPanel(
                    condition = sprintf("input['%s'] == 'after'", ns("end_type")),
                    ns = ns,
                    htmltools::div(
                      class = "d-flex align-items-center gap-2",
                      shiny::numericInput(ns("end_count"), NULL, value = 10, min = 1, max = 999, width = "80px"),
                      htmltools::span("occurrences")
                    )
                  ),
                  shiny::conditionalPanel(
                    condition = sprintf("input['%s'] == 'by_date'", ns("end_type")),
                    ns = ns,
                    shiny::dateInput(ns("end_date"), NULL, value = Sys.Date() + 90)
                  )
                )
              )
            ),

            # Description (optional, collapsed by default)
            htmltools::div(
              class = "mt-2",
              htmltools::tags$details(
                htmltools::tags$summary(
                  class = "text-muted small cursor-pointer",
                  "Add description..."
                ),
                shiny::textAreaInput(
                  ns("description"),
                  label = NULL,
                  placeholder = "Event description...",
                  rows = 3,
                  width = "100%"
                )
              )
            )
          )
        )
      )
    }

    # Generate AI icon for event
    shiny::observeEvent(input$generate_event_icon, {
      title <- trimws(input$event_title)
      if (nchar(title) == 0) {
        shiny::showNotification("Enter an event title first", type = "warning")
        return()
      }

      shiny::withProgress(message = "Generating icon...", {
        result <- generate_icon(title, type = "event")
        if (result$success) {
          generated_event_icon(result$base64)
          shiny::showNotification("Icon generated!", type = "message")
        } else {
          shiny::showNotification(paste("Generation failed:", result$error), type = "error")
        }
      })
    })

    # Icon preview output
    output$event_icon_preview <- shiny::renderUI({
      icon_data <- generated_event_icon()
      if (is.null(icon_data) || nchar(icon_data) == 0) {
        return(NULL)
      }

      htmltools::div(
        class = "icon-preview mt-2",
        htmltools::img(
          src = paste0("data:image/png;base64,", icon_data),
          class = "generated-icon-img",
          alt = "Generated icon"
        ),
        shiny::actionButton(
          ns("clear_event_icon"),
          "",
          icon = bsicons::bs_icon("x-circle"),
          class = "btn-link btn-sm text-muted p-0 clear-icon-btn",
          title = "Remove generated icon"
        )
      )
    })

    # Clear generated icon
    shiny::observeEvent(input$clear_event_icon, {
      generated_event_icon(NULL)
    })

    # Handle event creation
    shiny::observeEvent(input$create_event, {
      title <- trimws(input$event_title)
      date <- selected_date_for_event()

      # Validate title
      if (is.null(title) || nchar(title) == 0) {
        shiny::showNotification(
          "Please enter an event title",
          type = "warning"
        )
        return()
      }

      # Build start/end times
      if (isTRUE(input$all_day)) {
        start <- date
        end <- date + 1
        all_day <- TRUE
      } else {
        # Parse time inputs
        start_time <- tryCatch({
          as.POSIXct(paste(date, input$start_time), format = "%Y-%m-%d %H:%M")
        }, error = function(e) NULL)

        end_time <- tryCatch({
          as.POSIXct(paste(date, input$end_time), format = "%Y-%m-%d %H:%M")
        }, error = function(e) NULL)

        if (is.null(start_time) || is.null(end_time)) {
          shiny::showNotification(
            "Invalid time format. Use HH:MM (e.g., 09:00)",
            type = "warning"
          )
          return()
        }

        start <- start_time
        end <- end_time
        all_day <- FALSE
      }

      # Build recurrence rule if not a one-time event
      recurrence_rrule <- NULL
      recurrence_type <- input$recurrence_type

      if (!is.null(recurrence_type) && recurrence_type != "once") {
        # Build the recurrence rule parameters
        rule_params <- list(
          type = recurrence_type,
          interval = as.integer(input$recurrence_interval %||% 1),
          end_type = input$end_type %||% "never",
          start_date = date
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

        # Create the rule and convert to RRULE
        recurrence_rule <- do.call(create_recurrence_rule, rule_params)
        recurrence_rrule <- recurrence_to_rrule(recurrence_rule)
      }

      # Create the event
      result <- create_event(
        title = title,
        start = start,
        end = end,
        calendar_id = input$calendar,
        description = if (nchar(trimws(input$description)) > 0) input$description else NULL,
        location = if (nchar(trimws(input$location)) > 0) input$location else NULL,
        all_day = all_day,
        recurrence = if (!is.null(recurrence_rrule)) recurrence_rrule else NULL
      )

      if (result$success) {
        # Save generated icon if present
        icon_data <- generated_event_icon()
        if (!is.null(icon_data) && nchar(icon_data) > 0 && !is.null(result$event$id)) {
          tryCatch({
            save_event_icon(result$event$id, icon_data)
          }, error = function(e) {
            # Silent fail - icon saving is optional
          })
        }

        shiny::removeModal()

        # Show confirmation with recurrence description if applicable
        if (!is.null(recurrence_rule)) {
          desc <- format_recurrence(recurrence_rule)
          shiny::showNotification(
            paste("Created:", title, "-", desc),
            type = "message",
            duration = 4
          )
        } else {
          shiny::showNotification(
            paste("Created:", title),
            type = "message",
            duration = 3
          )
        }
        # Trigger calendar refresh
        refresh_trigger(refresh_trigger() + 1)
      } else {
        shiny::showNotification(
          paste("Failed to create event:", result$error),
          type = "error",
          duration = 5
        )
      }
    }, ignoreInit = TRUE)

    # Return reactive for external use
    shiny::reactive({
      input$create_event
    })
  })
}
