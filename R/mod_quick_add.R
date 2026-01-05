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
    generated_event_icon <- shiny::reactiveVal(NULL)

    # Listen for quick add trigger from JavaScript
    shiny::observeEvent(input$quick_add_trigger, {
      trigger_data <- input$quick_add_trigger
      if (!is.null(trigger_data) && !is.null(trigger_data$date)) {
        selected_date_for_event(as.Date(trigger_data$date))
        generated_event_icon(NULL)
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
            class = "d-flex justify-content-between align-items-center w-100",
            # Recurrence preview on the left
            shiny::uiOutput(ns("recurrence_preview")),
            # Buttons on the right
            htmltools::div(
              class = "d-flex gap-2",
              shiny::modalButton("Cancel"),
              shiny::actionButton(
                ns("create_event"),
                "Create Event",
                class = "btn-primary",
                icon = bsicons::bs_icon("plus-circle")
              )
            )
          ),

          # Form content
          htmltools::div(
            class = "quick-add-form",

            # Event title
            htmltools::div(
              class = "mb-3",
              shiny::textInput(
                ns("event_title"),
                NULL,
                placeholder = "Add title",
                width = "100%"
              ) |> htmltools::tagAppendAttributes(
                class = "form-control-lg",
                style = "font-size: 1.25rem; font-weight: 500;"
              )
            ),

            # Time section
            htmltools::div(
              class = "mb-3",
              htmltools::div(
                class = "d-flex align-items-center gap-2 mb-2",
                bsicons::bs_icon("clock", class = "text-muted"),
                shiny::checkboxInput(
                  ns("all_day"),
                  "All day",
                  value = FALSE,
                  width = "auto"
                )
              ),
              shiny::uiOutput(ns("time_inputs"))
            ),

            # Calendar selector
            htmltools::div(
              class = "mb-3",
              htmltools::div(
                class = "d-flex align-items-center gap-2",
                bsicons::bs_icon("calendar3", class = "text-muted"),
                htmltools::div(
                  class = "flex-grow-1",
                  shiny::selectInput(
                    ns("calendar"),
                    NULL,
                    choices = calendar_choices,
                    width = "100%"
                  )
                )
              )
            ),

            # Location
            htmltools::div(
              class = "mb-3",
              htmltools::div(
                class = "d-flex align-items-center gap-2",
                bsicons::bs_icon("geo-alt", class = "text-muted"),
                htmltools::div(
                  class = "flex-grow-1",
                  shiny::textInput(
                    ns("location"),
                    NULL,
                    placeholder = "Add location",
                    width = "100%"
                  )
                )
              )
            ),

            # Recurrence section
            htmltools::div(
              class = "mb-3",
              htmltools::div(
                class = "d-flex align-items-start gap-2",
                bsicons::bs_icon("arrow-repeat", class = "text-muted mt-2"),
                htmltools::div(
                  class = "flex-grow-1",
                  shiny::selectInput(
                    ns("recurrence_type"),
                    NULL,
                    choices = c(
                      "Does not repeat" = "once",
                      "Daily" = "daily",
                      "Weekly" = "weekly",
                      "Monthly" = "monthly"
                    ),
                    selected = "once",
                    width = "100%"
                  ),
                  # Dynamic recurrence options
                  shiny::uiOutput(ns("recurrence_options"))
                )
              )
            ),

            # Description (collapsible)
            htmltools::div(
              class = "mb-2",
              htmltools::div(
                class = "d-flex align-items-start gap-2",
                bsicons::bs_icon("text-left", class = "text-muted mt-2"),
                htmltools::div(
                  class = "flex-grow-1",
                  shiny::textAreaInput(
                    ns("description"),
                    NULL,
                    placeholder = "Add description",
                    rows = 2,
                    width = "100%"
                  )
                )
              )
            )
          )
        )
      )
    }

    # Dynamic time inputs
    output$time_inputs <- shiny::renderUI({
      if (isTRUE(input$all_day)) {
        return(NULL)
      }

      htmltools::div(
        class = "row g-2 ms-4",
        htmltools::div(
          class = "col-6",
          shiny::textInput(
            ns("start_time"),
            NULL,
            value = "09:00",
            placeholder = "Start time",
            width = "100%"
          )
        ),
        htmltools::div(
          class = "col-6",
          shiny::textInput(
            ns("end_time"),
            NULL,
            value = "10:00",
            placeholder = "End time",
            width = "100%"
          )
        )
      )
    })

    # Dynamic recurrence options based on type
    output$recurrence_options <- shiny::renderUI({
      req(input$recurrence_type)
      rec_type <- input$recurrence_type

      if (rec_type == "once") {
        return(NULL)
      }

      # Build the options UI based on recurrence type
      options_ui <- switch(
        rec_type,
        "daily" = daily_options_ui(ns),
        "weekly" = weekly_options_ui(ns, selected_date_for_event()),
        "monthly" = monthly_options_ui(ns, selected_date_for_event()),
        NULL
      )

      htmltools::div(
        class = "recurrence-details mt-2 p-2 bg-light rounded",

        # Interval row
        htmltools::div(
          class = "d-flex align-items-center gap-2 mb-2",
          htmltools::span("Every", class = "text-muted small"),
          shiny::numericInput(
            ns("recurrence_interval"),
            NULL,
            value = 1,
            min = 1,
            max = 99,
            width = "70px"
          ),
          htmltools::span(
            class = "text-muted small",
            switch(rec_type,
              "daily" = "day(s)",
              "weekly" = "week(s)",
              "monthly" = "month(s)"
            )
          )
        ),

        # Type-specific options
        options_ui,

        # End condition
        htmltools::div(
          class = "mt-3 pt-2 border-top",
          htmltools::div(
            class = "d-flex align-items-center gap-2 flex-wrap",
            htmltools::span("Ends:", class = "text-muted small"),
            shiny::radioButtons(
              ns("end_type"),
              NULL,
              choices = c("Never" = "never", "After" = "after", "On" = "by_date"),
              selected = "never",
              inline = TRUE
            )
          ),
          shiny::uiOutput(ns("end_condition_details"))
        )
      )
    })

    # End condition details
    output$end_condition_details <- shiny::renderUI({
      req(input$end_type)

      switch(
        input$end_type,
        "after" = htmltools::div(
          class = "d-flex align-items-center gap-2 mt-2",
          shiny::numericInput(
            ns("end_count"),
            NULL,
            value = 10,
            min = 1,
            max = 999,
            width = "80px"
          ),
          htmltools::span("occurrences", class = "text-muted small")
        ),
        "by_date" = htmltools::div(
          class = "mt-2",
          shiny::dateInput(
            ns("end_date"),
            NULL,
            value = Sys.Date() + 90,
            width = "150px"
          )
        ),
        NULL
      )
    })

    # Recurrence preview in footer
    output$recurrence_preview <- shiny::renderUI({
      rec_type <- input$recurrence_type
      if (is.null(rec_type) || rec_type == "once") {
        return(htmltools::span())
      }

      # Build preview text
      interval <- input$recurrence_interval %||% 1
      preview <- switch(
        rec_type,
        "daily" = {
          if (isTRUE(input$weekdays_only)) {
            "Every weekday"
          } else if (interval == 1) {
            "Daily"
          } else {
            paste("Every", interval, "days")
          }
        },
        "weekly" = {
          days <- input$recurrence_days
          if (length(days) > 0) {
            day_abbr <- c(
              "monday" = "Mon", "tuesday" = "Tue", "wednesday" = "Wed",
              "thursday" = "Thu", "friday" = "Fri", "saturday" = "Sat",
              "sunday" = "Sun"
            )
            day_str <- paste(day_abbr[days], collapse = ", ")
            if (interval == 1) {
              paste("Weekly on", day_str)
            } else {
              paste("Every", interval, "weeks on", day_str)
            }
          } else {
            if (interval == 1) "Weekly" else paste("Every", interval, "weeks")
          }
        },
        "monthly" = {
          if (interval == 1) "Monthly" else paste("Every", interval, "months")
        },
        ""
      )

      htmltools::span(
        class = "text-muted small",
        bsicons::bs_icon("arrow-repeat", class = "me-1"),
        preview
      )
    })

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
        shiny::showNotification("Please enter an event title", type = "warning")
        return()
      }

      # Build start/end times
      if (isTRUE(input$all_day)) {
        start <- date
        end <- date + 1
        all_day <- TRUE
      } else {
        start_time <- tryCatch({
          as.POSIXct(paste(date, input$start_time), format = "%Y-%m-%d %H:%M")
        }, error = function(e) NULL)

        end_time <- tryCatch({
          as.POSIXct(paste(date, input$end_time), format = "%Y-%m-%d %H:%M")
        }, error = function(e) NULL)

        if (is.null(start_time) || is.null(end_time)) {
          shiny::showNotification("Invalid time format. Use HH:MM (e.g., 09:00)", type = "warning")
          return()
        }

        start <- start_time
        end <- end_time
        all_day <- FALSE
      }

      # Build recurrence rule if not a one-time event
      recurrence_rrule <- NULL
      recurrence_rule <- NULL
      recurrence_type <- input$recurrence_type

      if (!is.null(recurrence_type) && recurrence_type != "once") {
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
          monthly_type <- input$monthly_type %||% "day_of_month"
          if (monthly_type == "day_of_month") {
            rule_params$by_month_day <- as.integer(input$month_day %||% lubridate::day(date))
          } else {
            rule_params$by_week_num <- as.integer(input$week_num %||% 1)
            rule_params$by_weekday_name <- input$weekday_name %||% tolower(weekdays(date))
          }
        }

        recurrence_rule <- do.call(create_recurrence_rule, rule_params)
        recurrence_rrule <- recurrence_to_rrule(recurrence_rule)
      }

      # Create the event
      result <- create_event(
        title = title,
        start = start,
        end = end,
        calendar_id = input$calendar,
        description = if (nchar(trimws(input$description %||% "")) > 0) input$description else NULL,
        location = if (nchar(trimws(input$location %||% "")) > 0) input$location else NULL,
        all_day = all_day,
        recurrence = recurrence_rrule
      )

      if (result$success) {
        # Save generated icon if present
        icon_data <- generated_event_icon()
        if (!is.null(icon_data) && nchar(icon_data) > 0 && !is.null(result$event$id)) {
          tryCatch({
            save_event_icon(result$event$id, icon_data)
          }, error = function(e) NULL)
        }

        shiny::removeModal()

        # Show confirmation with recurrence description
        if (!is.null(recurrence_rule)) {
          desc <- format_recurrence(recurrence_rule)
          shiny::showNotification(
            paste("Created:", title, "-", desc),
            type = "message",
            duration = 4
          )
        } else {
          shiny::showNotification(paste("Created:", title), type = "message", duration = 3)
        }
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

# Helper: Daily recurrence options UI
daily_options_ui <- function(ns) {
  htmltools::div(
    shiny::checkboxInput(
      ns("weekdays_only"),
      "Weekdays only (Mon-Fri)",
      value = FALSE
    )
  )
}

# Helper: Weekly recurrence options UI
weekly_options_ui <- function(ns, date) {

  # Pre-select the day of the week from the selected date
  default_day <- if (!is.null(date)) {
    tolower(weekdays(date))
  } else {
    NULL
  }

  # JavaScript to handle visual toggle state
  toggle_js <- htmltools::HTML(sprintf("
    $(document).on('change', '#%s input[type=checkbox]', function() {
      $(this).closest('.checkbox-inline, .form-check').toggleClass('checked', this.checked);
    });
    // Initialize on load
    $(function() {
      $('#%s input[type=checkbox]:checked').each(function() {
        $(this).closest('.checkbox-inline, .form-check').addClass('checked');
      });
    });
  ", ns("recurrence_days"), ns("recurrence_days")))

  htmltools::tagList(
    htmltools::div(
      class = "weekly-day-picker",
      htmltools::tags$label(class = "form-label small text-muted", "On these days:"),
      shiny::checkboxGroupInput(
        ns("recurrence_days"),
        NULL,
        choiceNames = list(
          htmltools::span("S", class = "day-label"),
          htmltools::span("M", class = "day-label"),
          htmltools::span("T", class = "day-label"),
          htmltools::span("W", class = "day-label"),
          htmltools::span("T", class = "day-label"),
          htmltools::span("F", class = "day-label"),
          htmltools::span("S", class = "day-label")
        ),
        choiceValues = c("sunday", "monday", "tuesday", "wednesday", "thursday", "friday", "saturday"),
        selected = default_day,
        inline = TRUE
      )
    ),
    htmltools::tags$script(toggle_js)
  )
}

# Helper: Monthly recurrence options UI
monthly_options_ui <- function(ns, date) {
  # Calculate default values from selected date
  day_of_month <- if (!is.null(date)) lubridate::day(date) else 1
  week_num <- if (!is.null(date)) ceiling(lubridate::day(date) / 7) else 1
  weekday_name <- if (!is.null(date)) tolower(weekdays(date)) else "monday"

  week_labels <- c("1" = "first", "2" = "second", "3" = "third", "4" = "fourth", "-1" = "last")

  htmltools::div(
    shiny::radioButtons(
      ns("monthly_type"),
      NULL,
      choiceNames = list(
        htmltools::span(paste("On day", day_of_month)),
        htmltools::span(paste("On the", week_labels[as.character(week_num)], tools::toTitleCase(weekday_name)))
      ),
      choiceValues = c("day_of_month", "nth_weekday"),
      selected = "day_of_month"
    ),
    # Hidden inputs to store computed values
    htmltools::tags$input(
      type = "hidden",
      id = ns("month_day"),
      name = ns("month_day"),
      value = day_of_month
    ),
    htmltools::tags$input(
      type = "hidden",
      id = ns("week_num"),
      name = ns("week_num"),
      value = week_num
    ),
    htmltools::tags$input(
      type = "hidden",
      id = ns("weekday_name"),
      name = ns("weekday_name"),
      value = weekday_name
    )
  )
}
