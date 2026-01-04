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

    # Listen for quick add trigger from JavaScript
    shiny::observeEvent(input$quick_add_trigger, {
      trigger_data <- input$quick_add_trigger
      if (!is.null(trigger_data) && !is.null(trigger_data$date)) {
        selected_date_for_event(as.Date(trigger_data$date))
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

            # Event title
            shiny::textInput(
              ns("event_title"),
              "Event Title",
              placeholder = "Enter event title...",
              width = "100%"
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

      # Create the event
      result <- create_event(
        title = title,
        start = start,
        end = end,
        calendar_id = input$calendar,
        description = if (nchar(trimws(input$description)) > 0) input$description else NULL,
        location = if (nchar(trimws(input$location)) > 0) input$location else NULL,
        all_day = all_day
      )

      if (result$success) {
        shiny::removeModal()
        shiny::showNotification(
          paste("Created:", title),
          type = "message",
          duration = 3
        )
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
