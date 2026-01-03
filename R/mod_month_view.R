#' Month View Module UI
#'
#' @param id Module namespace ID
#'
#' @return A Shiny UI definition
#'
#' @keywords internal
mod_month_view_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::div(
    class = "month-view-container",

    # Navigation header
    htmltools::div(
      class = "month-nav",
      style = "display: flex; align-items: center; justify-content: space-between; flex-direction: row;",
      # Previous month button
      shiny::actionButton(
        ns("prev_month"),
        label = NULL,
        icon = bsicons::bs_icon("chevron-left"),
        class = "btn-nav btn-outline-secondary"
      ),

      # Current month display
      htmltools::div(
        class = "month-title-container",
        style = "flex: 1; text-align: center;",
        htmltools::h2(
          class = "month-title",
          style = "margin: 0; font-size: 1.5rem;",
          shiny::textOutput(ns("month_title"), inline = TRUE)
        )
      ),

      # Navigation buttons
      htmltools::div(
        class = "month-nav-buttons",
        style = "display: flex; gap: 0.5rem;",
        shiny::actionButton(
          ns("today"),
          "Today",
          class = "btn-today"
        ),
        shiny::actionButton(
          ns("next_month"),
          label = NULL,
          icon = bsicons::bs_icon("chevron-right"),
          class = "btn-nav btn-outline-secondary"
        )
      )
    ),

    # Month grid
    shiny::uiOutput(ns("month_grid"))
  )
}

#' Month View Module Server
#'
#' @param id Module namespace ID
#' @param events Reactive containing event data
#' @param selected_date Reactive value for selected date
#' @param calendars Reactive containing calendar list
#'
#' @keywords internal
mod_month_view_server <- function(id, events, selected_date, calendars) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Current month (first day of month)
    current_month <- shiny::reactive({
      date <- selected_date()
      as.Date(format(date, "%Y-%m-01"))
    })

    # Navigation
    shiny::observeEvent(input$prev_month, {
      # Go to previous month
      current <- current_month()
      prev <- seq(current, by = "-1 month", length.out = 2)[2]
      selected_date(prev)
    })

    shiny::observeEvent(input$next_month, {
      # Go to next month
      current <- current_month()
      next_month <- seq(current, by = "1 month", length.out = 2)[2]
      selected_date(next_month)
    })

    shiny::observeEvent(input$today, {
      selected_date(Sys.Date())
    })

    # Month title
    output$month_title <- shiny::renderText({
      format(current_month(), "%B %Y")
    })

    # Build month grid
    output$month_grid <- shiny::renderUI({
      tryCatch({
        month_start <- current_month()
        today <- Sys.Date()
        current_events <- tryCatch(events(), error = function(e) NULL)

        # Get all days in the month
        month_end <- seq(month_start, by = "1 month", length.out = 2)[2] - 1
        month_days <- seq(month_start, month_end, by = "day")

        # Get the day of week for the first day (0 = Sunday)
        first_dow <- as.numeric(format(month_start, "%w"))

        # Calculate padding days from previous month
        if (first_dow > 0) {
          prev_days <- seq(month_start - first_dow, month_start - 1, by = "day")
        } else {
          prev_days <- NULL
        }

        # Calculate padding days for next month (to complete the grid)
        total_cells <- length(prev_days) + length(month_days)
        remaining <- (7 - (total_cells %% 7)) %% 7
        if (remaining > 0) {
          next_days <- seq(month_end + 1, month_end + remaining, by = "day")
        } else {
          next_days <- NULL
        }

        # Combine all days
        all_days <- c(prev_days, month_days, next_days)

        # Group events by day with full info
        events_by_day <- list()
        if (!is.null(current_events) && nrow(current_events) > 0) {
          for (i in seq_len(nrow(current_events))) {
            event_date <- as.character(as.Date(current_events$start[i]))
            if (is.null(events_by_day[[event_date]])) {
              events_by_day[[event_date]] <- list()
            }
            events_by_day[[event_date]] <- c(events_by_day[[event_date]], list(list(
              title = current_events$title[i],
              color = current_events$color[i],
              start = current_events$start[i]
            )))
          }
        }

        # Build day cells
        day_cells <- lapply(all_days, function(date) {
          is_today <- date == today
          is_selected <- date == selected_date()
          is_current_month <- format(date, "%m") == format(month_start, "%m")
          is_weekend <- format(date, "%w") %in% c("0", "6")
          date_str <- as.character(date)
          day_num <- format(date, "%d")

          # Get events for this day
          day_events <- events_by_day[[date_str]]
          event_count <- if (!is.null(day_events)) length(day_events) else 0
          # Limit display to 3 events max
          display_events <- if (event_count > 0) day_events[1:min(3, event_count)] else list()

          htmltools::div(
            class = paste(
              "month-day-cell",
              if (is_today) "is-today",
              if (is_selected) "is-selected",
              if (!is_current_month) "other-month",
              if (is_weekend) "is-weekend"
            ),
            `data-date` = date_str,
            onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})", ns("day_clicked"), date_str),
            # Day number
            htmltools::div(
              class = "month-day-number",
              day_num
            ),
            # Event list (shown on larger screens)
            if (event_count > 0) {
              htmltools::div(
                class = "month-day-events",
                # Mini event cards with titles
                lapply(display_events, function(evt) {
                  time_str <- format(as.POSIXct(evt$start), "%l:%M %p")
                  time_str <- trimws(time_str)
                  htmltools::div(
                    class = "month-event-chip",
                    style = sprintf("background-color: %s;", evt$color),
                    htmltools::span(class = "month-event-time", time_str),
                    htmltools::span(class = "month-event-title", evt$title)
                  )
                }),
                # Show +N if more than 3 events
                if (event_count > 3) {
                  htmltools::div(
                    class = "month-event-more",
                    sprintf("+%d more", event_count - 3)
                  )
                }
              )
            }
          )
        })

        # Day of week headers
        dow_headers <- lapply(c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"), function(day) {
          htmltools::div(class = "month-dow-header", day)
        })

        # Wrap in grid container
        htmltools::div(
          class = "month-grid-wrapper",
          # Day of week header row
          htmltools::div(
            class = "month-dow-row",
            dow_headers
          ),
          # Calendar grid
          htmltools::div(
            class = "month-grid",
            day_cells
          )
        )
      }, error = function(e) {
        htmltools::div(
          class = "alert alert-danger",
          paste("Error rendering month view:", e$message)
        )
      })
    })

    # Handle day click - navigate to that day
    shiny::observeEvent(input$day_clicked, {
      clicked_date <- as.Date(input$day_clicked)
      selected_date(clicked_date)
      # Switch to day view using JavaScript (modules can't directly update parent navbar)
      shinyjs::runjs("$('a[data-value=\"day\"]').tab('show');")
    })
  })
}
