#' Day View Module UI
#'
#' @param id Module namespace ID
#'
#' @return A Shiny UI definition
#'
#' @keywords internal
mod_day_view_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_columns(
    col_widths = 12,
    fill = TRUE,
    fillable = TRUE,

    # Navigation header
    bslib::card(
      fill = FALSE,
      class = "day-nav-card mb-3",
      bslib::card_body(
        class = "d-flex justify-content-between align-items-center py-2",
        # Previous day button
        shiny::actionButton(
          ns("prev_day"),
          label = NULL,
          icon = bsicons::bs_icon("chevron-left"),
          class = "btn-outline-primary"
        ),
        # Current day display
        htmltools::div(
          class = "text-center",
          htmltools::h3(
            class = "mb-0 day-title",
            shiny::textOutput(ns("day_title"), inline = TRUE)
          ),
          htmltools::tags$small(
            class = "text-muted",
            shiny::textOutput(ns("day_subtitle"), inline = TRUE)
          )
        ),
        # Navigation buttons
        htmltools::div(
          shiny::actionButton(
            ns("today"),
            "Today",
            class = "btn-primary me-2"
          ),
          shiny::actionButton(
            ns("next_day"),
            label = NULL,
            icon = bsicons::bs_icon("chevron-right"),
            class = "btn-outline-primary"
          )
        )
      )
    ),

    # Day schedule (hourly view)
    bslib::card(
      fill = TRUE,
      fillable = TRUE,
      class = "day-schedule-card",
      bslib::card_body(
        fill = TRUE,
        fillable = TRUE,
        class = "p-0 day-schedule-container",
        shiny::uiOutput(ns("day_schedule"))
      )
    )
  )
}

#' Day View Module Server
#'
#' @param id Module namespace ID
#' @param events Reactive containing event data
#' @param selected_date Reactive value for selected date
#' @param calendars Reactive containing calendar list
#'
#' @keywords internal
mod_day_view_server <- function(id, events, selected_date, calendars) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Navigation
    shiny::observeEvent(input$prev_day, {
      selected_date(selected_date() - 1)
    })

    shiny::observeEvent(input$next_day, {
      selected_date(selected_date() + 1)
    })

    shiny::observeEvent(input$today, {
      selected_date(Sys.Date())
    })

    # Day title
    output$day_title <- shiny::renderText({
      format(selected_date(), "%A, %B %d")
    })

    # Day subtitle
    output$day_subtitle <- shiny::renderText({
      date <- selected_date()
      today <- Sys.Date()

      diff <- as.numeric(date - today)
      if (diff == 0) {
        "Today"
      } else if (diff == 1) {
        "Tomorrow"
      } else if (diff == -1) {
        "Yesterday"
      } else {
        format(date, "%Y")
      }
    })

    # Build hourly schedule
    output$day_schedule <- shiny::renderUI({
      date <- selected_date()
      current_events <- events()
      now <- Sys.time()
      current_hour <- as.numeric(format(now, "%H"))
      current_minute <- as.numeric(format(now, "%M"))
      is_today <- date == Sys.Date()

      # Filter events for this day
      day_events <- if (!is.null(current_events) && nrow(current_events) > 0) {
        current_events[as.Date(current_events$start) == date, ]
      } else {
        data.frame()
      }

      # Separate all-day events
      all_day_events <- if (nrow(day_events) > 0) {
        day_events[day_events$all_day, ]
      } else {
        data.frame()
      }

      timed_events <- if (nrow(day_events) > 0) {
        day_events[!day_events$all_day, ]
      } else {
        data.frame()
      }

      # Hours to display (6 AM to 10 PM)
      hours <- 6:22

      # Build hour rows
      hour_rows <- lapply(hours, function(hour) {
        hour_label <- format(
          as.POSIXct(sprintf("2024-01-01 %02d:00:00", hour)),
          "%l %p"
        )

        # Find events that overlap this hour
        hour_events <- if (nrow(timed_events) > 0) {
          event_hours <- as.numeric(format(as.POSIXct(timed_events$start), "%H"))
          timed_events[event_hours == hour, ]
        } else {
          data.frame()
        }

        # Current time indicator
        time_indicator <- if (is_today && hour == current_hour) {
          htmltools::div(
            class = "current-time-indicator",
            style = htmltools::css(top = paste0((current_minute / 60) * 100, "%"))
          )
        } else {
          NULL
        }

        htmltools::div(
          class = paste(
            "hour-row",
            if (is_today && hour == current_hour) "current-hour" else ""
          ),
          htmltools::div(class = "hour-label", hour_label),
          htmltools::div(
            class = "hour-content",
            time_indicator,
            if (nrow(hour_events) > 0) {
              lapply(seq_len(nrow(hour_events)), function(j) {
                event <- hour_events[j, ]
                event_card_detailed(event)
              })
            }
          )
        )
      })

      # All-day events section
      all_day_section <- if (nrow(all_day_events) > 0) {
        htmltools::div(
          class = "all-day-section",
          htmltools::div(class = "all-day-label", "All Day"),
          htmltools::div(
            class = "all-day-events",
            lapply(seq_len(nrow(all_day_events)), function(j) {
              event <- all_day_events[j, ]
              event_card_simple(event)
            })
          )
        )
      } else {
        NULL
      }

      # Combine
      htmltools::div(
        class = "day-schedule",
        all_day_section,
        htmltools::div(class = "hour-grid", hour_rows)
      )
    })
  })
}

#' Create Detailed Event Card
#'
#' @param event A single row from the events data frame
#'
#' @return HTML for a detailed event card
#'
#' @keywords internal
event_card_detailed <- function(event) {
  start_time <- format(as.POSIXct(event$start), "%l:%M %p")
  end_time <- format(as.POSIXct(event$end), "%l:%M %p")
  color <- event$color %||% "#74B9FF"

  htmltools::div(
    class = "event-card-detailed",
    style = htmltools::css(
      `border-left-color` = color,
      `background-color` = paste0(color, "15")
    ),
    htmltools::div(
      class = "event-header",
      htmltools::span(class = "event-title", event$title),
      htmltools::span(class = "event-time-range", paste(start_time, "-", end_time))
    ),
    if (!is.null(event$location) && !is.na(event$location) && nchar(event$location) > 0) {
      htmltools::div(
        class = "event-location",
        bsicons::bs_icon("geo-alt", size = "0.8em"),
        " ",
        event$location
      )
    },
    if (!is.null(event$description) && !is.na(event$description) && nchar(event$description) > 0) {
      htmltools::div(
        class = "event-description text-muted",
        substr(event$description, 1, 100),
        if (nchar(event$description) > 100) "..." else ""
      )
    }
  )
}

#' Create Simple Event Card
#'
#' @param event A single row from the events data frame
#'
#' @return HTML for a simple event card
#'
#' @keywords internal
event_card_simple <- function(event) {
  color <- event$color %||% "#74B9FF"

  htmltools::div(
    class = "event-card-simple",
    style = htmltools::css(
      `background-color` = color,
      color = "white"
    ),
    event$title
  )
}
