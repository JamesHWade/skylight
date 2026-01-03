#' Agenda View Module UI
#'
#' @param id Module namespace ID
#'
#' @return A Shiny UI definition
#'
#' @keywords internal
mod_agenda_view_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_columns(
    col_widths = 12,
    fill = TRUE,
    fillable = TRUE,

    # Header
    bslib::card(
      fill = FALSE,
      class = "agenda-header-card mb-3",
      bslib::card_body(
        class = "d-flex justify-content-between align-items-center py-2",
        htmltools::h4(class = "mb-0", "Upcoming Events"),
        htmltools::div(
          shiny::actionButton(
            ns("today"),
            "Today",
            icon = bsicons::bs_icon("calendar-check"),
            class = "btn-outline-primary"
          )
        )
      )
    ),

    # Agenda list
    bslib::card(
      fill = TRUE,
      fillable = TRUE,
      class = "agenda-list-card",
      bslib::card_body(
        fill = TRUE,
        fillable = TRUE,
        class = "p-0 agenda-container",
        shiny::uiOutput(ns("agenda_list"))
      )
    )
  )
}

#' Agenda View Module Server
#'
#' @param id Module namespace ID
#' @param events Reactive containing event data
#' @param selected_date Reactive value for selected date
#'
#' @keywords internal
mod_agenda_view_server <- function(id, events, selected_date) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Jump to today
    shiny::observeEvent(input$today, {
      selected_date(Sys.Date())
    })

    # Build agenda list
    output$agenda_list <- shiny::renderUI({
      current_events <- events()
      start_date <- selected_date()
      today <- Sys.Date()

      if (is.null(current_events) || nrow(current_events) == 0) {
        return(
          htmltools::div(
            class = "agenda-empty text-center text-muted p-5",
            bsicons::bs_icon("calendar-x", size = "3em"),
            htmltools::h5(class = "mt-3", "No upcoming events"),
            htmltools::p("Your calendar is clear for the next two weeks.")
          )
        )
      }

      # Sort events by start time
      current_events <- current_events[order(current_events$start), ]

      # Group events by date
      current_events$date_group <- as.Date(current_events$start)
      unique_dates <- unique(current_events$date_group)

      # Build day sections
      day_sections <- lapply(unique_dates, function(date) {
        day_events <- current_events[current_events$date_group == date, ]

        # Format date header
        date_label <- if (date == today) {
          "Today"
        } else if (date == today + 1) {
          "Tomorrow"
        } else if (date < today + 7) {
          format(date, "%A")
        } else {
          format(date, "%A, %B %d")
        }

        # Relative date
        diff <- as.numeric(date - today)
        relative_label <- if (diff == 0) {
          ""
        } else if (diff == 1) {
          ""
        } else if (diff > 0) {
          paste0("in ", diff, " days")
        } else {
          paste0(abs(diff), " days ago")
        }

        htmltools::div(
          class = paste(
            "agenda-day-section",
            if (date == today) "today" else "",
            if (date < today) "past" else ""
          ),
          # Date header
          htmltools::div(
            class = "agenda-date-header",
            htmltools::span(class = "agenda-date-label", date_label),
            if (nchar(relative_label) > 0) {
              htmltools::span(class = "agenda-date-relative text-muted", relative_label)
            },
            htmltools::span(
              class = "agenda-date-full text-muted",
              format(date, "%B %d, %Y")
            )
          ),
          # Events for this day
          htmltools::div(
            class = "agenda-day-events",
            lapply(seq_len(nrow(day_events)), function(i) {
              agenda_event_card(day_events[i, ])
            })
          )
        )
      })

      htmltools::div(
        class = "agenda-list",
        day_sections
      )
    })
  })
}

#' Create Agenda Event Card
#'
#' @param event A single row from the events data frame
#'
#' @return HTML for an agenda event card
#'
#' @keywords internal
agenda_event_card <- function(event) {
  color <- event$color %||% "#74B9FF"

  # Format time
  time_display <- if (event$all_day) {
    htmltools::span(class = "event-time all-day", "All Day")
  } else {
    start_time <- format(as.POSIXct(event$start), "%l:%M %p")
    end_time <- format(as.POSIXct(event$end), "%l:%M %p")
    htmltools::span(
      class = "event-time",
      start_time, " - ", end_time
    )
  }

  # Store event data as JSON for the modal
  event_data <- jsonlite::toJSON(
    list(
      id = event$id,
      title = event$title,
      start = format(as.POSIXct(event$start), "%Y-%m-%dT%H:%M:%S"),
      end = format(as.POSIXct(event$end), "%Y-%m-%dT%H:%M:%S"),
      all_day = isTRUE(event$all_day),
      location = if (!is.na(event$location)) event$location else "",
      description = if (!is.na(event$description)) event$description else "",
      calendar_name = event$calendar_name,
      color = color,
      recurring = isTRUE(event$recurring)
    ),
    auto_unbox = TRUE
  )

  htmltools::div(
    class = "agenda-event-card",
    `data-event-id` = event$id,
    `data-event` = event_data,
    role = "button",
    tabindex = "0",
    style = "cursor: pointer;",
    # Color indicator
    htmltools::div(
      class = "agenda-event-color",
      style = htmltools::css(`background-color` = color)
    ),
    # Event content
    htmltools::div(
      class = "agenda-event-content",
      htmltools::div(
        class = "agenda-event-header",
        htmltools::span(
          class = "agenda-event-title",
          event$title,
          if (isTRUE(event$recurring)) {
            bsicons::bs_icon("arrow-repeat", class = "recurring-icon", size = "0.75em")
          }
        ),
        time_display
      ),
      if (!is.null(event$location) && !is.na(event$location) && nchar(event$location) > 0) {
        htmltools::div(
          class = "agenda-event-location text-muted",
          bsicons::bs_icon("geo-alt", size = "0.8em"),
          " ",
          event$location
        )
      },
      if (!is.null(event$calendar_name)) {
        htmltools::div(
          class = "agenda-event-calendar text-muted",
          bsicons::bs_icon("calendar3", size = "0.8em"),
          " ",
          event$calendar_name
        )
      }
    )
  )
}
