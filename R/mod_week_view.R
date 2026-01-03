#' Week View Module UI
#'
#' @param id Module namespace ID
#'
#' @return A Shiny UI definition
#'
#' @keywords internal
mod_week_view_ui <- function(id) {
  ns <- NS(id)

  layout_columns(
    col_widths = 12,
    fill = TRUE,
    fillable = TRUE,

    # Navigation header
    card(
      fill = FALSE,
      class = "week-nav-card mb-3",
      card_body(
        class = "d-flex justify-content-between align-items-center py-2",
        # Previous week button
        actionButton(
          ns("prev_week"),
          label = NULL,
          icon = bsicons::bs_icon("chevron-left"),
          class = "btn-outline-primary"
        ),
        # Current week display
        div(
          class = "text-center",
          h4(
            class = "mb-0 week-title",
            textOutput(ns("week_title"), inline = TRUE)
          ),
          tags$small(
            class = "text-muted",
            textOutput(ns("week_range"), inline = TRUE)
          )
        ),
        # Navigation buttons
        div(
          actionButton(
            ns("today"),
            "Today",
            class = "btn-primary me-2"
          ),
          actionButton(
            ns("next_week"),
            label = NULL,
            icon = bsicons::bs_icon("chevron-right"),
            class = "btn-outline-primary"
          )
        )
      )
    ),

    # Week grid
    card(
      fill = TRUE,
      fillable = TRUE,
      class = "week-grid-card",
      card_body(
        fill = TRUE,
        fillable = TRUE,
        class = "p-0",
        uiOutput(ns("week_grid"))
      )
    )
  )
}

#' Week View Module Server
#'
#' @param id Module namespace ID
#' @param events Reactive containing event data
#' @param selected_date Reactive value for selected date
#' @param calendars Reactive containing calendar list
#'
#' @keywords internal
mod_week_view_server <- function(id, events, selected_date, calendars) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Week start date (Sunday)
    week_start <- reactive({
      date <- selected_date()
      date - as.numeric(format(date, "%w"))
    })

    # Week dates
    week_dates <- reactive({
      start <- week_start()
      seq(start, by = "day", length.out = 7)
    })

    # Navigation
    observeEvent(input$prev_week, {
      selected_date(selected_date() - 7)
    })

    observeEvent(input$next_week, {
      selected_date(selected_date() + 7)
    })

    observeEvent(input$today, {
      selected_date(Sys.Date())
    })

    # Week title
    output$week_title <- renderText({
      start <- week_start()
      if (format(start, "%Y") == format(Sys.Date(), "%Y")) {
        format(start, "%B %Y")
      } else {
        format(start, "%B %Y")
      }
    })

    # Week range
    output$week_range <- renderText({
      dates <- week_dates()
      start <- dates[1]
      end <- dates[7]

      if (format(start, "%b") == format(end, "%b")) {
        paste0(format(start, "%b %d"), " - ", format(end, "%d"))
      } else {
        paste0(format(start, "%b %d"), " - ", format(end, "%b %d"))
      }
    })

    # Build week grid
    output$week_grid <- renderUI({
      dates <- week_dates()
      today <- Sys.Date()
      current_events <- events()

      # Build day columns
      day_columns <- lapply(seq_along(dates), function(i) {
        date <- dates[i]
        is_today <- date == today
        day_name <- format(date, "%a")
        day_num <- format(date, "%d")

        # Filter events for this day
        day_events <- if (!is.null(current_events) && nrow(current_events) > 0) {
          current_events[as.Date(current_events$start) == date, ]
        } else {
          data.frame()
        }

        # Build day column
        div(
          class = paste(
            "day-column",
            if (is_today) "today" else "",
            if (i == 1 || i == 7) "weekend" else ""
          ),
          # Day header
          div(
            class = paste("day-header", if (is_today) "today" else ""),
            tags$span(class = "day-name", day_name),
            tags$span(
              class = paste("day-number", if (is_today) "today-badge" else ""),
              day_num
            )
          ),
          # Events container
          div(
            class = "day-events",
            if (nrow(day_events) > 0) {
              lapply(seq_len(nrow(day_events)), function(j) {
                event <- day_events[j, ]
                event_card(event)
              })
            } else {
              NULL
            }
          )
        )
      })

      # Wrap in grid container
      div(
        class = "week-grid",
        day_columns
      )
    })
  })
}

#' Create Event Card UI
#'
#' @param event A single row from the events data frame
#'
#' @return HTML for an event card
#'
#' @keywords internal
event_card <- function(event) {
  # Format time
  start_time <- if (!event$all_day) {
    format(as.POSIXct(event$start), "%l:%M %p")
  } else {
    "All day"
  }

  # Get color or default
  color <- event$color %||% "#74B9FF"

  div(
    class = "event-card",
    style = css(
      `border-left-color` = color,
      `background-color` = paste0(color, "20")
    ),
    div(
      class = "event-time",
      start_time
    ),
    div(
      class = "event-title",
      event$title
    ),
    if (!is.null(event$location) && nchar(event$location) > 0) {
      div(
        class = "event-location",
        bsicons::bs_icon("geo-alt", size = "0.75em"),
        " ",
        event$location
      )
    }
  )
}
