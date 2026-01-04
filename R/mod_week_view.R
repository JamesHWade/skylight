#' Week View Module UI
#'
#' @param id Module namespace ID
#'
#' @return A Shiny UI definition
#'
#' @keywords internal
mod_week_view_ui <- function(id) {
 ns <- shiny::NS(id)

  htmltools::div(
    class = "week-view-container",

    # Navigation header
    htmltools::div(
      class = "week-nav",
      style = "display: flex; align-items: center; justify-content: space-between; flex-direction: row;",
      # Previous week button
      shiny::actionButton(
        ns("prev_week"),
        label = NULL,
        icon = bsicons::bs_icon("chevron-left"),
        class = "btn-nav btn-outline-secondary"
      ),

      # Current week display
      htmltools::div(
        class = "week-title-container",
        style = "flex: 1; text-align: center;",
        htmltools::h2(
          class = "week-title",
          style = "margin: 0; font-size: 1.5rem;",
          shiny::textOutput(ns("week_title"), inline = TRUE)
        ),
        htmltools::div(
          class = "week-subtitle",
          shiny::textOutput(ns("week_range"), inline = TRUE)
        )
      ),

      # Navigation buttons
      htmltools::div(
        class = "week-nav-buttons",
        style = "display: flex; gap: 0.5rem;",
        shiny::actionButton(
          ns("today"),
          "Today",
          class = "btn-today"
        ),
        shiny::actionButton(
          ns("next_week"),
          label = NULL,
          icon = bsicons::bs_icon("chevron-right"),
          class = "btn-nav btn-outline-secondary"
        )
      )
    ),

    # Week grid
    shiny::uiOutput(ns("week_grid"))
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
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Week start date (Sunday)
    week_start <- shiny::reactive({
      date <- selected_date()
      date - as.numeric(format(date, "%w"))
    })

    # Week dates
    week_dates <- shiny::reactive({
      start <- week_start()
      seq(start, by = "day", length.out = 7)
    })

    # Navigation
    shiny::observeEvent(input$prev_week, {
      selected_date(selected_date() - 7)
    })

    shiny::observeEvent(input$next_week, {
      selected_date(selected_date() + 7)
    })

    shiny::observeEvent(input$today, {
      selected_date(Sys.Date())
    })

    # Week title
    output$week_title <- shiny::renderText({
      tryCatch({
        start <- week_start()
        format(start, "%B %Y")
      }, error = function(e) {
        paste("Error:", e$message)
      })
    })

    # Week range
    output$week_range <- shiny::renderText({
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
    output$week_grid <- shiny::renderUI({
      tryCatch({
        dates <- week_dates()
        today <- Sys.Date()
        current_events <- tryCatch(events(), error = function(e) NULL)

      # Build day columns
      day_columns <- lapply(seq_along(dates), function(i) {
        date <- dates[i]
        is_today <- date == today
        is_past <- date < today
        is_weekend <- i == 1 || i == 7
        day_name <- format(date, "%A")
        day_name_short <- format(date, "%a")
        day_num <- format(date, "%d")
        month_short <- format(date, "%b")

        # Check for holiday
        holiday_info <- get_holiday_info(date)

        # Filter events for this day and add conflict info
        day_events <- if (!is.null(current_events) && nrow(current_events) > 0) {
          de <- current_events[as.Date(current_events$start) == date, ]
          if (nrow(de) > 0) {
            add_conflict_info(de)
          } else {
            de
          }
        } else {
          data.frame()
        }

        # Build day column
        htmltools::div(
          class = paste(
            "day-column",
            if (is_today) "is-today",
            if (is_past) "is-past",
            if (is_weekend) "is-weekend",
            if (holiday_info$is_holiday) "is-holiday",
            holiday_info$class
          ),
          `data-date` = format(date, "%Y-%m-%d"),
          style = "display: flex; flex-direction: column; background: var(--bs-body-bg, #FDF8F3); min-height: 400px;",
          # Day header
          htmltools::div(
            class = "day-header",
            htmltools::span(class = "day-name-full", day_name),
            htmltools::span(class = "day-name-short", day_name_short),
            htmltools::div(
              class = paste("day-date", if (is_today) "today-badge"),
              htmltools::span(class = "day-num", day_num),
              if (as.numeric(day_num) == 1 || i == 1) {
                htmltools::span(class = "day-month", month_short)
              }
            ),
            if (holiday_info$is_holiday) {
              htmltools::div(class = "holiday-name", holiday_info$name)
            },
            # Quick add button (visible on hover)
            htmltools::tags$button(
              type = "button",
              class = "quick-add-btn",
              title = "Add event",
              bsicons::bs_icon("plus", size = "1rem")
            )
          ),
          # Events container
          htmltools::div(
            class = "day-body",
            if (nrow(day_events) > 0) {
              htmltools::div(
                class = "day-events",
                lapply(seq_len(nrow(day_events)), function(j) {
                  event <- day_events[j, ]
                  event_card(event)
                })
              )
            } else if (is_today) {
              htmltools::div(
                class = "day-empty today-empty",
                htmltools::span(class = "empty-dot"),
                htmltools::span("No events today")
              )
            }
          )
        )
      })

      # Wrap in grid container
      htmltools::div(
        class = "week-grid",
        style = "display: grid; grid-template-columns: repeat(7, 1fr); gap: 1px; min-height: 400px;",
        day_columns
      )
      }, error = function(e) {
        htmltools::div(
          class = "alert alert-danger",
          paste("Error rendering calendar:", e$message)
        )
      })
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
  start_time <- if (!isTRUE(event$all_day)) {
    trimws(format(as.POSIXct(event$start), "%l:%M %p"))
  } else {
    NULL
  }

  # Get color or default
  color <- if (!is.null(event$color) && !is.na(event$color) && nchar(event$color) > 0) {
    event$color
  } else {
    "#74B9FF"
  }

  # Check for conflicts
  has_conflict <- isTRUE(event$has_conflict)
  conflict_count <- if (!is.null(event$conflict_count)) event$conflict_count else 0

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
      recurring = isTRUE(event$recurring),
      has_conflict = has_conflict,
      conflict_count = conflict_count
    ),
    auto_unbox = TRUE
  )

  htmltools::div(
    class = paste(
      "event-card",
      if (isTRUE(event$all_day)) "all-day-event",
      if (has_conflict) "has-conflict"
    ),
    `data-event-id` = event$id,
    `data-event` = event_data,
    role = "button",
    tabindex = "0",
    style = htmltools::css(
      `--event-color` = color,
      cursor = "pointer"
    ),
    # Conflict indicator
    if (has_conflict) {
      htmltools::span(
        class = "conflict-indicator",
        title = paste0("Overlaps with ", conflict_count, " event", if (conflict_count > 1) "s"),
        bsicons::bs_icon("exclamation-triangle-fill", size = "0.7em")
      )
    },
    if (!is.null(start_time)) {
      htmltools::span(class = "event-time", start_time)
    },
    htmltools::span(
      class = "event-title",
      event$title,
      if (isTRUE(event$recurring)) {
        bsicons::bs_icon("arrow-repeat", class = "recurring-icon", size = "0.75em")
      }
    ),
    if (!is.null(event$location) && !is.na(event$location) && nchar(event$location) > 0) {
      htmltools::span(
        class = "event-location",
        bsicons::bs_icon("geo-alt", size = "0.7em"),
        event$location
      )
    }
  )
}
