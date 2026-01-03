#' Generate Sample Events for Demo Mode
#'
#' Creates realistic sample calendar events for testing and demonstration.
#'
#' @param start Date. Start of the date range.
#' @param end Date. End of the date range.
#'
#' @return A data frame of sample events matching the structure from Google Calendar API.
#'
#' @keywords internal
generate_sample_events <- function(start = Sys.Date() - 3, end = Sys.Date() + 10) {

# Sample event templates
  templates <- list(
    list(
      title = "Team Standup",
      duration_mins = 30,
      color = "#4285F4",
      calendar_name = "Work",
      recurring = TRUE
    ),
    list(
      title = "1:1 with Manager",
      duration_mins = 30,
      color = "#0B8043",
      calendar_name = "Work",
      location = "Zoom"
    ),
    list(
      title = "Project Planning",
      duration_mins = 60,
      color = "#4285F4",
      calendar_name = "Work",
      location = "Conference Room A"
    ),
    list(
      title = "Lunch with Sarah",
      duration_mins = 60,
      color = "#F4511E",
      calendar_name = "Personal",
      location = "Cafe Luna"
    ),
    list(
      title = "Code Review",
      duration_mins = 45,
      color = "#7986CB",
      calendar_name = "Work"
    ),
    list(
      title = "Dentist Appointment",
      duration_mins = 60,
      color = "#E67C73",
      calendar_name = "Personal",
      location = "Downtown Dental"
    ),
    list(
      title = "Team Retrospective",
      duration_mins = 90,
      color = "#4285F4",
      calendar_name = "Work",
      location = "Main Conference Room"
    ),
    list(
      title = "Yoga Class",
      duration_mins = 60,
      color = "#33B679",
      calendar_name = "Personal",
      location = "Fitness Center"
    ),
    list(
      title = "Product Demo",
      duration_mins = 45,
      color = "#039BE5",
      calendar_name = "Work",
      location = "Zoom"
    ),
    list(
      title = "Coffee Chat",
      duration_mins = 30,
      color = "#F6BF26",
      calendar_name = "Personal"
    ),
    list(
      title = "Sprint Review",
      duration_mins = 60,
      color = "#4285F4",
      calendar_name = "Work"
    ),
    list(
      title = "Doctor's Appointment",
      duration_mins = 45,
      color = "#E67C73",
      calendar_name = "Personal",
      location = "Medical Center"
    ),
    list(
      title = "Design Review",
      duration_mins = 60,
      color = "#7986CB",
      calendar_name = "Work"
    ),
    list(
      title = "Grocery Shopping",
      duration_mins = 45,
      color = "#33B679",
      calendar_name = "Personal",
      all_day = FALSE
    ),
    list(
      title = "Birthday Party",
      duration_mins = 180,
      color = "#D50000",
      calendar_name = "Personal",
      location = "123 Main St"
    )
  )

  # All-day event templates
  all_day_templates <- list(
    list(
      title = "Company Holiday",
      color = "#4285F4",
      calendar_name = "Work"
    ),
    list(
      title = "Vacation Day",
      color = "#33B679",
      calendar_name = "Personal"
    ),
    list(
      title = "Conference",
      color = "#039BE5",
      calendar_name = "Work"
    ),
    list(
      title = "Mom's Birthday",
      color = "#D50000",
      calendar_name = "Personal"
    )
  )

  events <- list()
  event_id <- 1

  # Generate dates in range
  dates <- seq(as.Date(start), as.Date(end), by = "day")

  for (date in dates) {
    date <- as.Date(date, origin = "1970-01-01")
    day_of_week <- as.numeric(format(date, "%u"))  # 1 = Monday, 7 = Sunday

    # Skip some weekend days randomly
    if (day_of_week >= 6 && stats::runif(1) > 0.3) {
      next
    }

    # Determine number of events for this day (weighted toward weekdays)
    if (day_of_week <= 5) {
      n_events <- sample(1:4, 1, prob = c(0.2, 0.4, 0.3, 0.1))
    } else {
      n_events <- sample(0:2, 1, prob = c(0.3, 0.5, 0.2))
    }

    # Add all-day event occasionally
    if (stats::runif(1) > 0.85) {
      template <- all_day_templates[[sample(length(all_day_templates), 1)]]
      events[[length(events) + 1]] <- data.frame(
        id = paste0("demo_", event_id),
        calendar_id = paste0("demo_", tolower(template$calendar_name)),
        calendar_name = template$calendar_name,
        title = template$title,
        start = as.POSIXct(paste(date, "00:00:00")),
        end = as.POSIXct(paste(date + 1, "00:00:00")),
        all_day = TRUE,
        location = NA_character_,
        description = NA_character_,
        color = template$color,
        recurring = FALSE,
        status = "confirmed",
        stringsAsFactors = FALSE
      )
      event_id <- event_id + 1
    }

    # Add timed events
    if (n_events > 0) {
      # Generate reasonable start times (8 AM to 6 PM, weighted toward work hours)
      possible_hours <- if (day_of_week <= 5) {
        c(8, 9, 9, 10, 10, 11, 13, 14, 14, 15, 15, 16, 17)
      } else {
        c(9, 10, 11, 12, 14, 15, 16, 17, 18, 19)
      }

      start_hours <- sort(sample(possible_hours, min(n_events, length(possible_hours))))

      for (hour in start_hours) {
        template <- templates[[sample(length(templates), 1)]]

        # Add some minute variation
        start_min <- sample(c(0, 0, 0, 15, 30, 30), 1)
        start_time <- as.POSIXct(
          paste(date, sprintf("%02d:%02d:00", hour, start_min))
        )
        end_time <- start_time + template$duration_mins * 60

        events[[length(events) + 1]] <- data.frame(
          id = paste0("demo_", event_id),
          calendar_id = paste0("demo_", tolower(template$calendar_name)),
          calendar_name = template$calendar_name,
          title = template$title,
          start = start_time,
          end = end_time,
          all_day = FALSE,
          location = template$location %||% NA_character_,
          description = NA_character_,
          color = template$color,
          recurring = isTRUE(template$recurring),
          status = "confirmed",
          stringsAsFactors = FALSE
        )
        event_id <- event_id + 1
      }
    }
  }

  if (length(events) == 0) {
    return(data.frame(
      id = character(),
      calendar_id = character(),
      calendar_name = character(),
      title = character(),
      start = as.POSIXct(character()),
      end = as.POSIXct(character()),
      all_day = logical(),
      location = character(),
      description = character(),
      color = character(),
      recurring = logical(),
      status = character(),
      stringsAsFactors = FALSE
    ))
  }

  result <- do.call(rbind, events)
  result <- result[order(result$start), ]
  rownames(result) <- NULL
  result
}

#' Generate Sample Calendars for Demo Mode
#'
#' @return A data frame of sample calendars.
#'
#' @keywords internal
generate_sample_calendars <- function() {
  data.frame(
    id = c("demo_work", "demo_personal"),
    name = c("Work", "Personal"),
    color = c("#4285F4", "#33B679"),
    primary = c(TRUE, FALSE),
    stringsAsFactors = FALSE
  )
}

#' Check if Running in Demo Mode
#'
#' Demo mode is enabled when Google OAuth credentials are not configured.
#'
#' @return Logical. TRUE if in demo mode.
#'
#' @keywords internal
is_demo_mode <- function() {
  client_id <- Sys.getenv("GOOGLE_CLIENT_ID")
  client_secret <- Sys.getenv("GOOGLE_CLIENT_SECRET")
  nchar(client_id) == 0 || nchar(client_secret) == 0
}
