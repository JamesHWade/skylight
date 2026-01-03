#' Format Date Relative to Today
#'
#' Formats a date with natural language relative to today.
#'
#' @param date A Date object.
#' @param include_time Logical. Whether to include time if available.
#'
#' @return A character string.
#'
#' @keywords internal
format_relative_date <- function(date, include_time = FALSE) {
  today <- Sys.Date()
  date <- as.Date(date)
  diff <- as.numeric(date - today)

  label <- if (diff == 0) {
    "Today"
  } else if (diff == 1) {
    "Tomorrow"
  } else if (diff == -1) {
    "Yesterday"
  } else if (diff > 1 && diff < 7) {
    format(date, "%A")  # Day name
  } else if (diff >= 7 && diff < 14) {
    "Next week"
  } else {
    format(date, "%B %d")  # Month Day
  }

  label
}

#' Format Time for Display
#'
#' Formats a time for user-friendly display.
#'
#' @param datetime A POSIXct object.
#' @param format Time format: "12h" or "24h".
#'
#' @return A character string.
#'
#' @keywords internal
format_time <- function(datetime, format = "12h") {
  if (format == "24h") {
    format(datetime, "%H:%M")
  } else {
    trimws(format(datetime, "%l:%M %p"))
  }
}
#' Format Time Range
#'
#' Formats a start and end time as a range.
#'
#' @param start POSIXct start time.
#' @param end POSIXct end time.
#' @param format Time format: "12h" or "24h".
#'
#' @return A character string like "2:00 PM - 3:30 PM".
#'
#' @keywords internal
format_time_range <- function(start, end, format = "12h") {
  paste(
    format_time(start, format),
    "-",
    format_time(end, format)
  )
}

#' Format Duration
#'
#' Formats a duration in a human-readable way.
#'
#' @param start POSIXct start time.
#' @param end POSIXct end time.
#'
#' @return A character string like "1h 30m".
#'
#' @keywords internal
format_duration <- function(start, end) {
  diff_mins <- as.numeric(difftime(end, start, units = "mins"))

  if (diff_mins < 60) {
    paste0(round(diff_mins), "m")
  } else {
    hours <- floor(diff_mins / 60)
    mins <- round(diff_mins %% 60)
    if (mins == 0) {
      paste0(hours, "h")
    } else {
      paste0(hours, "h ", mins, "m")
    }
  }
}

#' Get Week Dates
#'
#' Returns all dates for the week containing the given date.
#'
#' @param date A Date object.
#' @param week_start Day the week starts: 0 = Sunday, 1 = Monday.
#'
#' @return A vector of 7 Date objects.
#'
#' @keywords internal
get_week_dates <- function(date, week_start = 0) {
  date <- as.Date(date)
  current_wday <- as.numeric(format(date, "%w"))  # 0 = Sunday

  # Calculate offset to week start
  offset <- (current_wday - week_start) %% 7
  start_date <- date - offset

  seq(start_date, by = "day", length.out = 7)
}

#' Get Month Dates
#'
#' Returns all dates for the calendar month grid containing the given date.
#'
#' @param date A Date object.
#' @param week_start Day the week starts: 0 = Sunday, 1 = Monday.
#'
#' @return A vector of Date objects (typically 35 or 42 days).
#'
#' @keywords internal
get_month_dates <- function(date, week_start = 0) {
  date <- as.Date(date)

  # First day of month
  first_of_month <- as.Date(format(date, "%Y-%m-01"))

  # Day of week for first of month
  first_wday <- as.numeric(format(first_of_month, "%w"))

  # Start date (may be in previous month)
  offset <- (first_wday - week_start) %% 7
  start_date <- first_of_month - offset

  # Last day of month
  last_of_month <- as.Date(format(
    seq(first_of_month, by = "month", length.out = 2)[2] - 1,
    "%Y-%m-%d"
  ))

  # Calculate number of weeks needed
  days_shown <- as.numeric(last_of_month - start_date) + 1
  weeks_needed <- ceiling(days_shown / 7)

  seq(start_date, by = "day", length.out = weeks_needed * 7)
}

#' Check if Date is Today
#'
#' @param date A Date object.
#'
#' @return Logical.
#'
#' @keywords internal
is_today <- function(date) {
  as.Date(date) == Sys.Date()
}

#' Check if Date is Weekend
#'
#' @param date A Date object.
#'
#' @return Logical.
#'
#' @keywords internal
is_weekend <- function(date) {
  wday <- as.numeric(format(as.Date(date), "%w"))
  wday == 0 || wday == 6
}

#' Check if Date is in Current Month
#'
#' @param date A Date object.
#' @param reference Reference date for comparison.
#'
#' @return Logical.
#'
#' @keywords internal
is_current_month <- function(date, reference = Sys.Date()) {
  format(as.Date(date), "%Y-%m") == format(as.Date(reference), "%Y-%m")
}

#' Parse Natural Language Date
#'
#' Attempts to parse natural language date references.
#'
#' @param text Character string with date reference.
#'
#' @return A Date object, or NULL if parsing fails.
#'
#' @keywords internal
parse_natural_date <- function(text) {
  text <- tolower(trimws(text))
  today <- Sys.Date()

  if (text %in% c("today", "now")) {
    return(today)
  }

  if (text == "tomorrow") {
    return(today + 1)
  }

  if (text == "yesterday") {
    return(today - 1)
  }

  # Day names (next occurrence)
  day_names <- c("sunday", "monday", "tuesday", "wednesday",
                 "thursday", "friday", "saturday")
  day_idx <- match(text, day_names)
  if (!is.na(day_idx)) {
    current_wday <- as.numeric(format(today, "%w")) + 1
    days_ahead <- (day_idx - current_wday) %% 7
    if (days_ahead == 0) days_ahead <- 7
    return(today + days_ahead)
  }

  # Try standard date parsing
  tryCatch({
    lubridate::parse_date_time(text, orders = c("mdy", "ymd", "dmy"))
  }, error = function(e) {
    NULL
  })
}
