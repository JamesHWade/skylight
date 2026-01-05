#' Recurrence Rule Utilities
#'
#' Functions for parsing and evaluating flexible recurring schedules
#' (Outlook-style recurrence patterns).
#'
#' @name utils_recurrence
#' @keywords internal
NULL

#' Create Recurrence Rule
#'
#' Builds a recurrence rule JSON structure for flexible scheduling.
#'
#' @param type Recurrence type: 'daily', 'weekly', 'monthly'.
#' @param interval How often (e.g., every 2 weeks). Default: 1.
#' @param by_days For weekly: vector of day names (e.g., c("monday", "wednesday")).
#' @param by_weekday For weekly/monthly: TRUE to use weekday-based recurrence.
#' @param by_month_day For monthly: day of month (1-31).
#' @param by_week_num For monthly: which week (1=first, 2=second, ..., -1=last).
#' @param by_weekday_name For monthly weekday: day name (e.g., "tuesday").
#' @param end_type End condition: 'never', 'after', 'by_date'.
#' @param end_count For end_type='after': number of occurrences.
#' @param end_date For end_type='by_date': end date (Date or character).
#' @param start_date When the recurrence started (for tracking occurrences).
#'
#' @return A JSON string representing the recurrence rule.
#'
#' @export
create_recurrence_rule <- function(type = "daily",
                                    interval = 1,
                                    by_days = NULL,
                                    by_weekday = FALSE,
                                    by_month_day = NULL,
                                    by_week_num = NULL,
                                    by_weekday_name = NULL,
                                    end_type = "never",
                                    end_count = NULL,
                                    end_date = NULL,
                                    start_date = NULL) {
  # Validate type
  type <- match.arg(type, c("daily", "weekly", "monthly"))

  # Validate interval
  interval <- as.integer(interval)
  if (is.na(interval) || interval < 1) interval <- 1

  # Validate end_type
  end_type <- match.arg(end_type, c("never", "after", "by_date"))

  # Build rule

  rule <- list(
    type = type,
    interval = interval,
    end_type = end_type
  )

  # Add start date
  if (!is.null(start_date)) {
    rule$start_date <- as.character(as.Date(start_date))
  } else {
    rule$start_date <- as.character(Sys.Date())
  }

  # Type-specific options
  if (type == "daily") {
    if (isTRUE(by_weekday)) {
      rule$weekdays_only <- TRUE
    }
  } else if (type == "weekly") {
    if (!is.null(by_days) && length(by_days) > 0) {
      # Normalize day names to lowercase
      rule$by_days <- tolower(by_days)
    }
  } else if (type == "monthly") {
    if (!is.null(by_month_day)) {
      # Day of month (e.g., 15th of every month)
      rule$by_month_day <- as.integer(by_month_day)
    } else if (!is.null(by_week_num) && !is.null(by_weekday_name)) {
      # Nth weekday (e.g., 2nd Tuesday)
      rule$by_week_num <- as.integer(by_week_num)
      rule$by_weekday_name <- tolower(by_weekday_name)
    }
  }

  # End conditions
  if (end_type == "after" && !is.null(end_count)) {
    rule$end_count <- as.integer(end_count)
  } else if (end_type == "by_date" && !is.null(end_date)) {
    rule$end_date <- as.character(as.Date(end_date))
  }

  jsonlite::toJSON(rule, auto_unbox = TRUE)
}

#' Parse Recurrence Rule
#'
#' Parses a JSON recurrence rule string into a list.
#'
#' @param rule_json JSON string or NULL.
#'
#' @return A list with rule parameters, or NULL if invalid/empty.
#'
#' @export
parse_recurrence_rule <- function(rule_json) {
  if (is.null(rule_json) || is.na(rule_json) || nchar(rule_json) == 0) {
    return(NULL)
  }

  tryCatch({
    jsonlite::fromJSON(rule_json, simplifyVector = FALSE)
  }, error = function(e) {
    warning("Failed to parse recurrence rule: ", conditionMessage(e))
    NULL
  })
}

#' Check if Date Matches Recurrence Rule
#'
#' Determines if a given date falls on a scheduled occurrence.
#'
#' @param date The date to check.
#' @param rule Parsed recurrence rule (list) or JSON string.
#' @param occurrence_count Optional: current occurrence count for 'after' end type.
#'
#' @return TRUE if the date matches the pattern, FALSE otherwise.
#'
#' @export
matches_recurrence <- function(date, rule, occurrence_count = NULL) {
  # Parse if JSON string
  if (is.character(rule)) {
    rule <- parse_recurrence_rule(rule)
  }

  if (is.null(rule)) return(FALSE)

  date <- as.Date(date)

  # Check end conditions first
  if (!check_recurrence_active(date, rule, occurrence_count)) {
    return(FALSE)
  }

  # Get start date
  start_date <- if (!is.null(rule$start_date)) {
    as.Date(rule$start_date)
  } else {
    as.Date("1970-01-01")  # No start constraint
  }

  # Can't match before start
  if (date < start_date) return(FALSE)

  # Check based on type
  type <- rule$type %||% "daily"
  interval <- rule$interval %||% 1

  switch(type,
    "daily" = matches_daily(date, rule, start_date, interval),
    "weekly" = matches_weekly(date, rule, start_date, interval),
    "monthly" = matches_monthly(date, rule, start_date, interval),
    FALSE
  )
}

#' Check if Recurrence is Still Active
#'
#' @param date The date to check.
#' @param rule Parsed recurrence rule.
#' @param occurrence_count Current occurrence count.
#'
#' @return TRUE if recurrence is still active, FALSE if ended.
#'
#' @keywords internal
check_recurrence_active <- function(date, rule, occurrence_count = NULL) {
  end_type <- rule$end_type %||% "never"

  if (end_type == "never") {
    return(TRUE)
  } else if (end_type == "by_date" && !is.null(rule$end_date)) {
    end_date <- as.Date(rule$end_date)
    return(date <= end_date)
  } else if (end_type == "after" && !is.null(rule$end_count)) {
    # If we have occurrence count, check against it
    if (!is.null(occurrence_count)) {
      return(occurrence_count < rule$end_count)
    }
    # Otherwise, we'd need to calculate occurrences - return TRUE for now
    return(TRUE)
  }

  TRUE
}

#' Check Daily Recurrence Match
#'
#' @keywords internal
matches_daily <- function(date, rule, start_date, interval) {
  # Weekdays only check
  if (isTRUE(rule$weekdays_only)) {
    day_of_week <- as.integer(format(date, "%u"))  # 1=Mon, 7=Sun
    if (day_of_week >= 6) return(FALSE)  # Saturday or Sunday
  }

  # Check interval (days since start)
  days_since <- as.integer(date - start_date)
  if (days_since < 0) return(FALSE)

  # For weekdays-only, we need to count weekdays not calendar days
  if (isTRUE(rule$weekdays_only)) {
    # Calculate weekdays between start and date
    weekdays_since <- count_weekdays(start_date, date)
    return((weekdays_since %% interval) == 0)
  }

  (days_since %% interval) == 0
}

#' Count Weekdays Between Two Dates
#'
#' @keywords internal
count_weekdays <- function(from, to) {
  if (from > to) return(0)
  if (from == to) return(1)

  dates <- seq(from, to, by = "day")
  weekdays <- sapply(dates, function(d) {
    dow <- as.integer(format(d, "%u"))
    dow < 6  # Mon-Fri
  })
  sum(weekdays)
}

#' Check Weekly Recurrence Match
#'
#' @keywords internal
matches_weekly <- function(date, rule, start_date, interval) {
  # Check if correct week (based on interval)
  weeks_since <- as.integer(difftime(date, start_date, units = "weeks"))
  if (weeks_since < 0) return(FALSE)

  # Week interval check
  if ((weeks_since %% interval) != 0) return(FALSE)

  # Check day of week
  day_name <- tolower(format(date, "%A"))
  by_days <- rule$by_days

  if (is.null(by_days) || length(by_days) == 0) {
    # No specific days = match same day as start
    start_day <- tolower(format(start_date, "%A"))
    return(day_name == start_day)
  }

  day_name %in% by_days
}

#' Check Monthly Recurrence Match
#'
#' @keywords internal
matches_monthly <- function(date, rule, start_date, interval) {
  # Calculate months since start
  start_year <- as.integer(format(start_date, "%Y"))
  start_month <- as.integer(format(start_date, "%m"))
  date_year <- as.integer(format(date, "%Y"))
  date_month <- as.integer(format(date, "%m"))

  months_since <- (date_year - start_year) * 12 + (date_month - start_month)
  if (months_since < 0) return(FALSE)

  # Month interval check
  if ((months_since %% interval) != 0) return(FALSE)

  # Check which pattern: day of month or nth weekday
  if (!is.null(rule$by_month_day)) {
    # Day of month (e.g., 15th)
    day_of_month <- as.integer(format(date, "%d"))
    return(day_of_month == rule$by_month_day)
  } else if (!is.null(rule$by_week_num) && !is.null(rule$by_weekday_name)) {
    # Nth weekday (e.g., 2nd Tuesday)
    return(matches_nth_weekday(date, rule$by_week_num, rule$by_weekday_name))
  } else {
    # Default: same day of month as start
    start_day <- as.integer(format(start_date, "%d"))
    date_day <- as.integer(format(date, "%d"))
    return(date_day == start_day)
  }
}

#' Check if Date is Nth Weekday of Month
#'
#' @param date The date to check.
#' @param week_num Which week (1-5 or -1 for last).
#' @param weekday_name Day name (e.g., "tuesday").
#'
#' @return TRUE if date is the nth weekday of its month.
#'
#' @keywords internal
matches_nth_weekday <- function(date, week_num, weekday_name) {
  # Check day name matches
  date_day_name <- tolower(format(date, "%A"))
  if (date_day_name != tolower(weekday_name)) return(FALSE)

  day_of_month <- as.integer(format(date, "%d"))

  if (week_num == -1) {
    # Last occurrence of this weekday
    # Check if there's no occurrence of this weekday later in the month
    next_week <- date + 7
    next_month <- as.integer(format(next_week, "%m"))
    current_month <- as.integer(format(date, "%m"))
    return(next_month != current_month)
  } else {
    # Nth occurrence (1st, 2nd, 3rd, 4th)
    # Day falls in week N if: (N-1)*7 < day <= N*7
    expected_min <- (week_num - 1) * 7 + 1
    expected_max <- week_num * 7
    return(day_of_month >= expected_min && day_of_month <= expected_max)
  }
}

#' Format Recurrence Rule for Display
#'
#' Creates a human-readable description of a recurrence rule.
#'
#' @param rule Parsed recurrence rule or JSON string.
#'
#' @return Human-readable string describing the recurrence.
#'
#' @export
format_recurrence <- function(rule) {
  if (is.character(rule)) {
    rule <- parse_recurrence_rule(rule)
  }

  if (is.null(rule)) return("No recurrence")

  type <- rule$type %||% "daily"
  interval <- rule$interval %||% 1

  # Build description
  desc <- switch(type,
    "daily" = {
      if (isTRUE(rule$weekdays_only)) {
        if (interval == 1) "Every weekday" else paste("Every", interval, "weekdays")
      } else {
        if (interval == 1) "Daily" else paste("Every", interval, "days")
      }
    },
    "weekly" = {
      days <- rule$by_days
      interval_text <- if (interval == 1) "Weekly" else paste("Every", interval, "weeks")
      if (!is.null(days) && length(days) > 0) {
        # Capitalize first letter of each day
        day_names <- sapply(days, function(d) {
          paste0(toupper(substr(d, 1, 1)), substr(d, 2, 3))
        })
        paste(interval_text, "on", paste(day_names, collapse = ", "))
      } else {
        interval_text
      }
    },
    "monthly" = {
      interval_text <- if (interval == 1) "Monthly" else paste("Every", interval, "months")
      if (!is.null(rule$by_month_day)) {
        paste(interval_text, "on day", rule$by_month_day)
      } else if (!is.null(rule$by_week_num) && !is.null(rule$by_weekday_name)) {
        ordinal <- switch(as.character(rule$by_week_num),
          "1" = "1st",
          "2" = "2nd",
          "3" = "3rd",
          "4" = "4th",
          "-1" = "last",
          paste0(rule$by_week_num, "th")
        )
        day_name <- paste0(toupper(substr(rule$by_weekday_name, 1, 1)),
                          substr(rule$by_weekday_name, 2, nchar(rule$by_weekday_name)))
        paste(interval_text, "on the", ordinal, day_name)
      } else {
        interval_text
      }
    },
    "Unknown"
  )

  # Add end condition
  end_type <- rule$end_type %||% "never"
  if (end_type == "after" && !is.null(rule$end_count)) {
    desc <- paste0(desc, " (", rule$end_count, " times)")
  } else if (end_type == "by_date" && !is.null(rule$end_date)) {
    desc <- paste0(desc, " until ", format(as.Date(rule$end_date), "%b %d, %Y"))
  }

  desc
}

#' Convert Recurrence Rule to RRULE
#'
#' Converts a recurrence rule JSON to iCalendar RRULE format for Google Calendar.
#'
#' @param rule Parsed recurrence rule (list) or JSON string.
#'
#' @return A character string in RRULE format, or NULL if invalid.
#'
#' @export
recurrence_to_rrule <- function(rule) {
  # Parse if JSON string
  if (is.character(rule)) {
    rule <- parse_recurrence_rule(rule)
  }

  if (is.null(rule)) return(NULL)

  type <- rule$type %||% "daily"
  interval <- rule$interval %||% 1

  # Map to RRULE FREQ
  freq <- switch(type,
    "daily" = "DAILY",
    "weekly" = "WEEKLY",
    "monthly" = "MONTHLY",
    NULL
  )

  if (is.null(freq)) return(NULL)

  # Build RRULE parts
  parts <- paste0("FREQ=", freq)

  # Add interval if not 1

  if (interval > 1) {
    parts <- paste0(parts, ";INTERVAL=", interval)
  }

  # Type-specific options
  if (type == "daily" && isTRUE(rule$weekdays_only)) {
    # Weekdays only = BYDAY=MO,TU,WE,TH,FR
    parts <- paste0(parts, ";BYDAY=MO,TU,WE,TH,FR")
  } else if (type == "weekly" && !is.null(rule$by_days) && length(rule$by_days) > 0) {
    # Convert day names to RRULE format (MO, TU, WE, etc.)
    day_map <- c(
      "monday" = "MO", "tuesday" = "TU", "wednesday" = "WE",
      "thursday" = "TH", "friday" = "FR", "saturday" = "SA", "sunday" = "SU"
    )
    rrule_days <- sapply(tolower(rule$by_days), function(d) day_map[d])
    rrule_days <- rrule_days[!is.na(rrule_days)]
    if (length(rrule_days) > 0) {
      parts <- paste0(parts, ";BYDAY=", paste(rrule_days, collapse = ","))
    }
  } else if (type == "monthly") {
    if (!is.null(rule$by_month_day)) {
      # Day of month (e.g., 15th)
      parts <- paste0(parts, ";BYMONTHDAY=", rule$by_month_day)
    } else if (!is.null(rule$by_week_num) && !is.null(rule$by_weekday_name)) {
      # Nth weekday (e.g., 2nd Tuesday = 2TU, last Friday = -1FR)
      day_map <- c(
        "monday" = "MO", "tuesday" = "TU", "wednesday" = "WE",
        "thursday" = "TH", "friday" = "FR", "saturday" = "SA", "sunday" = "SU"
      )
      day_code <- day_map[tolower(rule$by_weekday_name)]
      if (!is.na(day_code)) {
        parts <- paste0(parts, ";BYDAY=", rule$by_week_num, day_code)
      }
    }
  }

  # End conditions
  end_type <- rule$end_type %||% "never"
  if (end_type == "after" && !is.null(rule$end_count)) {
    parts <- paste0(parts, ";COUNT=", rule$end_count)
  } else if (end_type == "by_date" && !is.null(rule$end_date)) {
    # UNTIL format: YYYYMMDD or YYYYMMDDTHHMMSSZ
    until_date <- format(as.Date(rule$end_date), "%Y%m%d")
    parts <- paste0(parts, ";UNTIL=", until_date)
  }

  paste0("RRULE:", parts)
}
