#' Countdown Event Utilities
#'
#' Functions for managing countdown events (special events to track).
#'
#' @name utils_countdown
#' @keywords internal
NULL

# =============================================================================
# CRUD OPERATIONS
# =============================================================================

#' Add Countdown Event
#'
#' Adds an event to the countdown tracker.
#'
#' @param title Event title to display.
#' @param target_date Date of the event (Date or character YYYY-MM-DD).
#' @param event_id Optional calendar event ID to link to.
#' @param emoji Emoji to display (default: party popper).
#' @param icon_base64 Optional base64-encoded custom icon.
#' @param color Color for the countdown display.
#'
#' @return The ID of the created countdown.
#'
#' @export
add_countdown <- function(title, target_date, event_id = NULL,
                          emoji = "\U0001F389", icon_base64 = NULL,
                          color = "#6366f1") {
 # Validate inputs
  if (is.null(title) || nchar(trimws(title)) == 0) {
    stop("title cannot be empty", call. = FALSE)
  }

  target_date <- as.Date(target_date)
  if (is.na(target_date)) {
    stop("target_date must be a valid date", call. = FALSE)
  }

  db_execute("
    INSERT INTO countdown_events (event_id, title, target_date, emoji, icon_base64, color)
    VALUES (?, ?, ?, ?, ?, ?)
    ON CONFLICT (event_id) DO UPDATE SET
      title = EXCLUDED.title,
      target_date = EXCLUDED.target_date,
      emoji = EXCLUDED.emoji,
      icon_base64 = EXCLUDED.icon_base64,
      color = EXCLUDED.color
  ", params = list(event_id, title, as.character(target_date), emoji, icon_base64, color))

  # Return the ID
  result <- db_query("
    SELECT id FROM countdown_events
    WHERE title = ? AND target_date = ?
    ORDER BY created_at DESC
    LIMIT 1
  ", params = list(title, as.character(target_date)))

  if (nrow(result) > 0) result$id[1] else NULL
}

#' Remove Countdown Event
#'
#' Removes an event from the countdown tracker.
#'
#' @param id The countdown ID to remove.
#'
#' @return TRUE on success.
#'
#' @export
remove_countdown <- function(id) {
  db_execute("DELETE FROM countdown_events WHERE id = ?", params = list(id))
  invisible(TRUE)
}

#' Remove Countdown by Event ID
#'
#' Removes a countdown linked to a calendar event.
#'
#' @param event_id The calendar event ID.
#'
#' @return TRUE on success.
#'
#' @export
remove_countdown_by_event <- function(event_id) {
  db_execute("DELETE FROM countdown_events WHERE event_id = ?",
             params = list(event_id))
  invisible(TRUE)
}

#' Get All Countdowns
#'
#' Returns all countdown events, optionally filtered.
#'
#' @param future_only If TRUE, only return events with target_date >= today.
#' @param limit Maximum number of countdowns to return.
#'
#' @return A data frame of countdown events.
#'
#' @export
get_countdowns <- function(future_only = TRUE, limit = 10) {
  if (future_only) {
    db_query("
      SELECT id, event_id, title, target_date, emoji, icon_base64, color, created_at
      FROM countdown_events
      WHERE target_date >= ?
      ORDER BY target_date ASC
      LIMIT ?
    ", params = list(as.character(Sys.Date()), as.integer(limit)))
  } else {
    db_query("
      SELECT id, event_id, title, target_date, emoji, icon_base64, color, created_at
      FROM countdown_events
      ORDER BY target_date ASC
      LIMIT ?
    ", params = list(as.integer(limit)))
  }
}

#' Get Next Countdown
#'
#' Returns the next upcoming countdown event.
#'
#' @return A single-row data frame, or NULL if none.
#'
#' @export
get_next_countdown <- function() {
  result <- db_query("
    SELECT id, event_id, title, target_date, emoji, icon_base64, color
    FROM countdown_events
    WHERE target_date >= ?
    ORDER BY target_date ASC
    LIMIT 1
  ", params = list(as.character(Sys.Date())))

  if (nrow(result) == 0) return(NULL)
  result[1, ]
}

#' Check if Event is Countdown
#'
#' Checks if a calendar event is marked as a countdown.
#'
#' @param event_id The calendar event ID.
#'
#' @return TRUE if the event is a countdown, FALSE otherwise.
#'
#' @export
is_countdown_event <- function(event_id) {
  result <- db_query("
    SELECT COUNT(*) as n FROM countdown_events WHERE event_id = ?
  ", params = list(event_id))

  result$n[1] > 0
}

#' Get Countdown by Event ID
#'
#' Returns the countdown for a specific calendar event.
#'
#' @param event_id The calendar event ID.
#'
#' @return A single-row data frame, or NULL if not found.
#'
#' @export
get_countdown_by_event <- function(event_id) {
  result <- db_query("
    SELECT id, event_id, title, target_date, emoji, icon_base64, color
    FROM countdown_events
    WHERE event_id = ?
  ", params = list(event_id))

  if (nrow(result) == 0) return(NULL)
  result[1, ]
}

# =============================================================================
# DISPLAY HELPERS
# =============================================================================

#' Calculate Days Until
#'
#' Calculates the number of days until a target date.
#'
#' @param target_date The target date.
#'
#' @return Integer number of days (negative if past).
#'
#' @export
days_until <- function(target_date) {
  as.integer(as.Date(target_date) - Sys.Date())
}

#' Format Countdown Text
#'
#' Formats a human-readable countdown string.
#'
#' @param days Number of days until the event.
#' @param title Event title.
#'
#' @return A formatted string like "5 days until Vacation".
#'
#' @export
format_countdown <- function(days, title) {
  if (days == 0) {
    paste0("Today: ", title, "!")
  } else if (days == 1) {
    paste0("Tomorrow: ", title, "!")
  } else if (days < 0) {
    paste0(abs(days), " days since ", title)
  } else if (days <= 7) {
    paste0(days, " days until ", title)
  } else if (days <= 30) {
    weeks <- floor(days / 7)
    if (weeks == 1) {
      paste0("1 week until ", title)
    } else {
      paste0(weeks, " weeks until ", title)
    }
  } else {
    paste0(days, " days until ", title)
  }
}

#' Get Countdown Summary
#'
#' Returns a summary for displaying in the widget.
#'
#' @return A list with next countdown info, or NULL.
#'
#' @export
get_countdown_summary <- function() {
  countdown <- get_next_countdown()
  if (is.null(countdown)) return(NULL)

  days <- days_until(countdown$target_date)

  list(
    id = countdown$id,
    title = countdown$title,
    target_date = countdown$target_date,
    days = days,
    emoji = countdown$emoji,
    icon_base64 = countdown$icon_base64,
    color = countdown$color,
    text = format_countdown(days, countdown$title)
  )
}
