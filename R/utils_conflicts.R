#' Calendar Conflict Detection Utilities
#'
#' Functions for detecting and handling overlapping calendar events.
#'
#' @name utils_conflicts
#' @keywords internal
NULL

#' Detect Conflicting Events
#'
#' Finds events that overlap with each other within a set of events.
#'
#' @param events A data frame of events with `start` and `end` columns.
#'
#' @return A list where each event ID maps to a vector of conflicting event IDs.
#'
#' @export
detect_conflicts <- function(events) {
 if (is.null(events) || nrow(events) == 0) {
    return(list())
  }

  # Filter out all-day events (they don't conflict in the same way)
  timed_events <- events[!isTRUE(events$all_day) & !is.na(events$all_day) & events$all_day == FALSE, ]

  if (nrow(timed_events) < 2) {
    return(list())
  }

  conflicts <- list()

  for (i in seq_len(nrow(timed_events))) {
    event_i <- timed_events[i, ]
    start_i <- as.POSIXct(event_i$start)
    end_i <- as.POSIXct(event_i$end)
    id_i <- event_i$id

    conflicting_ids <- character(0)

    for (j in seq_len(nrow(timed_events))) {
      if (i == j) next

      event_j <- timed_events[j, ]
      start_j <- as.POSIXct(event_j$start)
      end_j <- as.POSIXct(event_j$end)
      id_j <- event_j$id

      # Check for overlap: events overlap if one starts before the other ends
      if (start_i < end_j && start_j < end_i) {
        conflicting_ids <- c(conflicting_ids, id_j)
      }
    }

    if (length(conflicting_ids) > 0) {
      conflicts[[id_i]] <- conflicting_ids
    }
  }

  conflicts
}

#' Get Events with Conflict Info
#'
#' Adds conflict information to a data frame of events.
#'
#' @param events A data frame of events.
#'
#' @return The events data frame with additional columns:
#'   - `has_conflict`: TRUE if event overlaps with another
#'   - `conflict_count`: Number of overlapping events
#'   - `conflict_ids`: Character string of conflicting event IDs
#'
#' @export
add_conflict_info <- function(events) {
  if (is.null(events) || nrow(events) == 0) {
    return(events)
  }

  conflicts <- detect_conflicts(events)

  events$has_conflict <- sapply(events$id, function(id) {
    id %in% names(conflicts)
  })

  events$conflict_count <- sapply(events$id, function(id) {
    if (id %in% names(conflicts)) {
      length(conflicts[[id]])
    } else {
      0L
    }
  })

  events$conflict_ids <- sapply(events$id, function(id) {
    if (id %in% names(conflicts)) {
      paste(conflicts[[id]], collapse = ",")
    } else {
      ""
    }
  })

  events
}

#' Check if Two Events Conflict
#'
#' Checks if two specific events overlap in time.
#'
#' @param event1 First event (data frame row or list with start, end).
#' @param event2 Second event (data frame row or list with start, end).
#'
#' @return TRUE if events overlap, FALSE otherwise.
#'
#' @export
events_conflict <- function(event1, event2) {
  # All-day events don't conflict with timed events
  if (isTRUE(event1$all_day) || isTRUE(event2$all_day)) {
    return(FALSE)
  }

  start1 <- as.POSIXct(event1$start)
  end1 <- as.POSIXct(event1$end)
  start2 <- as.POSIXct(event2$start)
  end2 <- as.POSIXct(event2$end)

  # Events overlap if one starts before the other ends
  start1 < end2 && start2 < end1
}

#' Get Conflict Summary for a Day
#'
#' Returns a summary of conflicts for events on a specific date.
#'
#' @param events A data frame of events.
#' @param date The date to check.
#'
#' @return A list with:
#'   - `has_conflicts`: TRUE if any conflicts exist
#'   - `conflict_count`: Number of conflicting event pairs
#'   - `conflicting_events`: Vector of event IDs that have conflicts
#'
#' @export
get_day_conflict_summary <- function(events, date) {
  if (is.null(events) || nrow(events) == 0) {
    return(list(
      has_conflicts = FALSE,
      conflict_count = 0L,
      conflicting_events = character(0)
    ))
  }

  # Filter to events on this date
  day_events <- events[as.Date(events$start) == date, ]

  if (nrow(day_events) < 2) {
    return(list(
      has_conflicts = FALSE,
      conflict_count = 0L,
      conflicting_events = character(0)
    ))
  }

  conflicts <- detect_conflicts(day_events)

  list(
    has_conflicts = length(conflicts) > 0,
    conflict_count = length(conflicts),
    conflicting_events = names(conflicts)
  )
}

#' Format Conflict Warning Message
#'
#' Creates a user-friendly warning message about conflicts.
#'
#' @param event The event with conflicts.
#' @param conflict_count Number of conflicting events.
#'
#' @return A formatted warning string.
#'
#' @export
format_conflict_warning <- function(event, conflict_count) {
  if (conflict_count == 1) {
    paste0("\"", event$title, "\" overlaps with 1 other event")
  } else {
    paste0("\"", event$title, "\" overlaps with ", conflict_count, " other events")
  }
}
