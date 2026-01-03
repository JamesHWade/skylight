#' Get Available Calendars
#'
#' Retrieves a list of all calendars accessible by the authenticated user.
#'
#' @return A data frame with columns: `id`, `name`, `color`, `primary`.
#'
#' @export
#'
#' @examples
#' if (interactive() && is_authenticated()) {
#'   calendars <- get_calendars()
#'   print(calendars)
#' }
get_calendars <- function() {
  token <- get_token()
  if (is.null(token)) {
    stop("Not authenticated. Call calendar_auth() first.", call. = FALSE)
  }

  # Check cache first
  cached <- get_cached_calendars()
  if (!is.null(cached)) {
    return(cached)
  }

  # Fetch from API
  resp <- httr2::request("https://www.googleapis.com/calendar/v3/users/me/calendarList") |>
    httr2::req_auth_bearer_token(token$access_token) |>
    httr2::req_perform()

  data <- httr2::resp_body_json(resp)

  # Parse calendars
  calendars <- purrr::map_dfr(data$items, function(cal) {
    data.frame(
      id = cal$id %||% NA_character_,
      name = cal$summary %||% NA_character_,
      color = cal$backgroundColor %||% "#4285F4",
      primary = isTRUE(cal$primary),
      stringsAsFactors = FALSE
    )
  })

  # Cache the result
  cache_calendars(calendars)

  calendars
}

#' Get Calendar Events
#'
#' Fetches events from Google Calendar within a specified date range.
#'
#' @param start Date or character. Start of the date range. Defaults to today.
#' @param end Date or character. End of the date range. Defaults to 7 days from start.
#' @param calendars Character vector. Calendar IDs to fetch from.
#'   Defaults to all calendars.
#'
#' @return A data frame with event details.
#'
#' @export
#'
#' @examples
#' if (interactive() && is_authenticated()) {
#'   events <- get_events(
#'     start = Sys.Date(),
#'     end = Sys.Date() + 7
#'   )
#'   print(events)
#' }
get_events <- function(
    start = Sys.Date(),
    end = start + 7,
    calendars = NULL
) {
  token <- get_token()
  if (is.null(token)) {
    stop("Not authenticated. Call calendar_auth() first.", call. = FALSE)
  }

  # Convert dates to ISO 8601 format
  start <- as.Date(start)
  end <- as.Date(end)
  time_min <- format(as.POSIXct(start), "%Y-%m-%dT00:00:00Z")
  time_max <- format(as.POSIXct(end + 1), "%Y-%m-%dT00:00:00Z")

  # Get calendar list if not specified
  if (is.null(calendars)) {
    calendar_list <- get_calendars()
    calendars <- calendar_list$id
  }

  # Fetch events from each calendar
  all_events <- purrr::map_dfr(calendars, function(cal_id) {
    tryCatch({
      fetch_calendar_events(token, cal_id, time_min, time_max)
    }, error = function(e) {
      warning("Failed to fetch events from calendar: ", cal_id, " - ", e$message)
      data.frame()
    })
  })

  # Sort by start time
  if (nrow(all_events) > 0) {
    all_events <- all_events[order(all_events$start), ]

    # Cache events
    cache_events(all_events)
  }

  all_events
}

#' Fetch Events from a Single Calendar
#'
#' @param token OAuth token
#' @param calendar_id Calendar ID
#' @param time_min Start time (ISO 8601)
#' @param time_max End time (ISO 8601)
#'
#' @return Data frame of events
#'
#' @keywords internal
fetch_calendar_events <- function(token, calendar_id, time_min, time_max) {
  # URL encode the calendar ID
  encoded_id <- utils::URLencode(calendar_id, reserved = TRUE)

  url <- glue::glue(
    "https://www.googleapis.com/calendar/v3/calendars/{encoded_id}/events"
  )

  resp <- httr2::request(url) |>
    httr2::req_auth_bearer_token(token$access_token) |>
    httr2::req_url_query(
      timeMin = time_min,
      timeMax = time_max,
      singleEvents = "true",
      orderBy = "startTime",
      maxResults = 250
    ) |>
    httr2::req_perform()

  data <- httr2::resp_body_json(resp)

  if (length(data$items) == 0) {
    return(data.frame())
  }

  # Get calendar info for color
  calendar_info <- tryCatch(
    get_calendars()[get_calendars()$id == calendar_id, ],
    error = function(e) data.frame(name = "Unknown", color = "#4285F4")
  )

  cal_name <- if (nrow(calendar_info) > 0) calendar_info$name[1] else "Unknown"
  cal_color <- if (nrow(calendar_info) > 0) calendar_info$color[1] else "#4285F4"

  # Parse events
  purrr::map_dfr(data$items, function(event) {
    # Handle all-day vs timed events
    # Use [[ instead of $ to avoid partial matching (date vs dateTime)
    is_all_day <- !is.null(event$start[["date"]])

    start_time <- if (is_all_day) {
      lubridate::ymd(event$start[["date"]], tz = "UTC")
    } else {
      lubridate::ymd_hms(event$start[["dateTime"]])
    }

    end_time <- if (is_all_day) {
      lubridate::ymd(event$end[["date"]], tz = "UTC")
    } else {
      lubridate::ymd_hms(event$end[["dateTime"]])
    }

    data.frame(
      id = event$id %||% NA_character_,
      calendar_id = calendar_id,
      calendar_name = cal_name,
      title = event$summary %||% "(No title)",
      start = start_time,
      end = end_time,
      all_day = is_all_day,
      location = event$location %||% NA_character_,
      description = event$description %||% NA_character_,
      color = event$colorId %||% cal_color,
      recurring = !is.null(event$recurringEventId),
      status = event$status %||% "confirmed",
      stringsAsFactors = FALSE
    )
  })
}

#' Get Cached Events
#'
#' Retrieves events from the local DuckDB cache.
#'
#' @param start Date. Start of date range.
#' @param end Date. End of date range.
#'
#' @return Data frame of cached events, or NULL if not available.
#'
#' @keywords internal
get_cached_events <- function(start, end) {
  tryCatch({
    con <- db_connect()
    on.exit(DBI::dbDisconnect(con))

    # Check if events table exists
    if (!DBI::dbExistsTable(con, "events")) {
      return(NULL)
    }

    DBI::dbGetQuery(con, "
      SELECT * FROM events
      WHERE DATE(start) >= ? AND DATE(start) <= ?
      ORDER BY start
    ", params = list(as.character(start), as.character(end)))
  }, error = function(e) {
    NULL
  })
}

#' Cache Events
#'
#' Stores events in the local DuckDB cache.
#'
#' @param events Data frame of events
#'
#' @keywords internal
cache_events <- function(events) {
  if (is.null(events) || nrow(events) == 0) {
    return(invisible(NULL))
  }

  tryCatch({
    con <- db_connect()
    on.exit(DBI::dbDisconnect(con))

    # Upsert events (replace existing, insert new)
    DBI::dbWriteTable(
      con, "events", events,
      overwrite = FALSE,
      append = TRUE
    )
  }, error = function(e) {
    # Silently fail - caching is not critical
  })

  invisible(NULL)
}

#' Get Cached Calendars
#'
#' @return Data frame of cached calendars, or NULL.
#'
#' @keywords internal
get_cached_calendars <- function() {
  cache_key <- "calendars"
  cached <- pkg_env[[cache_key]]

  if (!is.null(cached)) {
    cache_time <- attr(cached, "cache_time")
    timeout <- getOption("skylight.cache_timeout", 300)

    if (!is.null(cache_time) && difftime(Sys.time(), cache_time, units = "secs") < timeout) {
      return(cached)
    }
  }

  NULL
}

#' Cache Calendars
#'
#' @param calendars Data frame of calendars
#'
#' @keywords internal
cache_calendars <- function(calendars) {
  attr(calendars, "cache_time") <- Sys.time()
  pkg_env[["calendars"]] <- calendars
  invisible(NULL)
}
