#' Offline Resilience Utilities
#'
#' Functions for handling offline scenarios and graceful degradation.
#'
#' @name utils_offline
#' @keywords internal
NULL

#' Package Environment for Offline State
#'
#' @keywords internal
offline_env <- new.env(parent = emptyenv())
offline_env$is_offline <- FALSE
offline_env$last_online <- Sys.time()
offline_env$offline_since <- NULL

#' Check Network Connectivity
#'
#' Attempts to verify network connectivity by making a lightweight request.
#'
#' @param timeout Timeout in seconds for the check.
#'
#' @return Logical. TRUE if online, FALSE if offline.
#'
#' @keywords internal
check_connectivity <- function(timeout = 5) {
  tryCatch({
    # Try to reach Google's connectivity check endpoint
    resp <- httr2::request("https://www.google.com/generate_204") |>
      httr2::req_timeout(timeout) |>
      httr2::req_perform()

    status <- httr2::resp_status(resp)
    is_online <- status == 204 || status == 200

    # Update offline state
    if (is_online) {
      set_online_status(TRUE)
    }

    is_online
  }, error = function(e) {
    set_online_status(FALSE)
    FALSE
  })
}

#' Set Online/Offline Status
#'
#' Updates the cached connectivity status.
#'
#' @param is_online Logical. TRUE if online.
#'
#' @keywords internal
set_online_status <- function(is_online) {
  if (is_online) {
    offline_env$is_offline <- FALSE
    offline_env$last_online <- Sys.time()
    offline_env$offline_since <- NULL
  } else {
    if (!offline_env$is_offline) {
      # Just went offline
      offline_env$offline_since <- Sys.time()
    }
    offline_env$is_offline <- TRUE
  }
  invisible(is_online)
}

#' Get Current Offline Status
#'
#' Returns the current offline status without making a network request.
#'
#' @return A list with offline status information.
#'
#' @keywords internal
get_offline_status <- function() {
  list(
    is_offline = offline_env$is_offline,
    last_online = offline_env$last_online,
    offline_since = offline_env$offline_since,
    offline_duration = if (!is.null(offline_env$offline_since)) {
      difftime(Sys.time(), offline_env$offline_since, units = "mins")
    } else {
      NULL
    }
  )
}

#' Fetch Events with Offline Fallback
#'
#' Attempts to fetch events from the API, falling back to cached data if offline.
#'
#' @param start Date. Start of date range.
#' @param end Date. End of date range.
#' @param calendars Character vector. Calendar IDs to fetch.
#' @param force_refresh Logical. If TRUE, bypass cache and fetch from API.
#'
#' @return A list with events data frame and source indicator.
#'
#' @keywords internal
fetch_events_resilient <- function(start = Sys.Date(),
                                   end = start + 7,
                                   calendars = NULL,
                                   force_refresh = FALSE) {

  # First, try to get cached events

  cached_events <- get_cached_events(start, end)
  has_cache <- !is.null(cached_events) && nrow(cached_events) > 0


  # If not forcing refresh and we have recent cache, use it
  if (!force_refresh && has_cache) {
    cache_age <- get_cache_age()
    if (!is.null(cache_age) && cache_age < 5) {  # Less than 5 minutes old
      return(list(
        events = cached_events,
        source = "cache",
        is_offline = FALSE,
        cache_age_mins = cache_age
      ))
    }
  }

  # Try to fetch from API
  api_result <- tryCatch({
    events <- get_events(start = start, end = end, calendars = calendars)
    set_online_status(TRUE)

    # Update cache timestamp
    db_save_setting("events_cache_time", as.character(Sys.time()))

    list(
      events = events,
      source = "api",
      is_offline = FALSE,
      cache_age_mins = 0
    )
  }, error = function(e) {
    # API failed - check if it's a network error
    is_network_error <- grepl(
      "Could not resolve host|Connection refused|Timeout|SSL|network",
      e$message,
      ignore.case = TRUE
    )

    if (is_network_error) {
      set_online_status(FALSE)
    }

    # Fall back to cache if available
    if (has_cache) {
      cache_age <- get_cache_age()
      list(
        events = cached_events,
        source = "cache_fallback",
        is_offline = TRUE,
        cache_age_mins = cache_age,
        error = e$message
      )
    } else {
      # No cache available - fall back to demo events
      list(
        events = generate_sample_events(start, end),
        source = "demo_fallback",
        is_offline = TRUE,
        cache_age_mins = NULL,
        error = e$message
      )
    }
  })

  api_result
}

#' Get Cache Age in Minutes
#'
#' @return Numeric. Cache age in minutes, or NULL if no cache.
#'
#' @keywords internal
get_cache_age <- function() {
  cache_time_str <- db_get_setting("events_cache_time", default = NULL)

  if (is.null(cache_time_str)) {
    return(NULL)
  }

  tryCatch({
    cache_time <- as.POSIXct(cache_time_str)
    as.numeric(difftime(Sys.time(), cache_time, units = "mins"))
  }, error = function(e) {
    NULL
  })
}

#' Format Offline Status Message
#'
#' Creates a user-friendly message about offline status.
#'
#' @param status List from get_offline_status().
#' @param cache_age Numeric. Cache age in minutes.
#'
#' @return Character string with status message.
#'
#' @keywords internal
format_offline_message <- function(status, cache_age = NULL) {
  if (!status$is_offline) {
    return(NULL)
  }

  msg <- "You appear to be offline."

  if (!is.null(cache_age)) {
    if (cache_age < 60) {
      msg <- paste0(msg, " Showing cached data from ", round(cache_age), " minutes ago.")
    } else {
      hours <- round(cache_age / 60, 1)
      msg <- paste0(msg, " Showing cached data from ", hours, " hours ago.")
    }
  } else {
    msg <- paste0(msg, " No cached data available.")
  }

  msg
}

#' Create Offline Indicator UI
#'
#' Returns UI elements for displaying offline status.
#'
#' @param id Module namespace ID.
#'
#' @return Shiny UI elements.
#'
#' @keywords internal
offline_indicator_ui <- function(id = "offline") {
  ns <- shiny::NS(id)

 htmltools::div(
    id = ns("offline_banner"),
    class = "offline-banner",
    style = "display: none;",
    htmltools::div(
      class = "offline-banner-content",
      bsicons::bs_icon("wifi-off"),
      htmltools::span(
        id = ns("offline_message"),
        class = "offline-message",
        "You are offline. Showing cached data."
      ),
      htmltools::tags$button(
        id = ns("retry_connection"),
        class = "btn btn-sm btn-outline-light ms-2",
        onclick = sprintf("Shiny.setInputValue('%s', Date.now())", ns("retry")),
        "Retry"
      )
    )
  )
}

#' Offline Indicator Server
#'
#' Server logic for the offline indicator module.
#'
#' @param id Module namespace ID.
#' @param check_interval Interval in milliseconds to check connectivity.
#'
#' @return A reactive with current offline status.
#'
#' @keywords internal
offline_indicator_server <- function(id = "offline", check_interval = 30000) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive for offline status
    offline_status <- shiny::reactiveVal(list(is_offline = FALSE))

    # Periodic connectivity check
    shiny::observe({
      shiny::invalidateLater(check_interval)

      # Only check if we think we're offline, or periodically
      current <- offline_status()
      if (current$is_offline || runif(1) < 0.1) {  # 10% chance when online
        is_online <- check_connectivity(timeout = 3)
        offline_status(get_offline_status())
      }
    })

    # Handle retry button
    shiny::observeEvent(input$retry, {
      is_online <- check_connectivity(timeout = 5)
      offline_status(get_offline_status())

      if (is_online) {
        shiny::showNotification(
          "Connection restored!",
          type = "message",
          duration = 3
        )
      } else {
        shiny::showNotification(
          "Still offline. Will retry automatically.",
          type = "warning",
          duration = 3
        )
      }
    })

    # Update UI based on status
    shiny::observe({
      status <- offline_status()

      if (status$is_offline) {
        cache_age <- get_cache_age()
        message <- format_offline_message(status, cache_age)

        shinyjs::show("offline_banner")
        shinyjs::html("offline_message", message)
      } else {
        shinyjs::hide("offline_banner")
      }
    })

    # Return status reactive for other modules to use
    offline_status
  })
}
