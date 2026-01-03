#' Auto Dark Mode Module UI
#'
#' Settings UI for automatic dark mode based on time of day.
#'
#' @param id Module namespace ID
#'
#' @return Shiny UI elements
#'
#' @keywords internal
mod_auto_dark_mode_ui <- function(id) {
 ns <- shiny::NS(id)

  htmltools::div(
    class = "auto-dark-mode-settings px-2 py-1",

    # Auto mode toggle
    htmltools::div(
      class = "d-flex align-items-center justify-content-between mb-2",
      htmltools::span("Auto Dark Mode"),
      shiny::checkboxInput(
        ns("auto_enabled"),
        label = NULL,
        value = FALSE,
        width = "auto"
      )
    ),

    # Time settings (shown only when auto is enabled)
    shiny::conditionalPanel(
      condition = sprintf("input['%s'] == true", ns("auto_enabled")),
      ns = ns,
      htmltools::div(
        class = "auto-dark-time-settings",
        htmltools::div(
          class = "d-flex align-items-center justify-content-between mb-1",
          htmltools::tags$small(class = "text-muted", "Dark after"),
          htmltools::tags$input(
            type = "time",
            id = ns("sunset_time"),
            class = "form-control form-control-sm",
            value = "18:00",
            style = "width: 100px;"
          )
        ),
        htmltools::div(
          class = "d-flex align-items-center justify-content-between",
          htmltools::tags$small(class = "text-muted", "Light after"),
          htmltools::tags$input(
            type = "time",
            id = ns("sunrise_time"),
            class = "form-control form-control-sm",
            value = "06:00",
            style = "width: 100px;"
          )
        )
      )
    )
  )
}

#' Auto Dark Mode Module Server
#'
#' Server logic for automatic dark mode based on time of day.
#'
#' @param id Module namespace ID
#' @param check_interval Interval in milliseconds to check time (default: 60000 = 1 min)
#'
#' @return Reactive with current auto dark mode settings
#'
#' @keywords internal
mod_auto_dark_mode_server <- function(id, check_interval = 60000) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Load saved settings on startup
    shiny::observe({
      settings <- load_auto_dark_settings()

      shiny::updateCheckboxInput(
        session, "auto_enabled",
        value = settings$enabled
      )

      # Update time inputs via JavaScript
      if (!is.null(settings$sunset_time)) {
        shinyjs::runjs(sprintf(
          "document.getElementById('%s').value = '%s';",
          ns("sunset_time"), settings$sunset_time
        ))
      }
      if (!is.null(settings$sunrise_time)) {
        shinyjs::runjs(sprintf(
          "document.getElementById('%s').value = '%s';",
          ns("sunrise_time"), settings$sunrise_time
        ))
      }
    }) |> shiny::bindEvent(TRUE, once = TRUE)

    # Save settings when changed
    shiny::observe({
      save_auto_dark_settings(
        enabled = input$auto_enabled,
        sunset_time = input$sunset_time,
        sunrise_time = input$sunrise_time
      )
    }) |> shiny::bindEvent(
      input$auto_enabled,
      input$sunset_time,
      input$sunrise_time,
      ignoreInit = TRUE
    )

    # Periodic time check for auto mode
    shiny::observe({
      shiny::invalidateLater(check_interval)

      if (isTRUE(input$auto_enabled)) {
        sunset <- input$sunset_time %||% "18:00"
        sunrise <- input$sunrise_time %||% "06:00"

        should_be_dark <- is_dark_time(sunrise, sunset)

        # Toggle theme via JavaScript
        mode <- if (should_be_dark) "dark" else "light"
        shinyjs::runjs(sprintf(
          "document.documentElement.setAttribute('data-bs-theme', '%s');",
          mode
        ))
      }
    })

    # Return current settings as reactive
    shiny::reactive({
      list(
        enabled = input$auto_enabled,
        sunset_time = input$sunset_time,
        sunrise_time = input$sunrise_time
      )
    })
  })
}

#' Check if Current Time is in Dark Period
#'
#' @param sunrise_time Character. Sunrise time in "HH:MM" format.
#' @param sunset_time Character. Sunset time in "HH:MM" format.
#'
#' @return Logical. TRUE if current time is in dark period.
#'
#' @keywords internal
is_dark_time <- function(sunrise_time, sunset_time) {
  current <- as.numeric(format(Sys.time(), "%H")) * 60 +
    as.numeric(format(Sys.time(), "%M"))

  sunrise <- parse_time_to_minutes(sunrise_time)
  sunset <- parse_time_to_minutes(sunset_time)

  # Dark if before sunrise or after sunset
  current < sunrise || current >= sunset
}

#' Parse Time String to Minutes Since Midnight
#'
#' @param time_str Character. Time in "HH:MM" format.
#'
#' @return Numeric. Minutes since midnight.
#'
#' @keywords internal
parse_time_to_minutes <- function(time_str) {
  parts <- strsplit(time_str, ":")[[1]]
  as.numeric(parts[1]) * 60 + as.numeric(parts[2])
}

#' Save Auto Dark Mode Settings
#'
#' @param enabled Logical. Whether auto mode is enabled.
#' @param sunset_time Character. Sunset time in "HH:MM" format.
#' @param sunrise_time Character. Sunrise time in "HH:MM" format.
#'
#' @keywords internal
save_auto_dark_settings <- function(enabled, sunset_time, sunrise_time) {
  settings <- list(
    enabled = enabled,
    sunset_time = sunset_time,
    sunrise_time = sunrise_time
  )

  tryCatch({
    db_save_setting("auto_dark_mode", settings)
  }, error = function(e) {
    # Silently fail - settings are not critical
  })

  invisible(settings)
}

#' Load Auto Dark Mode Settings
#'
#' @return List with auto dark mode settings.
#'
#' @keywords internal
load_auto_dark_settings <- function() {
  tryCatch({
    settings <- db_get_setting("auto_dark_mode", default = NULL)
    if (is.null(settings)) {
      list(enabled = FALSE, sunset_time = "18:00", sunrise_time = "06:00")
    } else {
      settings
    }
  }, error = function(e) {
    list(enabled = FALSE, sunset_time = "18:00", sunrise_time = "06:00")
  })
}
