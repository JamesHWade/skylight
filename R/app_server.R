#' Main Application Server
#'
#' The server-side logic for the Skylight calendar app.
#'
#' @param input,output,session Standard Shiny server arguments
#'
#' @keywords internal
app_server <- function(input, output, session) {
  # Demo mode detection
  demo_mode <- is_demo_mode()

  if (demo_mode) {
    shiny::showNotification(
      "Running in demo mode with sample events",
      type = "message",
      duration = 5
    )
  }

  # Core reactive values
  selected_date <- shiny::reactiveVal(Sys.Date())
  refresh_trigger <- shiny::reactiveVal(0)

  # Auth module
  auth_status <- mod_auth_server("auth")

  # Offline indicator module
  offline_status <- offline_indicator_server("offline")

  # Auto dark mode module
  auto_dark_settings <- mod_auto_dark_mode_server("auto_dark")

  # Events reactive with offline resilience
  events <- shiny::reactive({
    # React to manual refresh
    refresh_trigger()

    if (demo_mode) {
      # Demo mode uses sample events
      generate_sample_events()
    } else {
      # Authenticated mode with offline fallback
      result <- fetch_events_resilient(
        start = selected_date() - 7,
        end = selected_date() + 30
      )

      # Show notification if using cached data
      if (result$source == "cache_fallback") {
        shiny::showNotification(
          paste("Showing cached data.", result$error),
          type = "warning",
          duration = 5
        )
      }

      result$events
    }
  })

  # Calendars reactive with offline resilience
  calendars <- shiny::reactive({
    if (demo_mode) {
      generate_sample_calendars()
    } else {
      tryCatch({
        get_calendars()
      }, error = function(e) {
        # Return cached or empty calendars on error
        generate_sample_calendars()  # Fallback to demo calendars
      })
    }
  })

  # Handle manual refresh button
  shiny::observeEvent(input$refresh_calendar, {
    refresh_trigger(refresh_trigger() + 1)
    shiny::showNotification(
      "Refreshing calendar...",
      type = "message",
      duration = 2
    )
  })

  # View modules
  mod_week_view_server("week_view", events = events, selected_date = selected_date, calendars = calendars)
  mod_month_view_server("month_view", events = events, selected_date = selected_date, calendars = calendars)
  mod_day_view_server("day_view", events = events, selected_date = selected_date, calendars = calendars)
  mod_agenda_view_server("agenda_view", events = events, selected_date = selected_date)

  # Widget modules
  mod_clock_server("clock")
  mod_chat_server("chat", events = events, calendars = calendars, selected_date = selected_date)
}

#' Dark Mode Theme
#'
#' Creates a dark variant of the app theme.
#'
#' @return A [bslib::bs_theme()] object
#'
#' @keywords internal
get_dark_theme <- function() {
  brand <- load_brand()

  bslib::bs_theme(
    version = 5,
    preset = "shiny",

    # Dark mode colors
    primary = brand$color$primary %||% "#74B9FF",
    secondary = brand$color$secondary %||% "#FF7675",

    # Dark backgrounds
    bg = "#1a1a2e",
    fg = "#eaeaea",

    # Typography
    base_font = bslib::font_google(
      brand$typography$base$family %||% "Inter"
    ),
    heading_font = bslib::font_google(
      brand$typography$headings$family %||% "DM Sans"
    ),
    font_scale = 1.1,

    # Card styling
    "card-border-radius" = "12px",
    "card-cap-bg" = "transparent",
    "card-bg" = "#252542"
  )
}
