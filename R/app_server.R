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

  # Auth module
  auth_status <- mod_auth_server("auth")

  # Simple event/calendar reactives for demo mode
  events <- shiny::reactive({
    generate_sample_events()
  })

  calendars <- shiny::reactive({
    generate_sample_calendars()
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
