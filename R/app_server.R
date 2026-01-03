#' Main Application Server
#'
#' The server-side logic for the Skylight calendar app.
#'
#' @param input,output,session Standard Shiny server arguments
#'
#' @keywords internal
app_server <- function(input, output, session) {
  # ──────────────────────────────────────────────────────────────────────────

# Reactive Values
  # ──────────────────────────────────────────────────────────────────────────

  # Current selected date (defaults to today)
  selected_date <- reactiveVal(Sys.Date())

  # Dark mode state
  dark_mode <- reactiveVal(FALSE)

  # Chat panel visibility
  chat_visible <- reactiveVal(FALSE)

  # ──────────────────────────────────────────────────────────────────────────
  # Authentication Module
  # ──────────────────────────────────────────────────────────────────────────

  auth_status <- mod_auth_server("auth")

  # ──────────────────────────────────────────────────────────────────────────
  # Calendar Data
  # ──────────────────────────────────────────────────────────────────────────

  # Refresh trigger (manual refresh button + timer)
  refresh_trigger <- reactiveVal(0)

  # Auto-refresh timer (every minute by default)
  observe({
    invalidateLater(getOption("skylight.refresh_interval", 60000))
    refresh_trigger(refresh_trigger() + 1)
  })

  # Manual refresh
  observeEvent(input$refresh_calendar, {
    refresh_trigger(refresh_trigger() + 1)
    showNotification("Calendar refreshed", type = "message", duration = 2)
  })

  # Fetch calendar list
  calendars <- reactive({
    req(auth_status())
    refresh_trigger()

    tryCatch(
      get_calendars(),
      error = function(e) {
        showNotification(
          paste("Failed to fetch calendars:", e$message),
          type = "error"
        )
        NULL
      }
    )
  }) |> bindCache(refresh_trigger())

  # Compute date range based on view and selected date
  date_range <- reactive({
    date <- selected_date()
    view <- input$main_nav %||% "week"

    switch(view,
      week = {
        # Get start of week (Sunday)
        start <- date - as.numeric(format(date, "%w"))
        list(start = start, end = start + 6)
      },
      day = {
        list(start = date, end = date)
      },
      agenda = {
        # Next 14 days for agenda
        list(start = date, end = date + 13)
      },
      # Default to week
      {
        start <- date - as.numeric(format(date, "%w"))
        list(start = start, end = start + 6)
      }
    )
  })

  # Fetch events for current date range
  events <- reactive({
    req(auth_status())
    range <- date_range()
    refresh_trigger()

    tryCatch(
      get_events(start = range$start, end = range$end),
      error = function(e) {
        showNotification(
          paste("Failed to fetch events:", e$message),
          type = "error"
        )
        # Return cached events if available
        get_cached_events(range$start, range$end)
      }
    )
  }) |> bindCache(date_range(), refresh_trigger())

  # ──────────────────────────────────────────────────────────────────────────
  # View Modules
  # ──────────────────────────────────────────────────────────────────────────

  mod_week_view_server(
    "week_view",
    events = events,
    selected_date = selected_date,
    calendars = calendars
  )

  mod_day_view_server(
    "day_view",
    events = events,
    selected_date = selected_date,
    calendars = calendars
  )

  mod_agenda_view_server(
    "agenda_view",
    events = events,
    selected_date = selected_date
  )

  # ──────────────────────────────────────────────────────────────────────────
  # Widget Modules
  # ──────────────────────────────────────────────────────────────────────────

  mod_clock_server("clock")

  mod_chat_server(
    "chat",
    events = events,
    calendars = calendars,
    selected_date = selected_date
  )

  # ──────────────────────────────────────────────────────────────────────────
  # UI Interactions
  # ──────────────────────────────────────────────────────────────────────────

  # Toggle chat sidebar
  observeEvent(input$toggle_chat, {
    chat_visible(!chat_visible())
    if (chat_visible()) {
      shinyjs::removeClass("chat_container", "collapsed")
      shinyjs::addClass("chat_container", "expanded")
    } else {
      shinyjs::removeClass("chat_container", "expanded")
      shinyjs::addClass("chat_container", "collapsed")
    }
  })

  # Toggle dark mode
  observeEvent(input$toggle_dark_mode, {
    dark_mode(!dark_mode())
    session$setCurrentTheme(
      if (dark_mode()) get_dark_theme() else get_theme()
    )
  })

  # Auto dark mode based on time
  observe({
    invalidateLater(60000) # Check every minute
    hour <- as.numeric(format(Sys.time(), "%H"))
    should_be_dark <- hour >= 20 || hour < 7

    if (should_be_dark != dark_mode()) {
      dark_mode(should_be_dark)
      session$setCurrentTheme(
        if (dark_mode()) get_dark_theme() else get_theme()
      )
    }
  })

  # ──────────────────────────────────────────────────────────────────────────
  # Session Cleanup
  # ──────────────────────────────────────────────────────────────────────────

  session$onSessionEnded(function() {
    # Cleanup any resources if needed
    message("Skylight session ended")
  })
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

  bs_theme(
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
