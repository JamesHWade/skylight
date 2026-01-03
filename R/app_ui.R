#' Main Application UI
#'
#' Constructs the main UI for the Skylight calendar app using bslib.
#'
#' @return A [bslib::page_navbar()] UI definition
#'
#' @keywords internal
app_ui <- function() {
 brand <- load_brand()

  bslib::page_navbar(
    id = "main_nav",
    title = htmltools::tags$span(
      class = "d-flex align-items-center",
      htmltools::tags$img(
        src = "www/logo.svg",
        height = "28px",
        class = "me-2"
      ),
      htmltools::tags$span(
        class = "brand-text",
        "Skylight"
      )
    ),
    theme = get_theme(),
    fillable = TRUE,
    navbar_options = bslib::navbar_options(
      bg = brand$color$primary %||% "#0077B6",
      collapsible = FALSE
    ),

    # Add external resources (CSS, JS, PWA)
    header = add_external_resources(),

    # Week View (default)
    bslib::nav_panel(
      title = "Week",
      value = "week",
      icon = bsicons::bs_icon("calendar-week"),
      mod_week_view_ui("week_view")
    ),

    # Day View
    bslib::nav_panel(
      title = "Day",
      value = "day",
      icon = bsicons::bs_icon("calendar-day"),
      mod_day_view_ui("day_view")
    ),

    # Agenda View
    bslib::nav_panel(
      title = "Agenda",
      value = "agenda",
      icon = bsicons::bs_icon("list-task"),
      mod_agenda_view_ui("agenda_view")
    ),

    # Spacer to push remaining items right
    bslib::nav_spacer(),

    # Clock widget in navbar
    bslib::nav_item(
      mod_clock_ui("clock")
    ),

    # Chat sidebar toggle
    bslib::nav_item(
      htmltools::tags$button(
        id = "toggle_chat",
        type = "button",
        class = "btn btn-outline-light btn-sm nav-btn",
        bsicons::bs_icon("chat-dots")
      )
    ),

    # Settings menu with dark mode inside
    bslib::nav_menu(
      title = NULL,
      icon = bsicons::bs_icon("gear"),
      align = "right",
      bslib::nav_item(
        mod_auth_ui("auth")
      ),
      "----",
      bslib::nav_item(
        htmltools::div(
          class = "d-flex align-items-center justify-content-between px-2 py-1",
          htmltools::span("Dark Mode"),
          bslib::input_dark_mode(id = "dark_mode", mode = "light")
        )
      ),
      "----",
      bslib::nav_item(
        shiny::actionButton(
          "refresh_calendar",
          "Refresh",
          icon = bsicons::bs_icon("arrow-clockwise"),
          class = "btn-sm btn-outline-secondary w-100"
        )
      )
    ),

    # Footer with chat panel (collapsible sidebar)
    footer = htmltools::div(
      id = "chat_container",
      class = "chat-sidebar collapsed",
      mod_chat_ui("chat")
    )
  )
}

#' Build App Theme from brand.yml
#'
#' Creates a bslib theme based on brand.yml configuration.
#'
#' @return A [bslib::bs_theme()] object
#'
#' @keywords internal
get_theme <- function() {
  brand <- load_brand()

 theme <- bslib::bs_theme(
    version = 5,
    preset = "shiny",

    # Colors from brand.yml
    primary = brand$color$primary %||% "#74B9FF",
    secondary = brand$color$secondary %||% "#FF7675",
    success = brand$color$success %||% "#55EFC4",
    warning = brand$color$warning %||% "#FFEAA7",
    danger = brand$color$danger %||% "#FF7675",
    info = brand$color$info %||% "#A29BFE",

    # Background and foreground
    bg = brand$color$background %||% "#FDF8F3",
    fg = brand$color$foreground %||% "#2D3436",

    # Typography
    base_font = bslib::font_google(
      brand$typography$base$family %||% "Inter"
    ),
    heading_font = bslib::font_google(
      brand$typography$headings$family %||% "DM Sans"
    ),
    font_scale = 1.1,

    # Calendar-specific customizations
    "card-border-radius" = "12px",
    "card-cap-bg" = "transparent",
    "navbar-padding-y" = "0.75rem"
  )

  # Add custom CSS rules if CSS file exists
  css_path <- app_sys("app/www/styles.css")
  if (file.exists(css_path)) {
    css_content <- paste(readLines(css_path), collapse = "\n")
    theme <- bslib::bs_add_rules(theme, css_content)
  }

  theme
}

#' Null-coalescing operator
#' @keywords internal
#' @noRd
`%||%` <- function(x, y) if (is.null(x)) y else x
