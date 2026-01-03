#' Main Application UI
#'
#' Constructs the main UI for the Skylight calendar app using bslib.
#'
#' @return A [bslib::page_navbar()] UI definition
#'
#' @keywords internal
app_ui <- function() {
  page_navbar(
    id = "main_nav",
    title = tags$span(
      tags$img(
        src = "www/logo.svg",
        height = "30px",
        class = "me-2",
        .noWS = "after"
      ),
      "Skylight"
    ),
    theme = get_theme(),
    fillable = TRUE,
    bg = "primary",

    # Add external resources (CSS, JS, PWA)
    header = add_external_resources(),

    # Week View (default)
    nav_panel(
      title = "Week",
      value = "week",
      icon = bsicons::bs_icon("calendar-week"),
      mod_week_view_ui("week_view")
    ),

    # Day View
    nav_panel(
      title = "Day",
      value = "day",
      icon = bsicons::bs_icon("calendar-day"),
      mod_day_view_ui("day_view")
    ),

    # Agenda View
    nav_panel(
      title = "Agenda",
      value = "agenda",
      icon = bsicons::bs_icon("list-task"),
      mod_agenda_view_ui("agenda_view")
    ),

    # Spacer to push remaining items right
    nav_spacer(),

    # Clock widget in navbar
    nav_item(
      mod_clock_ui("clock")
    ),

    # Chat sidebar toggle
    nav_item(
      actionButton(
        "toggle_chat",
        label = NULL,
        icon = bsicons::bs_icon("chat-dots"),
        class = "btn-outline-light"
      )
    ),

    # Settings menu
    nav_menu(
      title = "Settings",
      icon = bsicons::bs_icon("gear"),
      align = "right",
      nav_item(
        mod_auth_ui("auth")
      ),
      "----",
      nav_item(
        actionButton(
          "refresh_calendar",
          "Refresh Calendar",
          icon = bsicons::bs_icon("arrow-clockwise"),
          class = "btn-sm btn-outline-secondary w-100"
        )
      ),
      nav_item(
        actionButton(
          "toggle_dark_mode",
          "Toggle Dark Mode",
          icon = bsicons::bs_icon("moon-stars"),
          class = "btn-sm btn-outline-secondary w-100"
        )
      )
    ),

    # Footer with chat panel (collapsible sidebar)
    footer = div(
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

  bs_theme(
    version = 5,
    preset = "shiny",

    # Colors from brand.yml
    primary = brand$color$primary %||% "#74B9FF",
    secondary = brand$color$secondary %||% "#FF7675",
    success = brand$color$palette$mint %||% "#55EFC4",
    warning = brand$color$palette$sunshine %||% "#FFEAA7",
    danger = brand$color$palette$coral %||% "#FF7675",
    info = brand$color$palette$lavender %||% "#A29BFE",

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
  ) |>
    # Add custom CSS rules
    bslib::bs_add_rules(
      sass::sass_file(app_sys("app/www/styles.scss"))
    )
}

#' Null-coalescing operator
#' @keywords internal
#' @noRd
`%||%` <- function(x, y) if (is.null(x)) y else x
