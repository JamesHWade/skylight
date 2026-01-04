#' Run the Skylight Calendar Application
#'
#' Launch the Shiny application for the family calendar display.
#'
#' @param host Character. The host address to bind to. Defaults to "127.0.0.1"
#'   for local development. Use "0.0.0.0" for network access.
#' @param port Integer. The port number to run on. Defaults to 8080.
#' @param launch_browser Logical. Whether to open the app in a browser.
#'   Defaults to `TRUE` in interactive sessions.
#' @param ... Additional arguments passed to [shiny::runApp()].
#'
#' @return This function does not return; it runs the Shiny application.
#'
#' @examples
#' if (interactive()) {
#'   run_app()
#' }
#'
#' @export
run_app <- function(
    host = getOption("shiny.host", "127.0.0.1"),
    port = getOption("shiny.port", 8080L),
    launch_browser = interactive(),
    ...
) {
  # Suppress bslib color contrast warnings (informational only)
  options(bslib.color_contrast_warnings = FALSE)

  # Initialize database on startup
  db_init()

  # Build the Shiny app object
  app <- shinyApp(
    ui = app_ui(),
    server = app_server,
    onStart = function() {
      message("Skylight Calendar starting...")
      # Set package options
      options(
        skylight.cache_timeout = getOption("skylight.cache_timeout", 300),
        skylight.refresh_interval = getOption("skylight.refresh_interval", 60000)
      )
    }
  )

  # Run the app
  runApp(
    app,
    host = host,
    port = port,
    launch.browser = launch_browser,
    ...
  )
}

#' Get Path to Package Files
#'
#' Helper function to access files in the installed package.
#'
#' @param ... Path components relative to `inst/`
#'
#' @return Character string with the full path
#'
#' @keywords internal
app_sys <- function(...) {
  system.file(..., package = "skylight")
}

#' Add External Resources
#'
#' Add CSS, JavaScript, and other static resources to the app.
#'
#' @return A [htmltools::tagList()] with resource tags
#'
#' @keywords internal
add_external_resources <- function() {
  www_path <- app_sys("app/www")

  # Add resource path for www directory (must be done outside tagList)
  if (dir.exists(www_path)) {
    shiny::addResourcePath("www", www_path)
  }

  htmltools::tagList(
    # Initialize shinyjs
    shinyjs::useShinyjs(),

    # Add custom CSS if it exists
    if (file.exists(file.path(www_path, "styles.css"))) {
      htmltools::tags$head(
        htmltools::tags$link(rel = "stylesheet", type = "text/css", href = "www/styles.css")
      )
    },
    # Add custom JS if it exists
    if (file.exists(file.path(www_path, "custom.js"))) {
      htmltools::tags$head(
        htmltools::tags$script(src = "www/custom.js")
      )
    },
    # PWA manifest and meta tags
    htmltools::tags$head(
      htmltools::tags$link(rel = "manifest", href = "www/manifest.json"),
      htmltools::tags$meta(name = "theme-color", content = "#74B9FF"),
      htmltools::tags$meta(name = "apple-mobile-web-app-capable", content = "yes"),
      htmltools::tags$meta(name = "apple-mobile-web-app-status-bar-style", content = "default"),
      htmltools::tags$meta(name = "apple-mobile-web-app-title", content = "Skylight"),
      htmltools::tags$link(rel = "apple-touch-icon", href = "www/icon-192.png")
    ),
    # Viewport for mobile
    htmltools::tags$head(
      htmltools::tags$meta(
        name = "viewport",
        content = "width=device-width, initial-scale=1, maximum-scale=1, user-scalable=no"
      )
    ),

    # Event Details Modal
    htmltools::tags$div(
      id = "event-detail-modal",
      class = "modal fade",
      tabindex = "-1",
      `aria-labelledby` = "event-modal-label",
      `aria-hidden` = "true",
      htmltools::tags$div(
        class = "modal-dialog modal-dialog-centered",
        htmltools::tags$div(
          class = "modal-content",
          # Modal Header
          htmltools::tags$div(
            class = "modal-header",
            htmltools::tags$div(
              class = "event-modal-color",
              style = "width: 4px; height: 100%; border-radius: 2px; margin-right: 12px;"
            ),
            htmltools::tags$div(
              class = "flex-grow-1",
              htmltools::tags$h5(
                class = "modal-title event-modal-title",
                id = "event-modal-label",
                "Event Title"
              ),
              htmltools::tags$span(
                class = "event-modal-calendar badge bg-secondary",
                "Calendar"
              )
            ),
            htmltools::tags$button(
              type = "button",
              class = "btn-close",
              `data-bs-dismiss` = "modal",
              `aria-label` = "Close"
            )
          ),
          # Modal Body
          htmltools::tags$div(
            class = "modal-body",
            # Date & Time
            htmltools::tags$div(
              class = "event-modal-row mb-3",
              htmltools::tags$div(
                class = "event-modal-icon",
                bsicons::bs_icon("calendar-event")
              ),
              htmltools::tags$div(
                class = "event-modal-content",
                htmltools::tags$div(class = "event-modal-date fw-medium", "Date"),
                htmltools::tags$div(class = "event-modal-time text-muted", "Time")
              )
            ),
            # Location (conditionally shown)
            htmltools::tags$div(
              class = "event-modal-row event-modal-location-row mb-3",
              htmltools::tags$div(
                class = "event-modal-icon",
                bsicons::bs_icon("geo-alt")
              ),
              htmltools::tags$div(
                class = "event-modal-content",
                htmltools::tags$div(class = "event-modal-location", "Location")
              )
            ),
            # Description (conditionally shown)
            htmltools::tags$div(
              class = "event-modal-row event-modal-description-row",
              htmltools::tags$div(
                class = "event-modal-icon",
                bsicons::bs_icon("text-left")
              ),
              htmltools::tags$div(
                class = "event-modal-content",
                htmltools::tags$div(class = "event-modal-description text-muted", "Description")
              )
            )
          ),
          # Modal Footer
          htmltools::tags$div(
            class = "modal-footer",
            htmltools::tags$button(
              type = "button",
              class = "btn btn-secondary",
              `data-bs-dismiss` = "modal",
              "Close"
            )
          )
        )
      )
    )
  )
}
