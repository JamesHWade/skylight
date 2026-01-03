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

  tagList(
    # Add custom CSS if it exists
    if (file.exists(file.path(www_path, "styles.css"))) {
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "www/styles.css")
      )
    },
    # Add custom JS if it exists
    if (file.exists(file.path(www_path, "custom.js"))) {
      tags$head(
        tags$script(src = "www/custom.js")
      )
    },
    # Add favicon
    tags$head(
      tags$link(rel = "icon", type = "image/x-icon", href = "www/favicon.ico")
    ),
    # PWA manifest for iPad home screen
    tags$head(
      tags$link(rel = "manifest", href = "www/manifest.json"),
      tags$meta(name = "apple-mobile-web-app-capable", content = "yes"),
      tags$meta(name = "apple-mobile-web-app-status-bar-style", content = "default"),
      tags$meta(name = "apple-mobile-web-app-title", content = "Skylight")
    ),
    # Viewport for mobile
    tags$head(
      tags$meta(
        name = "viewport",
        content = "width=device-width, initial-scale=1, maximum-scale=1, user-scalable=no"
      )
    ),
    # Add resource path for www directory
    shiny::addResourcePath("www", www_path)
  )
}
