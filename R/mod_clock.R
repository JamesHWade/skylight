#' Clock Module UI
#'
#' A live-updating clock display for the navbar.
#'
#' @param id Module namespace ID
#'
#' @return A Shiny UI definition
#'
#' @keywords internal
mod_clock_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::div(
    class = "clock-widget d-flex align-items-center",
    htmltools::div(
      class = "clock-time me-2",
      shiny::textOutput(ns("time"), inline = TRUE)
    ),
    htmltools::div(
      class = "clock-date text-muted d-none d-md-block",
      shiny::textOutput(ns("date"), inline = TRUE)
    )
  )
}

#' Clock Module Server
#'
#' @param id Module namespace ID
#'
#' @keywords internal
mod_clock_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    # Update every second
    time_value <- shiny::reactiveVal(Sys.time())

    shiny::observe({
      shiny::invalidateLater(1000)
      time_value(Sys.time())
    })

    # Time display (e.g., "2:45 PM")
    output$time <- shiny::renderText({
      format(time_value(), "%l:%M %p")
    })

    # Date display (e.g., "Monday, January 15")
    output$date <- shiny::renderText({
      format(time_value(), "%A, %B %d")
    })
  })
}
