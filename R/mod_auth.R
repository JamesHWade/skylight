#' Authentication Module UI
#'
#' UI for Google Calendar authentication status and controls.
#'
#' @param id Module namespace ID
#'
#' @return A Shiny UI definition
#'
#' @keywords internal
mod_auth_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::div(
    class = "auth-widget p-2",
    shiny::uiOutput(ns("auth_status"))
  )
}

#' Authentication Module Server
#'
#' @param id Module namespace ID
#'
#' @return A reactive indicating authentication status (TRUE/FALSE)
#'
#' @keywords internal
mod_auth_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Authentication state
    auth_state <- shiny::reactiveVal(FALSE)

    # Check authentication on load
    shiny::observe({
      auth_state(is_authenticated())
    })

    # Render auth status UI
    output$auth_status <- shiny::renderUI({
      if (auth_state()) {
        # Authenticated - show user info and logout button
        user_email <- tryCatch(
          get_auth_email(),
          error = function(e) "Connected"
        )

        htmltools::div(
          class = "auth-connected",
          htmltools::div(
            class = "d-flex align-items-center mb-2",
            bsicons::bs_icon("check-circle-fill", class = "text-success me-2"),
            htmltools::span("Connected to Google Calendar")
          ),
          htmltools::div(
            class = "auth-email text-muted small mb-2",
            user_email
          ),
          shiny::actionButton(
            ns("logout"),
            "Disconnect",
            icon = bsicons::bs_icon("box-arrow-right"),
            class = "btn-sm btn-outline-danger w-100"
          )
        )
      } else {
        # Not authenticated - show connect button
        htmltools::div(
          class = "auth-disconnected",
          htmltools::div(
            class = "d-flex align-items-center mb-2 text-warning",
            bsicons::bs_icon("exclamation-triangle-fill", class = "me-2"),
            htmltools::span("Not connected")
          ),
          htmltools::p(
            class = "small text-muted mb-2",
            "Connect your Google Calendar to see your events."
          ),
          shiny::actionButton(
            ns("login"),
            "Connect Google Calendar",
            icon = bsicons::bs_icon("google"),
            class = "btn-sm btn-primary w-100"
          )
        )
      }
    })

    # Handle login
    shiny::observeEvent(input$login, {
      tryCatch({
        # Trigger OAuth flow
        calendar_auth()
        auth_state(TRUE)
        shiny::showNotification(
          "Successfully connected to Google Calendar!",
          type = "message"
        )
      }, error = function(e) {
        shiny::showNotification(
          paste("Authentication failed:", e$message),
          type = "error"
        )
      })
    })

    # Handle logout
    shiny::observeEvent(input$logout, {
      shiny::showModal(shiny::modalDialog(
        title = "Disconnect Google Calendar?",
        htmltools::p("You'll need to reconnect to see your calendar events."),
        footer = htmltools::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(ns("confirm_logout"), "Disconnect", class = "btn-danger")
        )
      ))
    })

    # Confirm logout
    shiny::observeEvent(input$confirm_logout, {
      calendar_deauth()
      auth_state(FALSE)
      shiny::removeModal()
      shiny::showNotification(
        "Disconnected from Google Calendar",
        type = "message"
      )
    })

    # Return auth state as reactive
    auth_state
  })
}
