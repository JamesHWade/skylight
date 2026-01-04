#' Chores Widget Module UI
#'
#' A compact chores display for the navbar showing today's progress.
#' Clicking navigates to the Chores tab.
#'
#' @param id Module namespace ID
#'
#' @return A Shiny UI definition
#'
#' @keywords internal
mod_chores_widget_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::div(
    id = ns("chores_widget"),
    class = "chores-widget d-flex align-items-center",
    role = "button",
    tabindex = "0",
    `aria-label` = "View chores",
    shiny::uiOutput(ns("chores_display"))
  )
}

#' Chores Widget Module Server
#'
#' Displays today's chore completion status in a compact format.
#'
#' @param id Module namespace ID
#' @param refresh_trigger Optional reactive to trigger refresh
#'
#' @keywords internal
mod_chores_widget_server <- function(id, refresh_trigger = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Auto-refresh every 30 seconds
    auto_refresh <- shiny::reactiveTimer(30000)

    # Get summary data
    summary_data <- shiny::reactive({
      auto_refresh()
      if (!is.null(refresh_trigger)) refresh_trigger()

      tryCatch(
        get_chores_today_summary(),
        error = function(e) list(completed = 0, total = 0)
      )
    })

    # Render widget display
    output$chores_display <- shiny::renderUI({
      summary <- summary_data()
      completed <- summary$completed
      total <- summary$total

      if (total == 0) {
        # No chores assigned today
        htmltools::tagList(
          bsicons::bs_icon("list-check", class = "me-1"),
          htmltools::span(class = "chores-count", "---")
        )
      } else if (completed == total) {
        # All done!
        htmltools::tagList(
          bsicons::bs_icon("check-circle-fill", class = "text-success me-1"),
          htmltools::span(class = "chores-count text-success", paste0(completed, "/", total))
        )
      } else {
        # In progress
        pct <- round(completed / total * 100)
        htmltools::tagList(
          bsicons::bs_icon("list-check", class = "me-1"),
          htmltools::span(class = "chores-count", paste0(completed, "/", total))
        )
      }
    })

    # Handle click - navigate to chores tab
    shiny::observeEvent(input$widget_click, {
      # This would need to be handled by the parent to switch tabs
      # For now, we'll use JavaScript to click the Chores nav item
      shinyjs::runjs("
        var choresTab = document.querySelector('[data-value=\"chores\"]');
        if (choresTab) choresTab.click();
      ")
    })

    # Make widget clickable
    shiny::observe({
      shinyjs::runjs(sprintf("
        var widget = document.getElementById('%s');
        if (widget) {
          widget.style.cursor = 'pointer';
          widget.onclick = function() {
            var choresTab = document.querySelector('[data-value=\"chores\"]');
            if (choresTab) choresTab.click();
          };
        }
      ", ns("chores_widget")))
    })

    # Return summary for external use
    summary_data
  })
}
