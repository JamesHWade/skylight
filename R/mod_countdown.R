#' Countdown Widget Module UI
#'
#' A compact countdown display for the navbar showing days until next event.
#' Clicking opens a modal to manage countdowns.
#'
#' @param id Module namespace ID
#'
#' @return A Shiny UI definition
#'
#' @keywords internal
mod_countdown_ui <- function(id) {

  ns <- shiny::NS(id)

  htmltools::div(
    id = ns("countdown_widget"),
    class = "countdown-widget d-flex align-items-center",
    role = "button",
    tabindex = "0",
    `aria-label` = "View countdowns",
    shiny::uiOutput(ns("countdown_display"))
  )
}

#' Countdown Widget Module Server
#'
#' Displays the next upcoming countdown in a compact format.
#'
#' @param id Module namespace ID
#'
#' @keywords internal
mod_countdown_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Refresh trigger
    refresh_trigger <- shiny::reactiveVal(0)

    # Auto-refresh every 5 minutes (countdowns don't need frequent updates)
    auto_refresh <- shiny::reactiveTimer(300000)

    # Get countdown data
    countdown_data <- shiny::reactive({
      auto_refresh()
      refresh_trigger()

      tryCatch(
        get_countdown_summary(),
        error = function(e) NULL
      )
    })

    # Render widget display
    output$countdown_display <- shiny::renderUI({
      countdown <- countdown_data()

      if (is.null(countdown)) {
        # No countdowns configured
        htmltools::tagList(
          htmltools::span(class = "countdown-icon", "\U0001F4C5"),
          htmltools::span(class = "countdown-text text-muted", "No countdowns")
        )
      } else if (countdown$days == 0) {
        # Today!
        htmltools::tagList(
          htmltools::span(class = "countdown-icon", countdown$emoji),
          htmltools::span(
            class = "countdown-text countdown-today",
            style = htmltools::css(color = countdown$color),
            paste0("Today: ", countdown$title, "!")
          )
        )
      } else if (countdown$days == 1) {
        # Tomorrow
        htmltools::tagList(
          htmltools::span(class = "countdown-icon", countdown$emoji),
          htmltools::span(
            class = "countdown-text",
            htmltools::span(class = "countdown-days fw-bold", "1"),
            " day"
          )
        )
      } else {
        # Multiple days
        htmltools::tagList(
          htmltools::span(class = "countdown-icon", countdown$emoji),
          htmltools::span(
            class = "countdown-text",
            htmltools::span(class = "countdown-days fw-bold", countdown$days),
            " days"
          )
        )
      }
    })

    # Make widget clickable - opens management modal
    shiny::observe({
      shinyjs::runjs(sprintf("
        var widget = document.getElementById('%s');
        if (widget) {
          widget.style.cursor = 'pointer';
          widget.onclick = function() {
            Shiny.setInputValue('%s', Math.random());
          };
        }
      ", ns("countdown_widget"), ns("widget_click")))
    })

    # Handle click - show countdown management modal
    shiny::observeEvent(input$widget_click, {
      shiny::showModal(shiny::modalDialog(
        title = htmltools::div(
          class = "d-flex align-items-center gap-2",
          htmltools::span("\U0001F389"),
          "Countdown Events"
        ),
        size = "m",
        easyClose = TRUE,
        footer = htmltools::tagList(
          shiny::actionButton(ns("add_countdown"), "Add Countdown",
                             icon = bsicons::bs_icon("plus-circle"),
                             class = "btn-primary"),
          shiny::modalButton("Close")
        ),
        shiny::uiOutput(ns("countdown_list"))
      ))
    })

    # Render countdown list in modal
    output$countdown_list <- shiny::renderUI({
      refresh_trigger()
      countdowns <- tryCatch(
        get_countdowns(future_only = FALSE, limit = 20),
        error = function(e) data.frame()
      )

      if (is.null(countdowns) || nrow(countdowns) == 0) {
        return(htmltools::div(
          class = "text-center text-muted py-4",
          htmltools::p(htmltools::span(style = "font-size: 3rem;", "\U0001F4C5")),
          htmltools::p("No countdown events yet."),
          htmltools::p(class = "small", "Add special events to track how many days until they happen!")
        ))
      }

      htmltools::div(
        class = "countdown-list",
        lapply(seq_len(nrow(countdowns)), function(i) {
          c <- countdowns[i, ]
          days <- days_until(c$target_date)
          is_past <- days < 0
          is_today <- days == 0

          htmltools::div(
            class = paste("countdown-item d-flex align-items-center gap-3 p-3 border-bottom",
                         if (is_past) "opacity-50" else if (is_today) "bg-light"),
            # Emoji
            htmltools::span(
              class = "countdown-item-emoji",
              style = "font-size: 1.5rem;",
              c$emoji
            ),
            # Info
            htmltools::div(
              class = "flex-grow-1",
              htmltools::div(class = "fw-medium", c$title),
              htmltools::div(
                class = "small text-muted",
                format(as.Date(c$target_date), "%B %d, %Y")
              )
            ),
            # Days count
            htmltools::div(
              class = "countdown-item-days text-end",
              style = htmltools::css(color = if (is_today) c$color else NULL),
              if (is_today) {
                htmltools::span(class = "badge bg-success", "Today!")
              } else if (days == 1) {
                htmltools::span(class = "fw-bold", "Tomorrow")
              } else if (is_past) {
                htmltools::span(class = "text-muted", paste0(abs(days), " days ago"))
              } else {
                htmltools::tagList(
                  htmltools::span(class = "fw-bold", style = "font-size: 1.25rem;", days),
                  htmltools::span(class = "text-muted small", " days")
                )
              }
            ),
            # Delete button
            shiny::actionButton(
              ns(paste0("delete_", c$id)),
              "",
              icon = bsicons::bs_icon("trash"),
              class = "btn-outline-danger btn-sm ms-2",
              onclick = sprintf(
                "Shiny.setInputValue('%s', {id: %d, nonce: Math.random()})",
                ns("delete_countdown"), c$id
              )
            )
          )
        })
      )
    })

    # Handle delete
    shiny::observeEvent(input$delete_countdown, {
      data <- input$delete_countdown
      if (is.null(data)) return()

      remove_countdown(data$id)
      refresh_trigger(refresh_trigger() + 1)
      shiny::showNotification("Countdown removed", type = "warning", duration = 2)
    })

    # Handle add countdown button
    shiny::observeEvent(input$add_countdown, {
      shiny::removeModal()
      shiny::showModal(shiny::modalDialog(
        title = "Add Countdown",
        size = "s",
        easyClose = TRUE,
        footer = htmltools::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(ns("save_countdown"), "Add", class = "btn-primary")
        ),
        countdown_form(ns)
      ))
    })

    # Save new countdown
    shiny::observeEvent(input$save_countdown, {
      title <- trimws(input$countdown_title)
      if (nchar(title) == 0) {
        shiny::showNotification("Title is required", type = "error")
        return()
      }

      target_date <- input$countdown_date
      if (is.null(target_date)) {
        shiny::showNotification("Date is required", type = "error")
        return()
      }

      emoji <- input$countdown_emoji
      if (is.null(emoji) || nchar(emoji) == 0) emoji <- "\U0001F389"

      add_countdown(
        title = title,
        target_date = target_date,
        emoji = emoji
      )

      refresh_trigger(refresh_trigger() + 1)
      shiny::removeModal()
      shiny::showNotification(paste("Countdown added:", title), type = "message")
    })

    # Return refresh trigger for external use
    refresh_trigger
  })
}

#' Countdown Form
#'
#' Form for adding a new countdown event.
#'
#' @param ns Namespace function
#'
#' @keywords internal
countdown_form <- function(ns) {
  # Emoji options
  emoji_choices <- c(
    "\U0001F389", # Party popper
    "\U0001F3D6", # Beach
    "\U0001F384", # Christmas tree
    "\U0001F382", # Birthday cake
    "\U0001F48D", # Ring (wedding)
    "\U0001F393", # Graduation cap
    "\U0001F3C6", # Trophy
    "\U0001F680", # Rocket
    "\U00002708", # Airplane
    "\U0001F3A4", # Microphone (concert)
    "\U0001F3AE", # Game controller
    "\U0001F3C0"  # Basketball
  )

  htmltools::tagList(
    shiny::textInput(
      ns("countdown_title"),
      "Event Name",
      placeholder = "e.g., Summer Vacation"
    ),
    shiny::dateInput(
      ns("countdown_date"),
      "Date",
      value = Sys.Date() + 30,
      min = Sys.Date()
    ),
    htmltools::div(
      class = "mb-3",
      htmltools::tags$label(class = "form-label", "Icon"),
      shiny::radioButtons(
        ns("countdown_emoji"),
        NULL,
        choices = setNames(emoji_choices, emoji_choices),
        selected = emoji_choices[1],
        inline = TRUE
      ) |> htmltools::tagAppendAttributes(class = "emoji-radio-picker")
    )
  )
}
