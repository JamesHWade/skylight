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

    # Generated icon storage
    generated_icon <- shiny::reactiveVal(NULL)

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

    # Helper to render countdown icon
    render_countdown_icon <- function(icon_base64, emoji, size = "1rem") {
      if (!is.null(icon_base64) && !is.na(icon_base64) && nchar(icon_base64) > 0) {
        htmltools::img(
          src = paste0("data:image/png;base64,", icon_base64),
          class = "countdown-custom-icon",
          style = htmltools::css(width = size, height = size),
          alt = ""
        )
      } else {
        # Use Bootstrap icon instead of emoji for navbar consistency
        bsicons::bs_icon("calendar-heart", class = "me-1")
      }
    }

    # Render widget display
    output$countdown_display <- shiny::renderUI({
      countdown <- countdown_data()

      if (is.null(countdown)) {
        # No countdowns configured - use Bootstrap icon
        htmltools::tagList(
          bsicons::bs_icon("calendar-heart", class = "me-1"),
          htmltools::span(class = "countdown-text text-muted d-none d-lg-inline", "No countdowns")
        )
      } else if (countdown$days == 0) {
        # Today!
        htmltools::tagList(
          render_countdown_icon(countdown$icon_base64, countdown$emoji, "1.25rem"),
          htmltools::span(
            class = "countdown-text countdown-today",
            "Today!"
          )
        )
      } else if (countdown$days == 1) {
        # Tomorrow
        htmltools::tagList(
          render_countdown_icon(countdown$icon_base64, countdown$emoji, "1.25rem"),
          htmltools::span(
            class = "countdown-text",
            htmltools::span(class = "countdown-days fw-bold", "1"),
            " day"
          )
        )
      } else {
        # Multiple days
        htmltools::tagList(
          render_countdown_icon(countdown$icon_base64, countdown$emoji, "1.25rem"),
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
          bsicons::bs_icon("calendar-heart"),
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
          htmltools::p(bsicons::bs_icon("calendar-heart", size = "3rem")),
          htmltools::p("No countdown events yet."),
          htmltools::p(class = "small", "Add special events to track how many days until they happen!")
        ))
      }

      htmltools::div(
        class = "countdown-list",
        lapply(seq_len(nrow(countdowns)), function(i) {
          cd <- countdowns[i, ]
          days <- days_until(cd$target_date)
          is_past <- days < 0
          is_today <- days == 0

          # Render icon (custom or fallback)
          icon_element <- if (!is.null(cd$icon_base64) && !is.na(cd$icon_base64) &&
                              nchar(cd$icon_base64) > 0) {
            htmltools::img(
              src = paste0("data:image/png;base64,", cd$icon_base64),
              class = "countdown-item-icon",
              style = "width: 40px; height: 40px; object-fit: contain; border-radius: 6px;",
              alt = ""
            )
          } else {
            htmltools::span(
              class = "countdown-item-emoji",
              style = "font-size: 1.5rem;",
              cd$emoji
            )
          }

          htmltools::div(
            class = paste("countdown-item d-flex align-items-center gap-3 p-3 border-bottom",
                         if (is_past) "opacity-50" else if (is_today) "bg-light"),
            # Icon
            icon_element,
            # Info
            htmltools::div(
              class = "flex-grow-1",
              htmltools::div(class = "fw-medium", cd$title),
              htmltools::div(
                class = "small text-muted",
                format(as.Date(cd$target_date), "%B %d, %Y")
              )
            ),
            # Days count
            htmltools::div(
              class = "countdown-item-days text-end",
              style = htmltools::css(color = if (is_today) cd$color else NULL),
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
              ns(paste0("delete_", cd$id)),
              "",
              icon = bsicons::bs_icon("trash"),
              class = "btn-outline-danger btn-sm ms-2",
              onclick = sprintf(
                "Shiny.setInputValue('%s', {id: %d, nonce: Math.random()})",
                ns("delete_countdown"), cd$id
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
      generated_icon(NULL)  # Reset generated icon
      shiny::removeModal()
      shiny::showModal(shiny::modalDialog(
        title = "Add Countdown",
        size = "m",
        easyClose = TRUE,
        footer = htmltools::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(ns("save_countdown"), "Add", class = "btn-primary")
        ),
        countdown_form(ns)
      ))
    })

    # Icon preview output
    output$countdown_icon_preview <- shiny::renderUI({
      icon_data <- generated_icon()
      if (is.null(icon_data) || nchar(icon_data) == 0) {
        # Show placeholder
        return(htmltools::div(
          class = "icon-preview-placeholder",
          bsicons::bs_icon("image", size = "2rem", class = "text-muted"),
          htmltools::span(class = "small text-muted d-block mt-1", "No icon yet")
        ))
      }

      htmltools::div(
        class = "icon-preview",
        htmltools::img(
          src = paste0("data:image/png;base64,", icon_data),
          class = "generated-icon-img",
          alt = "Generated icon"
        ),
        shiny::actionButton(
          ns("clear_countdown_icon"),
          "",
          icon = bsicons::bs_icon("x-circle"),
          class = "btn-link btn-sm text-muted p-0 clear-icon-btn",
          title = "Remove generated icon"
        )
      )
    })

    # Generate icon for countdown
    shiny::observeEvent(input$generate_countdown_icon, {
      title <- trimws(input$countdown_title)
      if (nchar(title) == 0) {
        shiny::showNotification("Enter an event name first", type = "warning")
        return()
      }

      shiny::withProgress(message = "Generating icon...", {
        result <- generate_icon(title, type = "event")
        if (result$success) {
          generated_icon(result$base64)
          shiny::showNotification("Icon generated!", type = "message", duration = 2)
        } else {
          shiny::showNotification(
            paste("Generation failed:", result$error),
            type = "error"
          )
        }
      })
    })

    # Clear generated icon
    shiny::observeEvent(input$clear_countdown_icon, {
      generated_icon(NULL)
    })

    # Use emoji instead
    shiny::observeEvent(input$use_countdown_emoji, {
      generated_icon(NULL)
      shiny::showNotification(
        paste("Using emoji:", input$countdown_emoji),
        type = "message",
        duration = 2
      )
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

      icon_base64 <- generated_icon()

      add_countdown(
        title = title,
        target_date = target_date,
        emoji = emoji,
        icon_base64 = icon_base64
      )

      generated_icon(NULL)  # Clear for next use
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
    # Icon section - AI generated is primary, emoji is fallback
    htmltools::div(
      class = "icon-section mb-3",
      htmltools::tags$label(class = "form-label", "Icon"),

      # AI icon area (primary)
      htmltools::div(
        class = "ai-icon-section mb-2",
        shiny::uiOutput(ns("countdown_icon_preview")),
        if (gemini_available()) {
          htmltools::div(
            class = "d-flex gap-2 align-items-center mt-2",
            shiny::actionButton(
              ns("generate_countdown_icon"),
              htmltools::tagList(bsicons::bs_icon("stars"), "Generate Icon"),
              class = "btn-outline-primary btn-sm"
            ),
            htmltools::span(
              class = "small text-muted",
              "AI-generated based on event name"
            )
          )
        } else {
          htmltools::div(
            class = "text-muted small",
            bsicons::bs_icon("info-circle"),
            " Set GEMINI_API_KEY for AI icons"
          )
        }
      ),

      # Emoji fallback (collapsed by default)
      htmltools::tags$details(
        class = "emoji-fallback mt-2",
        htmltools::tags$summary(
          class = "text-muted small cursor-pointer",
          "Or choose an emoji instead..."
        ),
        htmltools::div(
          class = "pt-2",
          shiny::radioButtons(
            ns("countdown_emoji"),
            NULL,
            choices = setNames(emoji_choices, emoji_choices),
            selected = emoji_choices[1],
            inline = TRUE
          ) |> htmltools::tagAppendAttributes(class = "emoji-radio-picker"),
          shiny::actionButton(
            ns("use_countdown_emoji"),
            "Use this emoji",
            class = "btn-outline-secondary btn-sm mt-2"
          )
        )
      )
    )
  )
}
