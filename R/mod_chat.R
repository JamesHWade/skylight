#' Chat Module UI
#'
#' AI-powered chat interface for natural language calendar interaction.
#'
#' @param id Module namespace ID
#'
#' @return A Shiny UI definition
#'
#' @keywords internal
mod_chat_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::div(
    class = "chat-panel",
    # Header
    htmltools::div(
      class = "chat-header d-flex justify-content-between align-items-center p-3",
      htmltools::h5(
        class = "mb-0",
        bsicons::bs_icon("chat-dots", class = "me-2"),
        "Calendar Assistant"
      ),
      shiny::actionButton(
        ns("clear_chat"),
        label = NULL,
        icon = bsicons::bs_icon("trash"),
        class = "btn-sm btn-outline-secondary"
      )
    ),

    # Chat messages area
    htmltools::div(
      class = "chat-messages",
      id = ns("messages_container"),
      shiny::uiOutput(ns("chat_messages"))
    ),

    # Input area
    htmltools::div(
      class = "chat-input-area p-3",
      htmltools::div(
        class = "input-group",
        shiny::textInput(
          ns("user_input"),
          label = NULL,
          placeholder = "Ask about your calendar...",
          width = "100%"
        ),
        shiny::actionButton(
          ns("send_message"),
          label = NULL,
          icon = bsicons::bs_icon("send"),
          class = "btn-primary"
        )
      ),
      htmltools::div(
        class = "chat-suggestions mt-2",
        lapply(
          c(
            "What's on my calendar today?",
            "Any conflicts this week?",
            "When am I free tomorrow?"
          ),
          function(suggestion) {
            shiny::actionLink(
              ns(paste0("suggest_", digest::digest(suggestion, algo = "crc32"))),
              suggestion,
              class = "chat-suggestion badge bg-light text-dark me-1"
            )
          }
        )
      )
    )
  )
}

#' Chat Module Server
#'
#' @param id Module namespace ID
#' @param events Reactive containing event data
#' @param calendars Reactive containing calendar list
#' @param selected_date Reactive value for selected date
#'
#' @keywords internal
mod_chat_server <- function(id, events, calendars, selected_date) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Chat history
    chat_history <- shiny::reactiveVal(list())

    # Initialize chat with ellmer (may fail if API key not configured)
    chat <- shiny::reactive({
      tryCatch(
        create_calendar_chat(events(), calendars()),
        error = function(e) NULL
      )
    })

    # Render chat messages
    output$chat_messages <- shiny::renderUI({
      history <- chat_history()

      if (length(history) == 0) {
        return(
          htmltools::div(
            class = "chat-welcome text-center p-4",
            bsicons::bs_icon("calendar-heart", size = "3em", class = "text-primary mb-3"),
            htmltools::h5("Hi! I'm your calendar assistant."),
            htmltools::p(
              class = "text-muted",
              "Ask me about your schedule, find free time, or get a summary of upcoming events."
            )
          )
        )
      }

      htmltools::div(
        class = "messages-list",
        lapply(history, function(msg) {
          render_chat_message(msg)
        })
      )
    })

    # Handle send button click
    shiny::observeEvent(input$send_message, {
      shiny::req(input$user_input)
      process_message(input$user_input)
      shiny::updateTextInput(session, "user_input", value = "")
    })
    # Handle Enter key in input
    shiny::observeEvent(input$user_input, {
      # This would need JS to detect Enter key - handled via custom.js
    }, ignoreInit = TRUE)

    # Handle suggestion clicks
    shiny::observe({
      suggestions <- c(
        "What's on my calendar today?",
        "Any conflicts this week?",
        "When am I free tomorrow?"
      )
      lapply(suggestions, function(suggestion) {
        input_id <- paste0("suggest_", digest::digest(suggestion, algo = "crc32"))
        shiny::observeEvent(input[[input_id]], {
          process_message(suggestion)
        }, ignoreInit = TRUE)
      })
    })

    # Clear chat
    shiny::observeEvent(input$clear_chat, {
      chat_history(list())
    })

    # Process user message
    process_message <- function(user_message) {
      # Add user message to history
      history <- chat_history()
      history <- append(history, list(list(
        role = "user",
        content = user_message,
        timestamp = Sys.time()
      )))
      chat_history(history)

      # Get AI response
      tryCatch({
        response <- get_chat_response(
          chat(),
          user_message,
          events(),
          selected_date()
        )

        # Add assistant response to history
        history <- chat_history()
        history <- append(history, list(list(
          role = "assistant",
          content = response,
          timestamp = Sys.time()
        )))
        chat_history(history)

        # Save to database
        save_chat_message(user_message, response)

      }, error = function(e) {
        # Add error message
        history <- chat_history()
        history <- append(history, list(list(
          role = "assistant",
          content = paste("Sorry, I encountered an error:", e$message),
          timestamp = Sys.time(),
          is_error = TRUE
        )))
        chat_history(history)
      })
    }
  })
}

#' Render a Chat Message
#'
#' @param msg A message list with role, content, and timestamp
#'
#' @return HTML for the message
#'
#' @keywords internal
render_chat_message <- function(msg) {
  is_user <- msg$role == "user"
  is_error <- isTRUE(msg$is_error)

  htmltools::div(
    class = paste(
      "chat-message",
      if (is_user) "user-message" else "assistant-message",
      if (is_error) "error-message" else ""
    ),
    htmltools::div(
      class = "message-avatar",
      if (is_user) {
        bsicons::bs_icon("person-circle")
      } else {
        bsicons::bs_icon("robot")
      }
    ),
    htmltools::div(
      class = "message-content",
      htmltools::div(
        class = "message-text",
        htmltools::HTML(commonmark::markdown_html(msg$content))
      ),
      htmltools::div(
        class = "message-timestamp text-muted small",
        format(msg$timestamp, "%l:%M %p")
      )
    )
  )
}

#' Create Calendar Chat Instance
#'
#' Initialize an ellmer chat with calendar context.
#'
#' @param events Current events data
#' @param calendars Current calendars list
#'
#' @return An ellmer chat object
#'
#' @keywords internal
create_calendar_chat <- function(events, calendars) {
  # Build context about current calendar state
  today <- Sys.Date()
  events_summary <- if (!is.null(events) && nrow(events) > 0) {
    paste(
      "The user has", nrow(events), "events in the current view.",
      "Calendars:", paste(unique(events$calendar_name), collapse = ", ")
    )
  } else {
    "The user's calendar appears to be empty for the current view."
  }

  system_prompt <- glue::glue("
    You are a helpful family calendar assistant for the Skylight calendar app.

    Current date: {format(today, '%A, %B %d, %Y')}

    Calendar context:
    {events_summary}

    Your capabilities:
    - Answer questions about the user's schedule
    - Summarize upcoming events
    - Identify scheduling conflicts
    - Find free time slots
    - Provide reminders about important events

    Guidelines:
    - Be concise and friendly
    - Use natural language for dates (e.g., 'tomorrow' instead of the date)
    - Format times in a readable way (e.g., '2:30 PM')
    - If you don't have enough information, ask for clarification
    - Focus on being helpful for a family with busy schedules
  ")

  # Create ellmer chat
  ellmer::chat_claude(
    system_prompt = system_prompt,
    model = "claude-sonnet-4-20250514"
  )
}

#' Get Chat Response
#'
#' Send a message to the LLM and get a response.
#'
#' @param chat An ellmer chat object
#' @param message User's message
#' @param events Current events data
#' @param selected_date Current selected date
#'
#' @return Character string with the response
#'
#' @keywords internal
get_chat_response <- function(chat, message, events, selected_date) {
  # Build events context for the query
  events_context <- ""
  if (!is.null(events) && nrow(events) > 0) {
    events_text <- apply(events, 1, function(e) {
      time_str <- if (e["all_day"]) {
        "All day"
      } else {
        paste(
          format(as.POSIXct(e["start"]), "%l:%M %p"),
          "-",
          format(as.POSIXct(e["end"]), "%l:%M %p")
        )
      }
      paste0(
        "- ", format(as.Date(e["start"]), "%A %b %d"), ": ",
        e["title"], " (", time_str, ")",
        if (!is.na(e["location"]) && nchar(e["location"]) > 0) {
          paste0(" at ", e["location"])
        } else ""
      )
    })
    events_context <- paste(
      "\n\nUpcoming events:\n",
      paste(events_text, collapse = "\n")
    )
  }

  # Send message with context
  full_message <- paste0(message, events_context)

  # Get response from ellmer
  chat$chat(full_message)
}

#' Save Chat Message to Database
#'
#' @param user_message The user's message
#' @param assistant_response The assistant's response
#'
#' @keywords internal
save_chat_message <- function(user_message, assistant_response) {
  tryCatch({
    con <- db_connect()
    on.exit(DBI::dbDisconnect(con))

    DBI::dbExecute(con, "
      INSERT INTO chat_history (user_message, assistant_response, created_at)
      VALUES (?, ?, ?)
    ", params = list(user_message, assistant_response, as.character(Sys.time())))
  }, error = function(e) {
    # Silently fail - chat history is not critical
  })
}
