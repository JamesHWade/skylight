#' Chat Module UI
#'
#' AI-powered chat interface for natural language calendar interaction.
#' Uses shinychat for the chat UI component.
#'
#' @param id Module namespace ID
#'
#' @return A Shiny UI definition
#'
#' @keywords internal
mod_chat_ui <- function(id) {

  ns <- shiny::NS(id)

 htmltools::div(
    class = "chat-panel d-flex flex-column h-100",
    # Header
    htmltools::div(
      class = "chat-header d-flex justify-content-between align-items-center p-3 border-bottom",
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

    # Shinychat UI - fills available space
    htmltools::div(
      class = "flex-grow-1 overflow-hidden",
      shinychat::chat_ui(
        id = ns("chat"),
        messages = list(
          list(
            role = "assistant",
            content = "Hi! I'm your calendar assistant. Ask me about your schedule, find free time, or add new events using natural language."
          )
        ),
        placeholder = "Ask about your calendar...",
        fill = TRUE
      )
    ),

    # Suggestion chips below the chat
    htmltools::div(
      class = "chat-suggestions p-2 border-top",
      lapply(
        c(
          "What's on my calendar today?",
          "Add soccer practice Tuesday 4pm",
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
}

#' Chat Module Server
#'
#' @param id Module namespace ID
#' @param events Reactive containing event data
#' @param calendars Reactive containing calendar list
#' @param selected_date Reactive value for selected date
#' @param refresh_trigger Reactive value to trigger calendar refresh after event creation
#' @param can_create_events Logical. Whether the chat can create events (FALSE in demo mode)
#'
#' @keywords internal
mod_chat_server <- function(id, events, calendars, selected_date,
                            refresh_trigger = NULL, can_create_events = TRUE) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Track chat initialization errors for user feedback
    chat_error <- shiny::reactiveVal(NULL)

    # Track event creation callback for tool use
    on_event_created <- function() {
      if (!is.null(refresh_trigger)) {
        refresh_trigger(refresh_trigger() + 1)
      }
    }

    # Initialize chat with ellmer (requires ANTHROPIC_API_KEY)
    # Use reactiveVal to maintain chat state across messages
    chat_instance <- shiny::reactiveVal(NULL)

    shiny::observe({
      tryCatch({
        chat_error(NULL)
        chat_obj <- create_calendar_chat(
          events = events(),
          calendars = calendars(),
          can_create_events = can_create_events,
          on_event_created = on_event_created
        )
        chat_instance(chat_obj)
      }, error = function(e) {
        error_msg <- conditionMessage(e)
        warning("Chat initialization failed: ", error_msg, call. = FALSE)
        chat_error(error_msg)
        chat_instance(NULL)
      })
    }) |> shiny::bindEvent(events(), calendars(), once = FALSE)

    # Handle user input from shinychat (input$chat_user_input)
    shiny::observeEvent(input$chat_user_input, {
      user_message <- input$chat_user_input
      shiny::req(nchar(trimws(user_message)) > 0)

      # Check if chat is available
      chat_obj <- chat_instance()
      if (is.null(chat_obj)) {
        error_detail <- chat_error()
        error_msg <- if (!is.null(error_detail)) {
          paste("Chat is not available:", error_detail)
        } else {
          "Chat is not available. Please ensure the ANTHROPIC_API_KEY environment variable is configured."
        }
        shinychat::chat_append("chat", error_msg, session = session)
        return()
      }

      # Build events context for the query
      events_context <- build_events_context(events())

      # Send message with context and stream response
      full_message <- paste0(user_message, events_context)

      tryCatch({
        # Use streaming for better UX
        stream <- chat_obj$stream(full_message)
        shinychat::chat_append("chat", stream, session = session)

        # Save to database (get the full response text)
        # Note: streaming means we don't easily get the full text here
        # We'll save asynchronously or skip for now
      }, error = function(e) {
        error_msg <- paste("Sorry, I encountered an error:", conditionMessage(e))
        shinychat::chat_append("chat", error_msg, session = session)
      })
    }, ignoreInit = TRUE)

    # Handle suggestion clicks
    suggestions <- c(
      "What's on my calendar today?",
      "Add soccer practice Tuesday 4pm",
      "When am I free tomorrow?"
    )

    lapply(suggestions, function(suggestion) {
      input_id <- paste0("suggest_", digest::digest(suggestion, algo = "crc32"))
      shiny::observeEvent(input[[input_id]], {
        # Add as user message and trigger the chat
        shinychat::chat_append("chat", suggestion, role = "user", session = session)

        # Process the suggestion through the chat
        chat_obj <- chat_instance()
        if (!is.null(chat_obj)) {
          events_context <- build_events_context(events())
          full_message <- paste0(suggestion, events_context)
          tryCatch({
            stream <- chat_obj$stream(full_message)
            shinychat::chat_append("chat", stream, session = session)
          }, error = function(e) {
            error_msg <- paste("Sorry, I encountered an error:", conditionMessage(e))
            shinychat::chat_append("chat", error_msg, session = session)
          })
        }
      }, ignoreInit = TRUE)
    })

    # Clear chat - reset with welcome message
    shiny::observeEvent(input$clear_chat, {
      shinychat::chat_clear("chat", session = session)
      # Re-add welcome message
      shinychat::chat_append(
        "chat",
        "Hi! I'm your calendar assistant. Ask me about your schedule, find free time, or add new events using natural language.",
        session = session
      )
      # Reset the chat instance to clear conversation history
      chat_instance(NULL)
    })
  })
}

#' Build Events Context String
#'
#' Creates a context string with upcoming events for the LLM.
#'
#' @param events Data frame of events
#'
#' @return Character string with events context
#'
#' @keywords internal
build_events_context <- function(events) {
 if (is.null(events) || nrow(events) == 0) {
    return("")
  }

  events_text <- apply(events, 1, function(e) {
    time_str <- if (isTRUE(as.logical(e["all_day"]))) {
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

  paste(
    "\n\nUpcoming events:\n",
    paste(events_text, collapse = "\n")
  )
}

#' Create Calendar Chat Instance
#'
#' Initialize an ellmer chat with calendar context and optional event creation tool.
#'
#' @param events Current events data
#' @param calendars Current calendars list
#' @param can_create_events Logical. Whether to enable event creation tool.
#' @param on_event_created Callback function to run after event creation.
#'
#' @return An ellmer chat object
#'
#' @keywords internal
create_calendar_chat <- function(events, calendars,
                                  can_create_events = TRUE,
                                  on_event_created = NULL) {
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

  # Build calendar list for tool context
  calendar_list <- if (!is.null(calendars) && nrow(calendars) > 0) {
    paste(
      "Available calendars:",
      paste(sprintf("- %s (id: %s)", calendars$name, calendars$id), collapse = "\n")
    )
  } else {
    "Available calendars: primary"
  }

  # Capabilities depend on whether event creation is enabled
  capabilities <- if (can_create_events) {
    "Your capabilities:
    - Answer questions about the user's schedule
    - Summarize upcoming events
    - Identify scheduling conflicts
    - Find free time slots
    - Provide reminders about important events
    - CREATE NEW EVENTS when the user asks (use the create_calendar_event tool)"
  } else {
    "Your capabilities:
    - Answer questions about the user's schedule
    - Summarize upcoming events
    - Identify scheduling conflicts
    - Find free time slots
    - Provide reminders about important events
    Note: Event creation is not available in demo mode."
  }

  system_prompt <- glue::glue("
    You are a helpful family calendar assistant for the Skylight calendar app.

    Current date: {format(today, '%A, %B %d, %Y')}

    Calendar context:
    {events_summary}

    {calendar_list}

    {capabilities}

    Guidelines:
    - Be concise and friendly
    - Use natural language for dates (e.g., 'tomorrow' instead of the date)
    - Format times in a readable way (e.g., '2:30 PM')
    - If you don't have enough information, ask for clarification
    - Focus on being helpful for a family with busy schedules
    - When creating events, confirm the details with a brief summary after creation
  ")

  # Create ellmer chat
  chat_obj <- ellmer::chat_claude(
    system_prompt = system_prompt,
    model = "claude-sonnet-4-20250514"
  )

  # Register event creation tool if enabled

  if (can_create_events) {
    # Get primary calendar ID for default
    primary_calendar <- if (!is.null(calendars) && nrow(calendars) > 0) {
      primary_idx <- which(calendars$primary)
      if (length(primary_idx) > 0) calendars$id[primary_idx[1]] else "primary"
    } else {
      "primary"
    }

    # Create the event creation tool
    event_tool <- create_event_tool(
      default_calendar = primary_calendar,
      on_success = on_event_created
    )

    chat_obj$register_tool(event_tool)
  }

  chat_obj
}

#' Create Event Tool for Chat
#'
#' Creates an ellmer tool definition for calendar event creation.
#'
#' @param default_calendar Default calendar ID to use
#' @param on_success Callback to run on successful event creation
#'
#' @return An ellmer tool definition
#'
#' @keywords internal
create_event_tool <- function(default_calendar = "primary", on_success = NULL) {
  # The actual function that creates events

  create_event_fn <- function(title, start_datetime, end_datetime = NULL,
                              all_day = FALSE, calendar_id = NULL,
                              location = NULL, description = NULL) {
    # Use default calendar if not specified
    if (is.null(calendar_id) || calendar_id == "") {
      calendar_id <- default_calendar
    }

    # Parse datetime strings
    start <- tryCatch({
      if (all_day) {
        as.Date(start_datetime)
      } else {
        lubridate::ymd_hm(start_datetime, tz = Sys.timezone())
      }
    }, error = function(e) {
      stop("Could not parse start time '", start_datetime, "'. ",
           "Use format: YYYY-MM-DD HH:MM (e.g., 2025-01-15 14:30)")
    })

    end <- if (!is.null(end_datetime) && end_datetime != "") {
      tryCatch({
        if (all_day) {
          as.Date(end_datetime)
        } else {
          lubridate::ymd_hm(end_datetime, tz = Sys.timezone())
        }
      }, error = function(e) {
        # Default to 1 hour later for timed events
        if (all_day) start + 1 else start + 3600
      })
    } else {
      # Default duration
      if (all_day) start + 1 else start + 3600
    }

    # Call the actual create_event function
    result <- create_event(
      title = title,
      start = start,
      end = end,
      calendar_id = calendar_id,
      description = description,
      location = location,
      all_day = all_day
    )

    if (result$success) {
      # Run callback if provided
      if (!is.null(on_success)) {
        on_success()
      }

      # Format success message
      time_str <- if (all_day) {
        format(as.Date(start), "%A, %B %d, %Y")
      } else {
        format(start, "%A, %B %d at %I:%M %p")
      }

      paste0(
        "Successfully created event '", title, "' for ", time_str, ".",
        if (!is.null(location) && location != "") paste0(" Location: ", location) else ""
      )
    } else {
      paste0("Failed to create event: ", result$error)
    }
  }

  # Define the tool with ellmer
  ellmer::tool(
    create_event_fn,
    name = "create_calendar_event",
    description = paste(
      "Create a new event on the user's Google Calendar.",
      "Use this when the user asks to add, create, or schedule an event.",
      "Always confirm the event details after successful creation."
    ),
    arguments = list(
      title = ellmer::type_string(
        "The event title/name (e.g., 'Soccer Practice', 'Doctor Appointment')",
        required = TRUE
      ),
      start_datetime = ellmer::type_string(
        paste(
          "Start date and time in format 'YYYY-MM-DD HH:MM' (e.g., '2025-01-15 14:30').",
          "For all-day events, use just the date 'YYYY-MM-DD'.",
          "Convert relative dates like 'tomorrow' or 'next Tuesday' to actual dates."
        ),
        required = TRUE
      ),
      end_datetime = ellmer::type_string(
        paste(
          "End date and time in format 'YYYY-MM-DD HH:MM'.",
          "Optional - defaults to 1 hour after start for timed events,",
          "or next day for all-day events."
        ),
        required = FALSE
      ),
      all_day = ellmer::type_boolean(
        "Whether this is an all-day event (no specific time)",
        required = FALSE
      ),
      calendar_id = ellmer::type_string(
        "The calendar ID to add the event to. Leave empty to use the primary calendar.",
        required = FALSE
      ),
      location = ellmer::type_string(
        "Optional location for the event",
        required = FALSE
      ),
      description = ellmer::type_string(
        "Optional description or notes for the event",
        required = FALSE
      )
    )
  )
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
    # Log but continue - chat history is non-critical functionality
    warning(
      "Failed to save chat message to database: ", conditionMessage(e),
      "\nChat will continue but history will not be preserved.",
      call. = FALSE
    )
  })
}
