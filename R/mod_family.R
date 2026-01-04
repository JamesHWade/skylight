#' Family Members Module UI
#'
#' UI for managing family members in the chores system.
#'
#' @param id Module namespace ID
#'
#' @return A Shiny UI definition
#'
#' @keywords internal
mod_family_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::div(
    class = "family-module p-2",
    htmltools::div(
      class = "d-flex justify-content-between align-items-center mb-3",
      htmltools::h6(class = "mb-0", "Family Members"),
      shiny::actionButton(
        ns("add_member"),
        "",
        icon = bsicons::bs_icon("person-plus"),
        class = "btn-sm btn-outline-primary"
      )
    ),
    shiny::uiOutput(ns("members_list"))
  )
}

#' Family Members Module Server
#'
#' @param id Module namespace ID
#'
#' @return A reactive returning the list of family members
#'
#' @keywords internal
mod_family_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive for family members data
    members <- shiny::reactiveVal(NULL)

    # Load members on init
    shiny::observe({
      members(get_family_members(active_only = TRUE))
    })

    # Render members list
    output$members_list <- shiny::renderUI({
      m <- members()

      if (is.null(m) || nrow(m) == 0) {
        return(htmltools::div(
          class = "text-muted small text-center py-3",
          htmltools::p("No family members yet."),
          htmltools::p("Add members to start tracking chores!")
        ))
      }

      htmltools::div(
        class = "family-members-list",
        lapply(seq_len(nrow(m)), function(i) {
          member_row(m[i, ], ns)
        })
      )
    })

    # Add member modal
    shiny::observeEvent(input$add_member, {
      shiny::showModal(shiny::modalDialog(
        title = "Add Family Member",
        member_form(ns, NULL),
        footer = htmltools::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(ns("save_new_member"), "Add Member", class = "btn-primary")
        ),
        easyClose = TRUE
      ))
    })

    # Save new member
    shiny::observeEvent(input$save_new_member, {
      name <- trimws(input$member_name)
      if (nchar(name) == 0) {
        shiny::showNotification("Name is required", type = "error")
        return()
      }

      display_name <- trimws(input$member_display_name)
      if (nchar(display_name) == 0) display_name <- NULL

      create_family_member(
        name = name,
        display_name = display_name,
        avatar_emoji = input$member_avatar,
        color = input$member_color
      )

      members(get_family_members(active_only = TRUE))
      shiny::removeModal()
      shiny::showNotification(paste(name, "added!"), type = "message")
    })

    # Edit member - dynamic observer for each member's edit button
    shiny::observe({
      m <- members()
      if (is.null(m)) return()

      lapply(m$id, function(member_id) {
        edit_id <- paste0("edit_", member_id)

        shiny::observeEvent(input[[edit_id]], {
          member <- get_family_member(member_id)
          if (is.null(member)) return()

          shiny::showModal(shiny::modalDialog(
            title = "Edit Family Member",
            member_form(ns, member),
            footer = htmltools::tagList(
              shiny::actionButton(
                ns(paste0("delete_", member_id)),
                "Delete",
                class = "btn-outline-danger me-auto"
              ),
              shiny::modalButton("Cancel"),
              shiny::actionButton(
                ns(paste0("save_edit_", member_id)),
                "Save Changes",
                class = "btn-primary"
              )
            ),
            easyClose = TRUE
          ))
        }, ignoreInit = TRUE, once = FALSE)

        # Save edit handler
        save_edit_id <- paste0("save_edit_", member_id)
        shiny::observeEvent(input[[save_edit_id]], {
          name <- trimws(input$member_name)
          if (nchar(name) == 0) {
            shiny::showNotification("Name is required", type = "error")
            return()
          }

          display_name <- trimws(input$member_display_name)
          if (nchar(display_name) == 0) display_name <- NULL

          update_family_member(
            id = member_id,
            name = name,
            display_name = display_name,
            avatar_emoji = input$member_avatar,
            color = input$member_color
          )

          members(get_family_members(active_only = TRUE))
          shiny::removeModal()
          shiny::showNotification("Member updated!", type = "message")
        }, ignoreInit = TRUE, once = FALSE)

        # Delete handler
        delete_id <- paste0("delete_", member_id)
        shiny::observeEvent(input[[delete_id]], {
          shiny::showModal(shiny::modalDialog(
            title = "Delete Family Member?",
            htmltools::p("This will remove them from the chores system. Their completion history will be preserved."),
            footer = htmltools::tagList(
              shiny::modalButton("Cancel"),
              shiny::actionButton(
                ns(paste0("confirm_delete_", member_id)),
                "Delete",
                class = "btn-danger"
              )
            )
          ))
        }, ignoreInit = TRUE, once = FALSE)

        # Confirm delete handler
        confirm_delete_id <- paste0("confirm_delete_", member_id)
        shiny::observeEvent(input[[confirm_delete_id]], {
          delete_family_member(member_id)
          members(get_family_members(active_only = TRUE))
          shiny::removeModal()
          shiny::showNotification("Member removed", type = "message")
        }, ignoreInit = TRUE, once = FALSE)
      })
    })

    # Return reactive members
    members
  })
}

#' Member Row UI
#'
#' Creates a single member row for the list.
#'
#' @param member A single-row data frame with member info.
#' @param ns Namespace function.
#'
#' @return HTML div
#'
#' @keywords internal
member_row <- function(member, ns) {
  display <- if (!is.null(member$display_name) && !is.na(member$display_name) && nchar(member$display_name) > 0) {
    member$display_name
  } else {
    member$name
  }

  htmltools::div(
    class = "family-member-row d-flex align-items-center py-2 px-2 mb-1 rounded",
    style = htmltools::css(
      `background-color` = paste0(member$color, "15"),
      `border-left` = paste0("3px solid ", member$color)
    ),
    htmltools::span(
      class = "member-avatar me-2",
      style = "font-size: 1.5rem;",
      member$avatar_emoji
    ),
    htmltools::div(
      class = "member-info flex-grow-1",
      htmltools::div(class = "member-display-name fw-medium", display),
      if (display != member$name) {
        htmltools::div(class = "member-name text-muted small", member$name)
      }
    ),
    shiny::actionButton(
      ns(paste0("edit_", member$id)),
      "",
      icon = bsicons::bs_icon("pencil"),
      class = "btn-sm btn-link text-muted"
    )
  )
}

#' Member Form
#'
#' Creates the form fields for adding/editing a member using proper Shiny inputs.
#'
#' @param ns Namespace function.
#' @param member Optional existing member data for editing.
#'
#' @return HTML form elements
#'
#' @keywords internal
member_form <- function(ns, member = NULL) {
  is_edit <- !is.null(member)

  # Default values
  name_val <- if (is_edit) member$name else ""
  display_val <- if (is_edit && !is.na(member$display_name)) member$display_name else ""
  avatar_val <- if (is_edit) member$avatar_emoji else "\U0001F464"
  color_val <- if (is_edit) member$color else "#74B9FF"

  # Common avatar options - named vector for radioButtons
  avatar_choices <- c(
    "\U0001F464" = "\U0001F464",
    "\U0001F466" = "\U0001F466",
    "\U0001F467" = "\U0001F467",
    "\U0001F468" = "\U0001F468",
    "\U0001F469" = "\U0001F469",
    "\U0001F474" = "\U0001F474",
    "\U0001F475" = "\U0001F475",
    "\U0001F476" = "\U0001F476",
    "\U0001F9D1" = "\U0001F9D1",
    "\U0001F431" = "\U0001F431",
    "\U0001F436" = "\U0001F436"
  )

  # Color palette matching app theme - values are hex codes
  color_values <- c("#74B9FF", "#FF7675", "#55EFC4", "#A29BFE", "#FFEAA7",
                    "#FD79A8", "#00B894", "#E17055", "#6C5CE7", "#81ECEC")

  # Create color swatches as choice names (styled spans with background color)
  color_choice_names <- lapply(color_values, function(color) {
    htmltools::span(
      style = htmltools::css(
        `background-color` = color,
        width = "1.5rem",
        height = "1.5rem",
        `border-radius` = "50%",
        display = "inline-block"
      )
    )
  })

  htmltools::tagList(
    shiny::textInput(
      ns("member_name"),
      "Name *",
      value = name_val,
      placeholder = "Enter name"
    ),
    shiny::textInput(
      ns("member_display_name"),
      "Nickname (optional)",
      value = display_val,
      placeholder = "Display name"
    ),
    # Avatar picker using radioButtons (CSS in styles.css)
    shiny::radioButtons(
      ns("member_avatar"),
      "Avatar",
      choices = avatar_choices,
      selected = avatar_val,
      inline = TRUE
    ) |> htmltools::tagAppendAttributes(class = "avatar-radio-picker"),
    # Color picker with styled color swatches (CSS in styles.css)
    shiny::radioButtons(
      ns("member_color"),
      "Color",
      choiceNames = color_choice_names,
      choiceValues = color_values,
      selected = color_val,
      inline = TRUE
    ) |> htmltools::tagAppendAttributes(class = "color-radio-picker")
  )
}
