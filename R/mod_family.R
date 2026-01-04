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
#' Creates the form fields for adding/editing a member.
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

  # Common avatar options
  avatar_choices <- c(
    "\U0001F464",  # person silhouette
    "\U0001F466",  # boy
    "\U0001F467",  # girl
    "\U0001F468",  # man
    "\U0001F469",  # woman
    "\U0001F474",  # old man
    "\U0001F475",  # old woman
    "\U0001F476",  # baby
    "\U0001F9D1",  # person
    "\U0001F431",  # cat face
    "\U0001F436"   # dog face
  )

  # Color palette matching app theme
  color_choices <- c(
    "#74B9FF",  # Soft blue
    "#FF7675",  # Coral
    "#55EFC4",  # Mint
    "#A29BFE",  # Lavender
    "#FFEAA7",  # Sunshine
    "#FD79A8",  # Pink
    "#00B894",  # Green
    "#E17055",  # Orange
    "#6C5CE7",  # Purple
    "#81ECEC"   # Cyan
  )

  htmltools::tagList(
    htmltools::div(
      class = "mb-3",
      htmltools::tags$label(class = "form-label", `for` = ns("member_name"), "Name *"),
      htmltools::tags$input(
        type = "text",
        class = "form-control",
        id = ns("member_name"),
        value = name_val,
        placeholder = "Enter name"
      )
    ),
    htmltools::div(
      class = "mb-3",
      htmltools::tags$label(class = "form-label", `for` = ns("member_display_name"), "Nickname (optional)"),
      htmltools::tags$input(
        type = "text",
        class = "form-control",
        id = ns("member_display_name"),
        value = display_val,
        placeholder = "Display name"
      )
    ),
    htmltools::div(
      class = "mb-3",
      htmltools::tags$label(class = "form-label", "Avatar"),
      htmltools::div(
        class = "avatar-picker d-flex flex-wrap gap-2",
        lapply(avatar_choices, function(emoji) {
          selected <- emoji == avatar_val
          htmltools::tags$button(
            type = "button",
            class = paste("btn avatar-option", if (selected) "btn-primary" else "btn-outline-secondary"),
            style = "font-size: 1.5rem; width: 3rem; height: 3rem;",
            onclick = sprintf(
              "document.getElementById('%s').value = '%s'; this.parentNode.querySelectorAll('.avatar-option').forEach(b => b.classList.remove('btn-primary')); this.classList.add('btn-primary');",
              ns("member_avatar"), emoji
            ),
            emoji
          )
        }),
        htmltools::tags$input(
          type = "hidden",
          id = ns("member_avatar"),
          value = avatar_val
        )
      )
    ),
    htmltools::div(
      class = "mb-3",
      htmltools::tags$label(class = "form-label", "Color"),
      htmltools::div(
        class = "color-picker d-flex flex-wrap gap-2",
        lapply(color_choices, function(color) {
          selected <- tolower(color) == tolower(color_val)
          htmltools::tags$button(
            type = "button",
            class = paste("btn color-option rounded-circle p-0", if (selected) "ring ring-primary" else ""),
            style = htmltools::css(
              `background-color` = color,
              width = "2rem",
              height = "2rem",
              border = if (selected) "2px solid #333" else "1px solid #ddd"
            ),
            onclick = sprintf(
              "document.getElementById('%s').value = '%s'; this.parentNode.querySelectorAll('.color-option').forEach(b => {b.style.border = '1px solid #ddd';}); this.style.border = '2px solid #333';",
              ns("member_color"), color
            ),
            ""
          )
        }),
        htmltools::tags$input(
          type = "hidden",
          id = ns("member_color"),
          value = color_val
        )
      )
    )
  )
}
