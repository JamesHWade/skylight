#' Gemini API Utilities for Icon Generation
#'
#' Functions for generating custom icons using Google's Gemini API.
#'
#' @name utils_gemini
#' @keywords internal
NULL
#' Generate Icon for Chore or Event
#'
#' Uses Gemini's image generation to create a custom icon.
#'
#' @param title The chore or event title.
#' @param type Either "chore" or "event".
#'
#' @return A list with:
#'   - `success`: TRUE if generation succeeded
#'   - `base64`: The base64-encoded PNG image (if success)
#'   - `error`: Error message (if failed)
#'
#' @export
generate_icon <- function(title, type = "chore") {
  api_key <- Sys.getenv("GEMINI_API_KEY")
  if (api_key == "") {
    return(list(success = FALSE, error = "No GEMINI_API_KEY configured"))
  }

  prompt <- sprintf(
    "Create a single 3D cartoon-style icon for a %s called '%s'.
The icon should be:
- Simple and recognizable at small sizes (64x64 pixels)
- Colorful with a playful, family-friendly aesthetic
- On a transparent or white background
- No text, just the icon imagery
Output only the icon image.",
    type, title
  )

  tryCatch({
    # Call Gemini Nano Banana (gemini-2.5-flash-image) directly using httr2
    resp <- httr2::request("https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-flash-image:generateContent") |>
      httr2::req_headers(
        `x-goog-api-key` = api_key,
        `Content-Type` = "application/json"
      ) |>
      httr2::req_body_json(list(
        contents = list(list(
          parts = list(list(text = prompt))
        ))
      )) |>
      httr2::req_perform()

    result <- httr2::resp_body_json(resp)

    # Extract image data from response
    parts <- result$candidates[[1]]$content$parts
    image_part <- NULL
    for (part in parts) {
      if (!is.null(part$inlineData)) {
        image_part <- part$inlineData
        break
      }
    }

    if (is.null(image_part)) {
      return(list(success = FALSE, error = "No image in response"))
    }

    list(success = TRUE, base64 = image_part$data)
  }, error = function(e) {
    list(success = FALSE, error = conditionMessage(e))
  })
}

#' Check if Gemini API is Available
#'
#' @return TRUE if GEMINI_API_KEY is set, FALSE otherwise.
#'
#' @export
gemini_available <- function() {
  Sys.getenv("GEMINI_API_KEY") != ""
}

# =============================================================================
# EVENT ICON STORAGE
# =============================================================================

#' Save Event Icon
#'
#' Stores a generated icon for a calendar event.
#'
#' @param event_id The event ID (from Google Calendar).
#' @param icon_base64 The base64-encoded PNG icon.
#'
#' @return TRUE on success.
#'
#' @export
save_event_icon <- function(event_id, icon_base64) {
  db_execute("
    INSERT INTO event_icons (event_id, icon_base64)
    VALUES (?, ?)
    ON CONFLICT (event_id) DO UPDATE SET
      icon_base64 = EXCLUDED.icon_base64,
      created_at = CURRENT_TIMESTAMP
  ", params = list(event_id, icon_base64))
  invisible(TRUE)
}

#' Get Event Icon
#'
#' Retrieves a stored icon for a calendar event.
#'
#' @param event_id The event ID.
#'
#' @return The base64-encoded icon, or NULL if not found.
#'
#' @export
get_event_icon <- function(event_id) {
  result <- db_query("
    SELECT icon_base64 FROM event_icons WHERE event_id = ?
  ", params = list(event_id))

  if (nrow(result) == 0) return(NULL)
  result$icon_base64[1]
}

#' Delete Event Icon
#'
#' Removes a stored icon for a calendar event.
#'
#' @param event_id The event ID.
#'
#' @return TRUE on success.
#'
#' @export
delete_event_icon <- function(event_id) {
  db_execute("DELETE FROM event_icons WHERE event_id = ?", params = list(event_id))
  invisible(TRUE)
}
