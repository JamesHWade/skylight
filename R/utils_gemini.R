#' Gemini API Utilities for Icon Generation
#'
#' Functions for generating custom icons using Google's Gemini API.
#'
#' @name utils_gemini
#' @keywords internal
NULL

#' Generate Icon for Chore or Event
#'
#' Uses Gemini's image generation (Nano Banana) to create a custom icon.
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

  # Set the API key for gemini.R
  gemini.R::setAPI(api_key)

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

  # Create temp file for output
  temp_file <- tempfile(fileext = ".png")
  on.exit(unlink(temp_file), add = TRUE)

  tryCatch({
    result_path <- gemini.R::nano_banana(
      prompt = prompt,
      type = "generate",
      output_path = temp_file
    )

    if (is.null(result_path) || !file.exists(result_path)) {
      return(list(success = FALSE, error = "Image generation failed"))
    }

    # Read the file and convert to base64
    raw_data <- readBin(result_path, "raw", file.info(result_path)$size)
    base64_data <- base64enc::base64encode(raw_data)

    list(success = TRUE, base64 = base64_data)
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
