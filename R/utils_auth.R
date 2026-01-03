#' Authenticate with Google Calendar
#'
#' Initiates the OAuth 2.0 flow for Google Calendar access.
#'
#' @param email Optional email address to use for authentication.
#' @param cache Logical. Whether to cache the OAuth token. Defaults to `TRUE`.
#' @param use_oob Logical. Whether to use out-of-band authentication.
#'   Useful for non-interactive sessions.
#'
#' @return Invisibly returns `TRUE` on success.
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#'   calendar_auth()
#' }
calendar_auth <- function(email = NULL, cache = TRUE, use_oob = FALSE) {
  # Get credentials from environment
  client_id <- Sys.getenv("GOOGLE_CLIENT_ID")
  client_secret <- Sys.getenv("GOOGLE_CLIENT_SECRET")

  if (nchar(client_id) == 0 || nchar(client_secret) == 0) {
    stop(
      "Google OAuth credentials not found. ",
      "Please set GOOGLE_CLIENT_ID and GOOGLE_CLIENT_SECRET environment variables.",
      call. = FALSE
    )
  }

  # Define OAuth app
  app <- httr2::oauth_client(
    id = client_id,
    secret = client_secret,
    token_url = "https://oauth2.googleapis.com/token",
    name = "skylight-calendar"
  )

  # Define scopes
  scopes <- c(
    "https://www.googleapis.com/auth/calendar",
    "https://www.googleapis.com/auth/userinfo.email"
  )

  # Get cache directory
  cache_dir <- get_token_cache_dir()
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }

  # Perform OAuth flow
  token <- httr2::oauth_flow_auth_code(
    client = app,
    auth_url = "https://accounts.google.com/o/oauth2/v2/auth",
    scope = paste(scopes, collapse = " "),
    redirect_uri = "http://localhost:8080"
  )

  # Cache the token
  if (cache) {
    token_path <- file.path(cache_dir, "google_calendar_token.rds")
    saveRDS(token, token_path)
  }

  # Store in package environment
  pkg_env$token <- token

  invisible(TRUE)
}

#' Clear Google Calendar Authentication
#'
#' Removes cached OAuth tokens and clears the current session.
#'
#' @return Invisibly returns `TRUE`.
#'
#' @export
#'
#' @examples
#' calendar_deauth()
calendar_deauth <- function() {
  # Clear from package environment
  pkg_env$token <- NULL

  # Remove cached token file
  token_path <- file.path(get_token_cache_dir(), "google_calendar_token.rds")
  if (file.exists(token_path)) {
    file.remove(token_path)
  }

  invisible(TRUE)
}

#' Check Authentication Status
#'
#' Checks whether the user is currently authenticated with Google Calendar.
#'
#' @return Logical. `TRUE` if authenticated, `FALSE` otherwise.
#'
#' @export
#'
#' @examples
#' is_authenticated()
is_authenticated <- function() {
 # If credentials aren't configured, we're not authenticated
  client_id <- Sys.getenv("GOOGLE_CLIENT_ID")
  client_secret <- Sys.getenv("GOOGLE_CLIENT_SECRET")

  if (nchar(client_id) == 0 || nchar(client_secret) == 0) {
    return(FALSE)
  }

  tryCatch({
    token <- get_token()
    !is.null(token)
  }, error = function(e) {
    FALSE
  })
}

#' Get Current User Email
#'
#' Retrieves the email address of the authenticated user.
#'
#' @return Character string with the email address.
#'
#' @keywords internal
get_auth_email <- function() {
  token <- get_token()
  if (is.null(token)) {
    stop("Not authenticated. Call calendar_auth() first.", call. = FALSE)
  }

  # Fetch user info
  resp <- httr2::request("https://www.googleapis.com/oauth2/v2/userinfo") |>
    httr2::req_auth_bearer_token(token$access_token) |>
    httr2::req_perform()

  user_info <- httr2::resp_body_json(resp)
  user_info$email %||% "Unknown"
}

#' Get OAuth Token
#'
#' Retrieves the current OAuth token, refreshing if necessary.
#'
#' @return An OAuth token object, or NULL if not authenticated.
#'
#' @keywords internal
get_token <- function() {
  # Check package environment first
  if (!is.null(pkg_env$token)) {
    token <- pkg_env$token

    # Check if token needs refresh
    if (token_needs_refresh(token)) {
      token <- refresh_token(token)
      pkg_env$token <- token
    }

    return(token)
  }

  # Try to load from cache
  token_path <- file.path(get_token_cache_dir(), "google_calendar_token.rds")
  if (file.exists(token_path)) {
    token <- readRDS(token_path)

    # Check if token needs refresh
    if (token_needs_refresh(token)) {
      token <- refresh_token(token)
      saveRDS(token, token_path)
    }

    pkg_env$token <- token
    return(token)
  }

  NULL
}

#' Check if Token Needs Refresh
#'
#' @param token An OAuth token object
#'
#' @return Logical. TRUE if token should be refreshed.
#'
#' @keywords internal
token_needs_refresh <- function(token) {
  if (is.null(token$expires_at)) {
    return(FALSE)
  }

  # Refresh if expires within 5 minutes
  expires_at <- as.POSIXct(token$expires_at, origin = "1970-01-01")
  expires_at - Sys.time() < as.difftime(5, units = "mins")
}

#' Refresh OAuth Token
#'
#' @param token An OAuth token object
#'
#' @return A refreshed OAuth token object
#'
#' @keywords internal
refresh_token <- function(token) {
  client_id <- Sys.getenv("GOOGLE_CLIENT_ID")
  client_secret <- Sys.getenv("GOOGLE_CLIENT_SECRET")

  app <- httr2::oauth_client(
    id = client_id,
    secret = client_secret,
    token_url = "https://oauth2.googleapis.com/token",
    name = "skylight-calendar"
  )

  httr2::oauth_flow_refresh(app, refresh_token = token$refresh_token)
}

#' Get Token Cache Directory
#'
#' @return Path to the token cache directory
#'
#' @keywords internal
get_token_cache_dir <- function() {
  cache_dir <- Sys.getenv("SKYLIGHT_CACHE_DIR")
  if (nchar(cache_dir) == 0) {
    cache_dir <- file.path(
      rappdirs::user_cache_dir("skylight"),
      "tokens"
    )
  }
  cache_dir
}

# Package environment for storing runtime state
pkg_env <- new.env(parent = emptyenv())
