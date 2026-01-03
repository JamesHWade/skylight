#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import shiny
#' @importFrom bslib bs_theme page_navbar nav_panel card card_header card_body
#' @importFrom bslib value_box layout_columns layout_sidebar sidebar
#' @importFrom htmltools tags tagList HTML css
#' @importFrom rlang .data .env
#' @importFrom glue glue
## usethis namespace: end
NULL

#' Skylight Calendar
#'
#' A beautiful, always-on calendar display app for iPad with Google Calendar
#' integration and AI-powered natural language interaction.
#'
#' @section Main Functions:
#' \describe{
#'   \item{[run_app()]}{Launch the Shiny application}
#'   \item{[calendar_auth()]}{Authenticate with Google Calendar}
#'   \item{[get_events()]}{Fetch calendar events}
#'   \item{[get_calendars()]}{List available calendars
#' }
#' }
#'
#' @section Package Options:
#' \describe{
#'   \item{`skylight.cache_timeout`}{Cache timeout in seconds (default: 300)}
#'   \item{`skylight.refresh_interval`}{UI refresh interval in ms (default: 60000)}
#' }
#'
#' @name skylight-package
#' @aliases skylight
NULL
