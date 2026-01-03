#' Weather Widget Module UI
#'
#' A compact weather display for the navbar showing current conditions.
#'
#' @param id Module namespace ID
#'
#' @return A Shiny UI definition
#'
#' @keywords internal
mod_weather_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::div(
    class = "weather-widget d-flex align-items-center",
    shiny::uiOutput(ns("weather_display"))
  )
}

#' Weather Widget Module Server
#'
#' Fetches and displays weather data from OpenWeatherMap API.
#'
#' @param id Module namespace ID
#' @param lat Reactive or static latitude (default: NULL, will use config)
#' @param lon Reactive or static longitude (default: NULL, will use config)
#'
#' @keywords internal
mod_weather_server <- function(id, lat = NULL, lon = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    # Weather data cache (refresh every 10 minutes)
    weather_cache <- shiny::reactiveVal(NULL)
    last_fetch <- shiny::reactiveVal(NULL)
    cache_duration <- 600  # 10 minutes in seconds

    # Get coordinates (default to a reasonable location if not configured)
    get_coords <- function() {
      lat_val <- if (is.null(lat)) {
        as.numeric(Sys.getenv("SKYLIGHT_WEATHER_LAT", "37.7749"))  # SF default
      } else if (is.reactive(lat)) {
        lat()
      } else {
        lat
      }

      lon_val <- if (is.null(lon)) {
        as.numeric(Sys.getenv("SKYLIGHT_WEATHER_LON", "-122.4194"))  # SF default
      } else if (is.reactive(lon)) {
        lon()
      } else {
        lon
      }

      list(lat = lat_val, lon = lon_val)
    }

    # Fetch weather data from OpenWeatherMap
    fetch_weather <- function() {
      api_key <- Sys.getenv("OPENWEATHERMAP_API_KEY")

      if (api_key == "") {
        return(list(
          success = FALSE,
          error = "No API key configured"
        ))
      }

      coords <- get_coords()

      tryCatch({
        resp <- httr2::request("https://api.openweathermap.org/data/2.5/weather") |>
          httr2::req_url_query(
            lat = coords$lat,
            lon = coords$lon,
            appid = api_key,
            units = "imperial"  # Use Fahrenheit
          ) |>
          httr2::req_timeout(10) |>
          httr2::req_perform()

        data <- httr2::resp_body_json(resp)

        list(
          success = TRUE,
          temp = round(data$main$temp),
          feels_like = round(data$main$feels_like),
          description = data$weather[[1]]$description,
          icon = data$weather[[1]]$icon,
          city = data$name
        )
      }, error = function(e) {
        list(
          success = FALSE,
          error = conditionMessage(e)
        )
      })
    }

    # Reactive weather data with caching
    weather_data <- shiny::reactive({
      # Check if we should use cache
      now <- Sys.time()
      cached <- weather_cache()
      last <- last_fetch()

      if (!is.null(cached) && !is.null(last)) {
        elapsed <- as.numeric(difftime(now, last, units = "secs"))
        if (elapsed < cache_duration) {
          return(cached)
        }
      }

      # Fetch fresh data
      data <- fetch_weather()
      weather_cache(data)
      last_fetch(now)
      data
    })

    # Auto-refresh every 10 minutes
    shiny::observe({
      shiny::invalidateLater(cache_duration * 1000)
      weather_data()
    })

    # Map OpenWeatherMap icon codes to Bootstrap icons
    get_weather_icon <- function(icon_code) {
      if (is.null(icon_code)) return("cloud")

      # Icon mapping (day/night variants)
      icons <- list(
        "01d" = "sun",           # clear sky day
        "01n" = "moon",          # clear sky night
        "02d" = "cloud-sun",     # few clouds day
        "02n" = "cloud-moon",    # few clouds night
        "03d" = "cloud",         # scattered clouds
        "03n" = "cloud",
        "04d" = "clouds",        # broken clouds
        "04n" = "clouds",
        "09d" = "cloud-drizzle", # shower rain
        "09n" = "cloud-drizzle",
        "10d" = "cloud-rain",    # rain
        "10n" = "cloud-rain",
        "11d" = "cloud-lightning", # thunderstorm
        "11n" = "cloud-lightning",
        "13d" = "snow",          # snow
        "13n" = "snow",
        "50d" = "cloud-haze",    # mist
        "50n" = "cloud-haze"
      )

      icons[[icon_code]] %||% "cloud"
    }

    # Render the weather display
    output$weather_display <- shiny::renderUI({
      data <- weather_data()

      if (!data$success) {
        # Show placeholder when no data available
        return(
          htmltools::div(
            class = "weather-unavailable text-muted",
            bsicons::bs_icon("cloud-slash", size = "1rem")
          )
        )
      }

      icon_name <- get_weather_icon(data$icon)

      htmltools::div(
        class = "weather-content d-flex align-items-center",
        htmltools::div(
          class = "weather-icon me-1",
          bsicons::bs_icon(icon_name, size = "1.1rem")
        ),
        htmltools::div(
          class = "weather-temp",
          paste0(data$temp, "\u00B0")
        ),
        htmltools::div(
          class = "weather-condition d-none d-lg-block ms-1",
          tools::toTitleCase(data$description)
        )
      )
    })

    # Return weather data for potential use by other modules
    weather_data
  })
}
