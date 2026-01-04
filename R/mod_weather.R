#' Weather Widget Module UI
#'
#' A compact weather display for the navbar showing current conditions.
#' Clicking opens a modal with today's weather and 5-day forecast.
#'
#' @param id Module namespace ID
#'
#' @return A Shiny UI definition
#'
#' @keywords internal
mod_weather_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::div(
    id = ns("weather_widget"),
    class = "weather-widget weather-widget-clickable d-flex align-items-center",
    role = "button",
    tabindex = "0",
    `aria-label` = "Click to view weather forecast",
    shiny::uiOutput(ns("weather_display"))
  )
}

#' Weather Widget Module Server
#'
#' Fetches and displays weather data from OpenWeatherMap API.
#' Supports browser geolocation for automatic location detection.
#'
#' @param id Module namespace ID
#' @param lat Reactive or static latitude (default: NULL, will use browser/config)
#' @param lon Reactive or static longitude (default: NULL, will use browser/config)
#' @param root_session The root Shiny session (for accessing global inputs)
#'
#' @keywords internal
mod_weather_server <- function(id, lat = NULL, lon = NULL, root_session = NULL) {

  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Get root session for accessing global inputs (browser_geolocation)
    root <- root_session %||% shiny::getDefaultReactiveDomain()

    # Weather data cache (refresh every 10 minutes)
    weather_cache <- shiny::reactiveVal(NULL)
    forecast_cache <- shiny::reactiveVal(NULL)
    last_fetch <- shiny::reactiveVal(NULL)
    last_forecast_fetch <- shiny::reactiveVal(NULL)
    cache_duration <- 600  # 10 minutes in seconds

    # Browser geolocation storage
    browser_location <- shiny::reactiveVal(NULL)

    # Watch for browser geolocation updates
    shiny::observe({
      geo <- root$input$browser_geolocation
      if (!is.null(geo) && !is.null(geo$lat) && !is.null(geo$lon)) {
        # Check if location actually changed before invalidating cache
        current <- shiny::isolate(browser_location())
        location_changed <- is.null(current) ||
          abs(current$lat - geo$lat) > 0.01 ||
          abs(current$lon - geo$lon) > 0.01

        browser_location(list(lat = geo$lat, lon = geo$lon, source = geo$source))

        # Only invalidate cache if location meaningfully changed
        if (location_changed && !is.null(current)) {
          weather_cache(NULL)
          forecast_cache(NULL)
        }
      }
    })

    # Get coordinates with priority: browser > env vars > defaults
    get_coords <- function() {
      # Priority 1: Explicit lat/lon parameters
      if (!is.null(lat) && !is.null(lon)) {
        lat_val <- if (is.reactive(lat)) lat() else lat
        lon_val <- if (is.reactive(lon)) lon() else lon
        return(list(lat = lat_val, lon = lon_val))
      }

      # Priority 2: Browser geolocation
      browser_loc <- browser_location()
      if (!is.null(browser_loc)) {
        return(list(lat = browser_loc$lat, lon = browser_loc$lon))
      }

      # Priority 3: Environment variables
      env_lat <- Sys.getenv("SKYLIGHT_WEATHER_LAT", "")
      env_lon <- Sys.getenv("SKYLIGHT_WEATHER_LON", "")
      if (env_lat != "" && env_lon != "") {
        return(list(
          lat = as.numeric(env_lat),
          lon = as.numeric(env_lon)
        ))
      }

      # Priority 4: Default (San Francisco)
      list(lat = 37.7749, lon = -122.4194)
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
          temp_min = round(data$main$temp_min),
          temp_max = round(data$main$temp_max),
          feels_like = round(data$main$feels_like),
          humidity = data$main$humidity,
          wind_speed = round(data$wind$speed),
          description = data$weather[[1]]$description,
          icon = data$weather[[1]]$icon,
          city = data$name,
          sunrise = data$sys$sunrise,
          sunset = data$sys$sunset
        )
      }, error = function(e) {
        list(
          success = FALSE,
          error = conditionMessage(e)
        )
      })
    }

    # Fetch 5-day forecast from OpenWeatherMap
    fetch_forecast <- function() {
      api_key <- Sys.getenv("OPENWEATHERMAP_API_KEY")

      if (api_key == "") {
        return(list(success = FALSE, error = "No API key configured"))
      }

      coords <- get_coords()

      tryCatch({
        resp <- httr2::request("https://api.openweathermap.org/data/2.5/forecast") |>
          httr2::req_url_query(
            lat = coords$lat,
            lon = coords$lon,
            appid = api_key,
            units = "imperial"
          ) |>
          httr2::req_timeout(10) |>
          httr2::req_perform()

        data <- httr2::resp_body_json(resp)

        # Process forecast data - group by day and get daily high/low
        forecast_list <- data$list
        daily <- list()

        for (item in forecast_list) {
          date <- as.Date(as.POSIXct(item$dt, origin = "1970-01-01"))
          date_str <- as.character(date)

          if (is.null(daily[[date_str]])) {
            daily[[date_str]] <- list(
              date = date,
              temp_min = item$main$temp_min,
              temp_max = item$main$temp_max,
              icon = item$weather[[1]]$icon,
              description = item$weather[[1]]$description
            )
          } else {
            daily[[date_str]]$temp_min <- min(daily[[date_str]]$temp_min, item$main$temp_min)
            daily[[date_str]]$temp_max <- max(daily[[date_str]]$temp_max, item$main$temp_max)
          }
        }

        # Convert to list and take first 5 days
        days <- lapply(names(daily)[1:min(5, length(daily))], function(d) {
          day <- daily[[d]]
          day$temp_min <- round(day$temp_min)
          day$temp_max <- round(day$temp_max)
          day
        })

        list(success = TRUE, days = days)
      }, error = function(e) {
        list(success = FALSE, error = conditionMessage(e))
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

    # Reactive forecast data with caching
    forecast_data <- shiny::reactive({
      now <- Sys.time()
      cached <- forecast_cache()
      last <- last_forecast_fetch()

      if (!is.null(cached) && !is.null(last)) {
        elapsed <- as.numeric(difftime(now, last, units = "secs"))
        if (elapsed < cache_duration) {
          return(cached)
        }
      }

      data <- fetch_forecast()
      forecast_cache(data)
      last_forecast_fetch(now)
      data
    })

    # Auto-refresh every 10 minutes
    shiny::observe({
      shiny::invalidateLater(cache_duration * 1000)
      weather_data()
    })

    # Handle widget click - open modal via JavaScript
    shiny::observe({
      # Add click handler via JavaScript
      shiny::insertUI(
        selector = "head",
        where = "beforeEnd",
        ui = htmltools::tags$script(htmltools::HTML(sprintf("
          $(document).on('click', '#%s', function() {
            Shiny.setInputValue('%s', Date.now());
          });
        ", ns("weather_widget"), ns("weather_click")))),
        immediate = TRUE
      )
    }) |> shiny::bindEvent(TRUE, once = TRUE)

    # Show modal when widget is clicked
    shiny::observeEvent(input$weather_click, {
      weather <- weather_data()
      forecast <- forecast_data()
      browser_loc <- browser_location()

      # Determine location source for display
      location_source <- if (!is.null(browser_loc)) {
        if (browser_loc$source == "browser") {
          "Using your location"
        } else if (browser_loc$source == "cache") {
          "Using cached location"
        } else {
          "Using default location"
        }
      } else {
        "Using default location"
      }

      shiny::showModal(
        shiny::modalDialog(
          title = htmltools::div(
            class = "d-flex align-items-center gap-2",
            bsicons::bs_icon(get_weather_icon(weather$icon), size = "1.5rem"),
            htmltools::span(
              if (weather$success) paste("Weather in", weather$city) else "Weather"
            )
          ),
          size = "l",
          easyClose = TRUE,
          footer = htmltools::div(
            class = "d-flex justify-content-between align-items-center w-100",
            htmltools::div(
              class = "d-flex align-items-center gap-2 text-muted small",
              bsicons::bs_icon("geo-alt", size = "0.9rem"),
              htmltools::span(location_source),
              htmltools::tags$button(
                type = "button",
                class = "btn btn-link btn-sm p-0 ms-1",
                onclick = "Shiny.setInputValue('refresh_location', Date.now()); localStorage.removeItem('skylight_user_location');",
                title = "Update location",
                bsicons::bs_icon("arrow-clockwise", size = "0.9rem")
              )
            ),
            shiny::modalButton("Close")
          ),

          # Modal content
          if (!weather$success) {
            htmltools::div(
              class = "text-center text-muted py-4",
              bsicons::bs_icon("cloud-slash", size = "3rem"),
              htmltools::p(class = "mt-3", "Weather data unavailable"),
              htmltools::p(class = "small", weather$error %||% "Check your API key configuration")
            )
          } else {
            htmltools::div(
              class = "weather-modal-content",

              # Current weather card
              htmltools::div(
                class = "weather-current-card p-4 rounded-3 mb-4",
                style = "background: linear-gradient(135deg, var(--bs-primary) 0%, color-mix(in sRGB, var(--bs-primary) 70%, black) 100%); color: white;",
                htmltools::div(
                  class = "d-flex justify-content-between align-items-start",
                  htmltools::div(
                    htmltools::div(class = "display-4 fw-bold", paste0(weather$temp, "\u00B0F")),
                    htmltools::div(class = "fs-5 opacity-75", tools::toTitleCase(weather$description)),
                    htmltools::div(class = "mt-2 opacity-75", paste0("Feels like ", weather$feels_like, "\u00B0F"))
                  ),
                  htmltools::div(
                    class = "text-end",
                    bsicons::bs_icon(get_weather_icon(weather$icon), size = "4rem"),
                    htmltools::div(class = "mt-2 small opacity-75", format(Sys.Date(), "%A, %B %d"))
                  )
                ),
                htmltools::hr(class = "my-3 opacity-25"),
                htmltools::div(
                  class = "d-flex justify-content-around text-center",
                  htmltools::div(
                    bsicons::bs_icon("thermometer-half", size = "1.2rem"),
                    htmltools::div(class = "small opacity-75", "High / Low"),
                    htmltools::div(class = "fw-semibold", paste0(weather$temp_max, "\u00B0 / ", weather$temp_min, "\u00B0"))
                  ),
                  htmltools::div(
                    bsicons::bs_icon("droplet", size = "1.2rem"),
                    htmltools::div(class = "small opacity-75", "Humidity"),
                    htmltools::div(class = "fw-semibold", paste0(weather$humidity, "%"))
                  ),
                  htmltools::div(
                    bsicons::bs_icon("wind", size = "1.2rem"),
                    htmltools::div(class = "small opacity-75", "Wind"),
                    htmltools::div(class = "fw-semibold", paste0(weather$wind_speed, " mph"))
                  )
                )
              ),

              # 5-day forecast
              htmltools::div(
                class = "weather-forecast",
                htmltools::h6(class = "text-muted mb-3", "5-Day Forecast"),
                if (!forecast$success) {
                  htmltools::div(class = "text-muted small", "Forecast unavailable")
                } else {
                  htmltools::div(
                    class = "row g-2",
                    lapply(forecast$days, function(day) {
                      day_name <- if (day$date == Sys.Date()) {
                        "Today"
                      } else if (day$date == Sys.Date() + 1) {
                        "Tomorrow"
                      } else {
                        format(day$date, "%a")
                      }

                      htmltools::div(
                        class = "col",
                        htmltools::div(
                          class = "forecast-day-card text-center p-2 rounded-2 border",
                          htmltools::div(class = "small fw-semibold", day_name),
                          htmltools::div(class = "my-2", bsicons::bs_icon(get_weather_icon(day$icon), size = "1.5rem")),
                          htmltools::div(
                            class = "small",
                            htmltools::span(class = "fw-semibold", paste0(day$temp_max, "\u00B0")),
                            htmltools::span(class = "text-muted ms-1", paste0(day$temp_min, "\u00B0"))
                          )
                        )
                      )
                    })
                  )
                }
              )
            )
          }
        )
      )
    }, ignoreInit = TRUE)

    # Handle location refresh request
    shiny::observeEvent(root$input$refresh_location, {
      # Clear caches to force re-fetch
      browser_location(NULL)
      weather_cache(NULL)
      forecast_cache(NULL)
      # Request new geolocation from browser
      session$sendCustomMessage("refresh-geolocation", list())
    }, ignoreInit = TRUE)

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
