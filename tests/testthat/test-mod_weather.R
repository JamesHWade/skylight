test_that("mod_weather_ui returns valid UI", {
  ui <- mod_weather_ui("test")
  expect_true(inherits(ui, "shiny.tag"))
  expect_true(grepl("weather-widget", as.character(ui)))
})

test_that("weather module handles missing API key gracefully", {
  # Ensure no API key is set for this test
  withr::local_envvar(OPENWEATHERMAP_API_KEY = "")

  shiny::testServer(mod_weather_server, {
    # The weather_data reactive should return success = FALSE
    data <- weather_data()
    expect_false(data$success)
    expect_equal(data$error, "No API key configured")
  })
})
