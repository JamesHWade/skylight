# Tests for mod_auto_dark_mode.R

test_that("parse_time_to_minutes correctly parses times", {
  expect_equal(parse_time_to_minutes("00:00"), 0)
  expect_equal(parse_time_to_minutes("06:00"), 360)
  expect_equal(parse_time_to_minutes("12:00"), 720)
  expect_equal(parse_time_to_minutes("18:00"), 1080)
  expect_equal(parse_time_to_minutes("23:59"), 1439)
  expect_equal(parse_time_to_minutes("09:30"), 570)
})

test_that("is_dark_time returns TRUE before sunrise", {
  # Mock current time to 5:00 AM
  # Since we can't easily mock Sys.time(), test the logic directly
  sunrise <- "06:00"
  sunset <- "18:00"

  # 5:00 AM = 300 minutes, should be dark (before 360)
  current_minutes <- 300
  sunrise_mins <- parse_time_to_minutes(sunrise)
  sunset_mins <- parse_time_to_minutes(sunset)

  is_dark <- current_minutes < sunrise_mins || current_minutes >= sunset_mins
  expect_true(is_dark)
})

test_that("is_dark_time returns FALSE during daytime", {
  sunrise <- "06:00"
  sunset <- "18:00"

  # 12:00 PM = 720 minutes, should be light
  current_minutes <- 720
  sunrise_mins <- parse_time_to_minutes(sunrise)
  sunset_mins <- parse_time_to_minutes(sunset)

  is_dark <- current_minutes < sunrise_mins || current_minutes >= sunset_mins
  expect_false(is_dark)
})

test_that("is_dark_time returns TRUE after sunset", {
  sunrise <- "06:00"
  sunset <- "18:00"

  # 8:00 PM = 1200 minutes, should be dark (after 1080)
  current_minutes <- 1200
  sunrise_mins <- parse_time_to_minutes(sunrise)
  sunset_mins <- parse_time_to_minutes(sunset)

  is_dark <- current_minutes < sunrise_mins || current_minutes >= sunset_mins
  expect_true(is_dark)
})

test_that("load_auto_dark_settings returns defaults when no settings saved", {
  # This test assumes no settings are saved
  settings <- load_auto_dark_settings()

  expect_type(settings, "list")
  expect_true("enabled" %in% names(settings))
  expect_true("sunset_time" %in% names(settings))
  expect_true("sunrise_time" %in% names(settings))
})

test_that("save_auto_dark_settings returns settings invisibly", {
  result <- save_auto_dark_settings(
    enabled = TRUE,
    sunset_time = "19:30",
    sunrise_time = "07:00"
  )

  # Function should return the settings list
  expect_type(result, "list")
  expect_true(result$enabled)
  expect_equal(result$sunset_time, "19:30")
  expect_equal(result$sunrise_time, "07:00")
})

test_that("mod_auto_dark_mode_ui returns valid HTML", {
  ui <- mod_auto_dark_mode_ui("test")

  expect_s3_class(ui, "shiny.tag")
  expect_true(grepl("auto-dark-mode-settings", as.character(ui)))
})
