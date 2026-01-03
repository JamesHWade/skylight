# Tests for utils_dates.R

test_that("format_time works with 12-hour format", {
  dt <- as.POSIXct("2025-01-15 14:30:00")
  result <- format_time(dt, "12h")
  expect_match(result, "2:30 PM")
})
test_that("format_time works with 24-hour format", {
  dt <- as.POSIXct("2025-01-15 14:30:00")
  result <- format_time(dt, "24h")
  expect_equal(result, "14:30")
})

test_that("format_time handles midnight and noon", {
  midnight <- as.POSIXct("2025-01-15 00:00:00")
  noon <- as.POSIXct("2025-01-15 12:00:00")

  expect_match(format_time(midnight, "12h"), "12:00 AM")
  expect_match(format_time(noon, "12h"), "12:00 PM")
  expect_equal(format_time(midnight, "24h"), "00:00")
  expect_equal(format_time(noon, "24h"), "12:00")
})

test_that("format_time_range creates correct ranges", {
  start <- as.POSIXct("2025-01-15 09:00:00")
  end <- as.POSIXct("2025-01-15 10:30:00")

  result <- format_time_range(start, end, "12h")
  expect_match(result, "9:00 AM")
  expect_match(result, "10:30 AM")
  expect_match(result, "-")
})

test_that("format_duration handles minutes only", {
  start <- as.POSIXct("2025-01-15 09:00:00")
  end <- as.POSIXct("2025-01-15 09:30:00")

  result <- format_duration(start, end)
  expect_equal(result, "30m")
})

test_that("format_duration handles hours only", {
  start <- as.POSIXct("2025-01-15 09:00:00")
  end <- as.POSIXct("2025-01-15 11:00:00")

  result <- format_duration(start, end)
  expect_equal(result, "2h")
})

test_that("format_duration handles hours and minutes", {
  start <- as.POSIXct("2025-01-15 09:00:00")
  end <- as.POSIXct("2025-01-15 10:45:00")

  result <- format_duration(start, end)
  expect_equal(result, "1h 45m")
})

test_that("get_week_dates returns 7 days starting from Sunday", {
  # A Wednesday in January 2025
  date <- as.Date("2025-01-15")
  result <- get_week_dates(date, week_start = 0)

  expect_length(result, 7)
  expect_equal(format(result[1], "%A"), "Sunday")
  expect_equal(format(result[7], "%A"), "Saturday")
  # The date should be in the range

  expect_true(date %in% result)
})

test_that("get_week_dates returns 7 days starting from Monday", {
  date <- as.Date("2025-01-15")
  result <- get_week_dates(date, week_start = 1)

  expect_length(result, 7)
  expect_equal(format(result[1], "%A"), "Monday")
  expect_equal(format(result[7], "%A"), "Sunday")
  expect_true(date %in% result)
})

test_that("get_month_dates returns proper calendar grid", {
  # January 2025 starts on Wednesday
  date <- as.Date("2025-01-15")
  result <- get_month_dates(date, week_start = 0)

  # Should have 35 days (5 weeks) for Jan 2025
  expect_true(length(result) %in% c(35, 42))

  # First day should be a Sunday
  expect_equal(format(result[1], "%A"), "Sunday")

  # Should contain all days of January
  jan_days <- seq(as.Date("2025-01-01"), as.Date("2025-01-31"), by = "day")
  expect_true(all(jan_days %in% result))
})

test_that("is_weekend correctly identifies weekends", {
  saturday <- as.Date("2025-01-18")
  sunday <- as.Date("2025-01-19")
  monday <- as.Date("2025-01-20")

  expect_true(is_weekend(saturday))
  expect_true(is_weekend(sunday))
  expect_false(is_weekend(monday))
})

test_that("is_current_month correctly identifies month", {
  jan_15 <- as.Date("2025-01-15")
  jan_31 <- as.Date("2025-01-31")
  feb_01 <- as.Date("2025-02-01")

  expect_true(is_current_month(jan_31, jan_15))
  expect_false(is_current_month(feb_01, jan_15))
})

test_that("parse_natural_date handles 'today'", {
  result <- parse_natural_date("today")
  expect_equal(result, Sys.Date())
})

test_that("parse_natural_date handles 'tomorrow'", {
  result <- parse_natural_date("tomorrow")
  expect_equal(result, Sys.Date() + 1)
})

test_that("parse_natural_date handles 'yesterday'", {
  result <- parse_natural_date("yesterday")
  expect_equal(result, Sys.Date() - 1)
})

test_that("parse_natural_date handles day names", {
  result <- parse_natural_date("monday")
  expect_s3_class(result, "Date")

  # Result should be within next 7 days
  expect_true(result > Sys.Date())
  expect_true(result <= Sys.Date() + 7)

  # Should be a Monday
  expect_equal(format(result, "%A"), "Monday")
})

test_that("parse_natural_date handles case insensitivity", {
  expect_equal(parse_natural_date("TODAY"), Sys.Date())
  expect_equal(parse_natural_date("Tomorrow"), Sys.Date() + 1)
  expect_equal(parse_natural_date("  today  "), Sys.Date())
})
