# Tests for utils_demo.R

test_that("generate_sample_calendars returns expected structure", {
  result <- generate_sample_calendars()

  expect_s3_class(result, "data.frame")
  expect_true(all(c("id", "name", "color", "primary") %in% names(result)))
  expect_equal(nrow(result), 2)
  expect_true("Work" %in% result$name)
  expect_true("Personal" %in% result$name)
})

test_that("generate_sample_calendars has valid colors", {
  result <- generate_sample_calendars()

  # Colors should be hex format
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", result$color)))
})

test_that("generate_sample_calendars has exactly one primary", {
  result <- generate_sample_calendars()
  expect_equal(sum(result$primary), 1)
})

test_that("generate_sample_events returns expected structure", {
  set.seed(42)  # For reproducibility
  result <- generate_sample_events(
    start = as.Date("2025-01-01"),
    end = as.Date("2025-01-07")
  )

  expect_s3_class(result, "data.frame")

  expected_cols <- c(
    "id", "calendar_id", "calendar_name", "title",
    "start", "end", "all_day", "location", "description",
    "color", "recurring", "status"
  )
  expect_true(all(expected_cols %in% names(result)))
})

test_that("generate_sample_events returns events within date range", {
  set.seed(42)
  start_date <- as.Date("2025-01-15")
  end_date <- as.Date("2025-01-20")

  result <- generate_sample_events(start = start_date, end = end_date)

  if (nrow(result) > 0) {
    event_dates <- as.Date(result$start)
    expect_true(all(event_dates >= start_date))
    expect_true(all(event_dates <= end_date))
  }
})

test_that("generate_sample_events has valid event times", {
  set.seed(42)
  result <- generate_sample_events(
    start = as.Date("2025-01-01"),
    end = as.Date("2025-01-14")
  )

  if (nrow(result) > 0) {
    # End time should always be after start time
    expect_true(all(result$end > result$start))

    # All events should have valid POSIXct times
    expect_s3_class(result$start, "POSIXct")
    expect_s3_class(result$end, "POSIXct")
  }
})

test_that("generate_sample_events has valid colors", {
  set.seed(42)
  result <- generate_sample_events(
    start = as.Date("2025-01-01"),
    end = as.Date("2025-01-07")
  )

  if (nrow(result) > 0) {
    expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", result$color)))
  }
})

test_that("generate_sample_events handles empty date range", {
  # Single day range that might not generate events (weekend)
  set.seed(123)
  result <- generate_sample_events(
    start = as.Date("2025-01-01"),
    end = as.Date("2025-01-01")
  )

  expect_s3_class(result, "data.frame")
  # Should have expected columns even if empty
  expect_true("id" %in% names(result))
})

test_that("generate_sample_events events are sorted by start time", {
  set.seed(42)
  result <- generate_sample_events(
    start = as.Date("2025-01-01"),
    end = as.Date("2025-01-14")
  )

  if (nrow(result) > 1) {
    # Each event start should be >= previous event start
    expect_true(all(diff(result$start) >= 0))
  }
})

test_that("is_demo_mode returns TRUE when credentials not set", {
  # Save original values
  orig_id <- Sys.getenv("GOOGLE_CLIENT_ID")
  orig_secret <- Sys.getenv("GOOGLE_CLIENT_SECRET")

  # Clear credentials
  Sys.setenv(GOOGLE_CLIENT_ID = "")
  Sys.setenv(GOOGLE_CLIENT_SECRET = "")

  expect_true(is_demo_mode())

  # Restore original values
  Sys.setenv(GOOGLE_CLIENT_ID = orig_id)
  Sys.setenv(GOOGLE_CLIENT_SECRET = orig_secret)
})

test_that("is_demo_mode returns FALSE when credentials are set", {
  # Save original values
  orig_id <- Sys.getenv("GOOGLE_CLIENT_ID")
  orig_secret <- Sys.getenv("GOOGLE_CLIENT_SECRET")

  # Set dummy credentials
  Sys.setenv(GOOGLE_CLIENT_ID = "test_id")
  Sys.setenv(GOOGLE_CLIENT_SECRET = "test_secret")

  expect_false(is_demo_mode())

  # Restore original values
  Sys.setenv(GOOGLE_CLIENT_ID = orig_id)
  Sys.setenv(GOOGLE_CLIENT_SECRET = orig_secret)
})
