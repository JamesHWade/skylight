# Tests for utils_offline.R

test_that("set_online_status updates offline state correctly", {
  # Set offline
  set_online_status(FALSE)
  status <- get_offline_status()
  expect_true(status$is_offline)
  expect_false(is.null(status$offline_since))

  # Set online
  set_online_status(TRUE)
  status <- get_offline_status()
  expect_false(status$is_offline)
  expect_null(status$offline_since)
  expect_false(is.null(status$last_online))
})

test_that("get_offline_status returns expected structure", {
  status <- get_offline_status()

  expect_type(status, "list")
  expect_true("is_offline" %in% names(status))
  expect_true("last_online" %in% names(status))
  expect_true("offline_since" %in% names(status))
  expect_true("offline_duration" %in% names(status))
})

test_that("format_offline_message returns NULL when online", {
  set_online_status(TRUE)
  status <- get_offline_status()
  result <- format_offline_message(status)
  expect_null(result)
})

test_that("format_offline_message returns message when offline", {
  set_online_status(FALSE)
  status <- get_offline_status()
  result <- format_offline_message(status, cache_age = 10)

  expect_type(result, "character")
  expect_match(result, "offline", ignore.case = TRUE)
  expect_match(result, "10 minutes")
})

test_that("format_offline_message handles hours correctly", {
  set_online_status(FALSE)
  status <- get_offline_status()
  result <- format_offline_message(status, cache_age = 120)

  expect_match(result, "hours")
})

test_that("format_offline_message handles no cache", {
  set_online_status(FALSE)
  status <- get_offline_status()
  result <- format_offline_message(status, cache_age = NULL)

  expect_match(result, "No cached data")
})

test_that("offline_indicator_ui returns valid HTML", {
  ui <- offline_indicator_ui("test")

  expect_s3_class(ui, "shiny.tag")
  # Should contain the offline banner div
  expect_true(grepl("offline-banner", as.character(ui)))
})

# Reset online status after tests
set_online_status(TRUE)
