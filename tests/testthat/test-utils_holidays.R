test_that("is_us_holiday correctly identifies holidays", {
  # Christmas

  expect_true(is_us_holiday(as.Date("2026-12-25")))
  expect_true(is_us_holiday(as.Date("2025-12-25")))

  # Independence Day
  expect_true(is_us_holiday(as.Date("2026-07-04")))

  # New Year's Day
  expect_true(is_us_holiday(as.Date("2026-01-01")))

  # Regular days should not be holidays
  expect_false(is_us_holiday(as.Date("2026-03-15")))
  expect_false(is_us_holiday(as.Date("2026-08-20")))
})

test_that("get_us_holiday returns correct names", {
  expect_equal(get_us_holiday(as.Date("2026-12-25")), "Christmas Day")
  expect_equal(get_us_holiday(as.Date("2026-07-04")), "Independence Day")
  expect_equal(get_us_holiday(as.Date("2026-01-01")), "New Year's Day")
  expect_null(get_us_holiday(as.Date("2026-03-15")))
})

test_that("get_holiday_info returns correct structure", {
  # Holiday
  info <- get_holiday_info(as.Date("2026-12-25"))
  expect_true(info$is_holiday)
  expect_equal(info$name, "Christmas Day")
  expect_equal(info$class, "holiday-christmas")

  # Non-holiday
  info <- get_holiday_info(as.Date("2026-03-15"))
  expect_false(info$is_holiday)
  expect_null(info$name)
  expect_null(info$class)
})

test_that("floating holidays are calculated correctly", {
  # Thanksgiving 2026 is November 26 (4th Thursday)
  expect_equal(get_us_holiday(as.Date("2026-11-26")), "Thanksgiving")

  # Memorial Day 2026 is May 25 (last Monday)
  expect_equal(get_us_holiday(as.Date("2026-05-25")), "Memorial Day")

  # Labor Day 2026 is September 7 (1st Monday)
  expect_equal(get_us_holiday(as.Date("2026-09-07")), "Labor Day")

  # MLK Day 2026 is January 19 (3rd Monday)
  expect_equal(get_us_holiday(as.Date("2026-01-19")), "Martin Luther King Jr. Day")
})

test_that("get_holidays_in_range returns correct holidays", {
  # December 2026 should have Christmas Eve, Christmas, and New Year's Eve
  holidays <- get_holidays_in_range(as.Date("2026-12-01"), as.Date("2026-12-31"))
  expect_true(nrow(holidays) >= 3)
  expect_true("Christmas Day" %in% holidays$name)
  expect_true("Christmas Eve" %in% holidays$name)
  expect_true("New Year's Eve" %in% holidays$name)
})
