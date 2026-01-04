#' US Holiday Utilities
#'
#' Functions for detecting and handling US federal holidays.
#'
#' @name utils_holidays
#' @keywords internal
NULL

#' Check if Date is a US Holiday
#'
#' @param date A Date object.
#'
#' @return Logical indicating if the date is a US federal holiday.
#'
#' @keywords internal
is_us_holiday <- function(date) {

  !is.null(get_us_holiday(date))
}

#' Get US Holiday Name
#'
#' Returns the holiday name if the date is a US federal holiday.
#'
#' @param date A Date object.
#'
#' @return Character string with holiday name, or NULL if not a holiday.
#'
#' @keywords internal
get_us_holiday <- function(date) {
  date <- as.Date(date)
  year <- as.integer(format(date, "%Y"))
  month <- as.integer(format(date, "%m"))
  day <- as.integer(format(date, "%d"))
  wday <- as.integer(format(date, "%w"))  # 0 = Sunday


  # Fixed holidays
  if (month == 1 && day == 1) return("New Year's Day")
  if (month == 7 && day == 4) return("Independence Day")
  if (month == 11 && day == 11) return("Veterans Day")
  if (month == 12 && day == 25) return("Christmas Day")
  if (month == 12 && day == 31) return("New Year's Eve")

  # Martin Luther King Jr. Day (3rd Monday in January)
  if (month == 1 && wday == 1) {
    if (day >= 15 && day <= 21) return("Martin Luther King Jr. Day")
  }

  # Presidents' Day (3rd Monday in February)
  if (month == 2 && wday == 1) {
    if (day >= 15 && day <= 21) return("Presidents' Day")
  }

  # Memorial Day (last Monday in May)
  if (month == 5 && wday == 1) {
    if (day >= 25 && day <= 31) return("Memorial Day")
  }

  # Juneteenth (June 19)
  if (month == 6 && day == 19) return("Juneteenth")

  # Labor Day (1st Monday in September)
  if (month == 9 && wday == 1) {
    if (day >= 1 && day <= 7) return("Labor Day")
  }

  # Columbus Day (2nd Monday in October)
  if (month == 10 && wday == 1) {
    if (day >= 8 && day <= 14) return("Columbus Day")
  }

  # Thanksgiving (4th Thursday in November)
  if (month == 11 && wday == 4) {
    if (day >= 22 && day <= 28) return("Thanksgiving")
  }

  # Day after Thanksgiving (Black Friday)
  if (month == 11 && wday == 5) {
    if (day >= 23 && day <= 29) return("Day after Thanksgiving")
  }

  # Christmas Eve
  if (month == 12 && day == 24) return("Christmas Eve")

  NULL
}

#' Get Holiday Info for Date
#'
#' Returns holiday information including name and styling class.
#'
#' @param date A Date object.
#'
#' @return A list with `is_holiday`, `name`, and `class` fields.
#'
#' @keywords internal
get_holiday_info <- function(date) {
  name <- get_us_holiday(date)

  if (is.null(name)) {
    return(list(
      is_holiday = FALSE,
      name = NULL,
      class = NULL
    ))
  }

  # Determine holiday class for styling
  holiday_class <- if (name %in% c("Christmas Day", "Christmas Eve")) {
    "holiday-christmas"
  } else if (name %in% c("Independence Day")) {
    "holiday-patriotic"
  } else if (name %in% c("Thanksgiving", "Day after Thanksgiving")) {
    "holiday-thanksgiving"
  } else if (name %in% c("New Year's Day", "New Year's Eve")) {
    "holiday-newyear"
  } else {
    "holiday-general"
  }

  list(
    is_holiday = TRUE,
    name = name,
    class = holiday_class
  )
}

#' Get All Holidays in Date Range
#'
#' Returns all US holidays within a date range.
#'
#' @param start_date Start of date range.
#' @param end_date End of date range.
#'
#' @return A data frame with `date`, `name`, and `class` columns.
#'
#' @keywords internal
get_holidays_in_range <- function(start_date, end_date) {
  dates <- seq(as.Date(start_date), as.Date(end_date), by = "day")

  holidays <- lapply(dates, function(d) {
    info <- get_holiday_info(d)
    if (info$is_holiday) {
      data.frame(
        date = d,
        name = info$name,
        class = info$class,
        stringsAsFactors = FALSE
      )
    } else {
      NULL
    }
  })

  do.call(rbind, holidays)
}
