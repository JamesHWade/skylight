#' Gamification Utilities for Chores
#'
#' Points calculation, streaks, and leaderboard functions.
#'
#' @name utils_gamification
#' @keywords internal
NULL

# =============================================================================
# STREAK BONUSES
# =============================================================================

#' Streak bonus thresholds
#'
#' Points awarded for maintaining completion streaks.
#' @keywords internal
STREAK_BONUSES <- list(
  "3" = 5,    # 3-day streak: +5 points

  "5" = 10,   # 5-day streak: +10 points
  "7" = 25,   # 1-week streak: +25 points
  "14" = 50,  # 2-week streak: +50 points
  "30" = 100  # 1-month streak: +100 points
)

#' Get Streak Bonus
#'
#' Returns the bonus points for a given streak count.
#'
#' @param streak_count Number of consecutive days with completions.
#'
#' @return Bonus points (integer).
#'
#' @export
get_streak_bonus <- function(streak_count) {
  if (streak_count < 3) return(0L)

  bonus <- 0L
  thresholds <- as.integer(names(STREAK_BONUSES))

  for (threshold in thresholds) {
    if (streak_count >= threshold) {
      bonus <- STREAK_BONUSES[[as.character(threshold)]]
    }
  }

  as.integer(bonus)
}

# =============================================================================
# STREAK CALCULATION
# =============================================================================

#' Get Current Streak
#'
#' Calculates the current completion streak for a family member.
#' A streak counts consecutive days with at least one completion.
#'
#' @param member_id The member ID.
#'
#' @return Number of consecutive days (integer).
#'
#' @export
get_current_streak <- function(member_id) {
  # Get distinct completion dates, ordered descending
  completions <- db_query("
    SELECT DISTINCT DATE(completed_at) as completion_date
    FROM chore_completions
    WHERE member_id = ?
    ORDER BY completion_date DESC
  ", params = list(member_id))

  if (nrow(completions) == 0) return(0L)

  streak <- 0L
  expected_date <- Sys.Date()

  for (i in seq_len(nrow(completions))) {
    comp_date <- as.Date(completions$completion_date[i])

    # Allow for today or yesterday to start the streak
    if (i == 1) {
      if (comp_date == expected_date || comp_date == expected_date - 1) {
        streak <- 1L
        expected_date <- comp_date - 1
      } else {
        # No recent completions, streak is 0
        break
      }
    } else {
      if (comp_date == expected_date) {
        streak <- streak + 1L
        expected_date <- comp_date - 1
      } else {
        # Gap in completions, streak ends
        break
      }
    }
  }

  streak
}

#' Get Longest Streak
#'
#' Calculates the longest completion streak ever achieved by a member.
#'
#' @param member_id The member ID.
#'
#' @return Number of days in longest streak (integer).
#'
#' @export
get_longest_streak <- function(member_id) {
  completions <- db_query("
    SELECT DISTINCT DATE(completed_at) as completion_date
    FROM chore_completions
    WHERE member_id = ?
    ORDER BY completion_date DESC
  ", params = list(member_id))

  if (nrow(completions) == 0) return(0L)

  longest <- 0L
  current <- 1L
  prev_date <- as.Date(completions$completion_date[1])

  for (i in seq_len(nrow(completions))[-1]) {
    comp_date <- as.Date(completions$completion_date[i])

    if (prev_date - comp_date == 1) {
      current <- current + 1L
    } else {
      longest <- max(longest, current)
      current <- 1L
    }

    prev_date <- comp_date
  }

  max(longest, current)
}

#' Check if Streak is Broken
#'
#' Checks if a member's streak would be broken if they don't complete
#' anything by the given date.
#'
#' @param member_id The member ID.
#' @param date The date to check (default: today).
#'
#' @return TRUE if streak would be broken, FALSE otherwise.
#'
#' @export
check_streak_broken <- function(member_id, date = Sys.Date()) {
  # Get most recent completion
  last_completion <- db_query("
    SELECT MAX(DATE(completed_at)) as last_date
    FROM chore_completions
    WHERE member_id = ?
  ", params = list(member_id))

  if (is.na(last_completion$last_date[1])) {
    # No completions yet, no streak to break
    return(FALSE)
  }

  last_date <- as.Date(last_completion$last_date[1])
  days_since <- as.integer(date - last_date)

  # Streak is broken if more than 1 day has passed
  days_since > 1
}

# =============================================================================
# POINTS CALCULATION
# =============================================================================

#' Calculate Points
#'
#' Calculates total points including base and streak bonus.
#'
#' @param chore_id The chore ID.
#' @param member_id The member ID.
#' @param streak_count Current streak count (optional, will be calculated if not provided).
#'
#' @return A list with base, streak_bonus, and total points.
#'
#' @export
calculate_points <- function(chore_id, member_id, streak_count = NULL) {
  chore <- get_chore(chore_id)
  if (is.null(chore)) {
    return(list(base = 0L, streak_bonus = 0L, total = 0L))
  }

  base_points <- as.integer(chore$points[1])

  if (is.null(streak_count)) {
    streak_count <- get_current_streak(member_id) + 1  # +1 for this completion
  }

  streak_bonus <- get_streak_bonus(streak_count)

  list(
    base = base_points,
    streak_bonus = streak_bonus,
    total = base_points + streak_bonus
  )
}

#' Get Today's Points
#'
#' Gets total points earned by a member today.
#'
#' @param member_id The member ID.
#'
#' @return Total points (integer).
#'
#' @export
get_today_points <- function(member_id) {

  today <- as.character(Sys.Date())
  result <- db_query("
    SELECT COALESCE(SUM(points_earned + streak_bonus), 0) as total
    FROM chore_completions
    WHERE member_id = ?
      AND DATE(completed_at) = ?
  ", params = list(member_id, today))

  as.integer(result$total[1])
}

#' Get Period Points
#'
#' Gets total points earned by a member in a time period.
#'
#' @param member_id The member ID.
#' @param period One of 'day', 'week', 'month', 'all_time'.
#'
#' @return Total points (integer).
#'
#' @export
get_period_points <- function(member_id, period = "week") {
  date_filter <- get_period_filter(period)

  result <- db_query(paste0("
    SELECT COALESCE(SUM(points_earned + streak_bonus), 0) as total
    FROM chore_completions
    WHERE member_id = ?
      AND ", date_filter
  ), params = list(member_id))

  as.integer(result$total[1])
}

# =============================================================================
# LEADERBOARD
# =============================================================================

#' Get Leaderboard
#'
#' Returns family members ranked by points for a given period.
#'
#' @param period One of 'day', 'week', 'month', 'all_time'.
#'
#' @return A data frame with member info and points, ordered by rank.
#'
#' @export
get_leaderboard <- function(period = "week") {
  date_filter <- get_period_filter(period)

  db_query(paste0("
    SELECT
      m.id,
      m.name,
      m.display_name,
      m.avatar_emoji,
      m.color,
      COALESCE(SUM(c.points_earned + c.streak_bonus), 0) as total_points,
      COUNT(c.id) as completions
    FROM family_members m
    LEFT JOIN chore_completions c ON m.id = c.member_id AND ", date_filter, "
    WHERE m.is_active = TRUE
    GROUP BY m.id, m.name, m.display_name, m.avatar_emoji, m.color
    ORDER BY total_points DESC, completions DESC
  "))
}

#' Get Member Stats
#'
#' Returns detailed statistics for a family member.
#'
#' @param member_id The member ID.
#' @param period One of 'day', 'week', 'month', 'all_time'.
#'
#' @return A list with various stats.
#'
#' @export
get_member_stats <- function(member_id, period = "week") {
  date_filter <- get_period_filter(period)

  points <- db_query(paste0("
    SELECT
      COALESCE(SUM(points_earned + streak_bonus), 0) as total_points,
      COUNT(*) as completions
    FROM chore_completions
    WHERE member_id = ?
      AND ", date_filter
  ), params = list(member_id))

  current_streak <- get_current_streak(member_id)
  longest_streak <- get_longest_streak(member_id)

  # Get rank
  leaderboard <- get_leaderboard(period)
  rank <- which(leaderboard$id == member_id)
  if (length(rank) == 0) rank <- NA

  list(
    total_points = as.integer(points$total_points[1]),
    completions = as.integer(points$completions[1]),
    current_streak = current_streak,
    longest_streak = longest_streak,
    rank = rank,
    total_members = nrow(leaderboard)
  )
}

#' Get Family Stats
#'
#' Returns aggregate statistics for the whole family.
#'
#' @param period One of 'day', 'week', 'month', 'all_time'.
#'
#' @return A list with family-wide stats.
#'
#' @export
get_family_stats <- function(period = "week") {
  today <- as.character(Sys.Date())
  date_filter <- get_period_filter(period, today)

  totals <- db_query(paste0("
    SELECT
      COALESCE(SUM(points_earned + streak_bonus), 0) as total_points,
      COUNT(*) as total_completions
    FROM chore_completions
    WHERE ", date_filter
  ))

  pending <- db_query("
    SELECT COUNT(*) as pending_today
    FROM chore_assignments
    WHERE assigned_date = ?
      AND status = 'pending'
  ", params = list(today))

  completed_today <- db_query("
    SELECT COUNT(*) as completed_today
    FROM chore_assignments
    WHERE assigned_date = ?
      AND status = 'completed'
  ", params = list(today))

  list(
    total_points = as.integer(totals$total_points[1]),
    total_completions = as.integer(totals$total_completions[1]),
    pending_today = as.integer(pending$pending_today[1]),
    completed_today = as.integer(completed_today$completed_today[1])
  )
}

# =============================================================================
# HELPERS
# =============================================================================

#' Get Period Filter
#'
#' Returns SQL WHERE clause fragment for date filtering.
#'
#' @param period One of 'day', 'week', 'month', 'all_time'.
#' @param today Today's date as character string (YYYY-MM-DD).
#'
#' @return SQL string.
#'
#' @keywords internal
get_period_filter <- function(period, today = as.character(Sys.Date())) {
  # Calculate week and month start dates in R to avoid DuckDB ICU dependency
  today_date <- as.Date(today)
  week_start <- as.character(today_date - as.numeric(format(today_date, "%u")) + 1)
  month_start <- as.character(as.Date(format(today_date, "%Y-%m-01")))

  switch(period,
    "day" = sprintf("DATE(completed_at) = '%s'", today),
    "week" = sprintf("completed_at >= '%s'", week_start),
    "month" = sprintf("completed_at >= '%s'", month_start),
    "all_time" = "1=1",
    "1=1"  # default
  )
}

#' Get Chores Summary for Today
#'
#' Returns a summary of today's chores status for the widget.
#'
#' @return A list with completed and total counts.
#'
#' @export
get_chores_today_summary <- function() {
  today <- as.character(Sys.Date())
  result <- db_query("
    SELECT
      COUNT(*) as total,
      SUM(CASE WHEN status = 'completed' THEN 1 ELSE 0 END) as completed
    FROM chore_assignments
    WHERE assigned_date = ?
  ", params = list(today))

  list(
    completed = if (is.na(result$completed[1])) 0L else as.integer(result$completed[1]),
    total = as.integer(result$total[1])
  )
}
