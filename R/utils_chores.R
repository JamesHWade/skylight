#' Initialize Chores Database Tables
#'
#' Creates the DuckDB tables required for the chores feature.
#' Called from db_init().
#'
#' @param con A DBI connection object.
#'
#' @return Invisibly returns TRUE on success.
#'
#' @keywords internal
db_init_chores <- function(con) {
  # Create sequences for auto-increment IDs
  tryCatch(
    DBI::dbExecute(con, "CREATE SEQUENCE IF NOT EXISTS family_members_id_seq"),
    error = function(e) NULL
  )
  tryCatch(
    DBI::dbExecute(con, "CREATE SEQUENCE IF NOT EXISTS chores_id_seq"),
    error = function(e) NULL
  )
  tryCatch(
    DBI::dbExecute(con, "CREATE SEQUENCE IF NOT EXISTS chore_assignments_id_seq"),
    error = function(e) NULL
  )
  tryCatch(
    DBI::dbExecute(con, "CREATE SEQUENCE IF NOT EXISTS chore_completions_id_seq"),
    error = function(e) NULL
  )
  tryCatch(
    DBI::dbExecute(con, "CREATE SEQUENCE IF NOT EXISTS chore_rotations_id_seq"),
    error = function(e) NULL
  )

  # Family members table

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS family_members (
      id INTEGER PRIMARY KEY DEFAULT nextval('family_members_id_seq'),
      name VARCHAR NOT NULL,
      display_name VARCHAR,
      avatar_emoji VARCHAR DEFAULT '\U0001F464',
      color VARCHAR DEFAULT '#74B9FF',
      birth_date DATE,
      is_active BOOLEAN DEFAULT TRUE,
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )
  ")

  # Chores table
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS chores (
      id INTEGER PRIMARY KEY DEFAULT nextval('chores_id_seq'),
      title VARCHAR NOT NULL,
      description VARCHAR,
      points INTEGER DEFAULT 10,
      frequency VARCHAR DEFAULT 'daily',
      frequency_days VARCHAR,
      category VARCHAR DEFAULT 'general',
      estimated_minutes INTEGER DEFAULT 15,
      icon_emoji VARCHAR DEFAULT '\U0001F9F9',
      is_active BOOLEAN DEFAULT TRUE,
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )
  ")

  # Chore assignments table
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS chore_assignments (
      id INTEGER PRIMARY KEY DEFAULT nextval('chore_assignments_id_seq'),
      chore_id INTEGER NOT NULL,
      member_id INTEGER NOT NULL,
      assigned_date DATE NOT NULL,
      due_time TIME,
      rotation_order INTEGER DEFAULT 0,
      status VARCHAR DEFAULT 'pending',
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )
  ")

  # Chore completions table (historical record)
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS chore_completions (
      id INTEGER PRIMARY KEY DEFAULT nextval('chore_completions_id_seq'),
      assignment_id INTEGER,
      chore_id INTEGER NOT NULL,
      member_id INTEGER NOT NULL,
      points_earned INTEGER NOT NULL,
      completed_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      completion_notes VARCHAR,
      verified_by INTEGER,
      streak_bonus INTEGER DEFAULT 0
    )
  ")

  # Chore rotations table (optional rotation config)
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS chore_rotations (
      id INTEGER PRIMARY KEY DEFAULT nextval('chore_rotations_id_seq'),
      chore_id INTEGER NOT NULL,
      rotation_members VARCHAR NOT NULL,
      rotation_frequency VARCHAR DEFAULT 'weekly',
      current_index INTEGER DEFAULT 0,
      last_rotated_at TIMESTAMP,
      is_active BOOLEAN DEFAULT TRUE,
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )
  ")

  # Create indexes for common queries
  tryCatch({
    DBI::dbExecute(con, "
      CREATE INDEX IF NOT EXISTS idx_assignments_date ON chore_assignments(assigned_date)
    ")
    DBI::dbExecute(con, "
      CREATE INDEX IF NOT EXISTS idx_assignments_member ON chore_assignments(member_id)
    ")
    DBI::dbExecute(con, "
      CREATE INDEX IF NOT EXISTS idx_assignments_status ON chore_assignments(status)
    ")
    DBI::dbExecute(con, "
      CREATE INDEX IF NOT EXISTS idx_completions_member ON chore_completions(member_id)
    ")
    DBI::dbExecute(con, "
      CREATE INDEX IF NOT EXISTS idx_completions_date ON chore_completions(completed_at)
    ")
  }, error = function(e) NULL)

  invisible(TRUE)
}

# =============================================================================
# FAMILY MEMBERS CRUD
# =============================================================================

#' Get Family Members
#'
#' Retrieves all family members from the database.
#'
#' @param active_only If TRUE (default), only returns active members.
#'
#' @return A data frame of family members.
#'
#' @export
get_family_members <- function(active_only = TRUE) {
  query <- "SELECT * FROM family_members"
  if (active_only) {
    query <- paste(query, "WHERE is_active = TRUE")
  }
  query <- paste(query, "ORDER BY name")

  db_query(query)
}

#' Get Family Member by ID
#'
#' @param id The member ID.
#'
#' @return A single-row data frame or NULL if not found.
#'
#' @export
get_family_member <- function(id) {
  result <- db_query("SELECT * FROM family_members WHERE id = ?", params = list(id))
  if (nrow(result) == 0) NULL else result
}

#' Create Family Member
#'
#' @param name The member's name.
#' @param display_name Optional display name/nickname.
#' @param avatar_emoji Emoji avatar (default: person emoji).
#' @param color Color for UI (default: soft blue).
#' @param birth_date Optional birth date.
#'
#' @return The ID of the created member.
#'
#' @export
create_family_member <- function(name,
                                  display_name = NULL,
                                  avatar_emoji = "\U0001F464",
                                  color = "#74B9FF",
                                  birth_date = NULL) {
  db_execute("
    INSERT INTO family_members (name, display_name, avatar_emoji, color, birth_date)
    VALUES (?, ?, ?, ?, ?)
  ", params = list(
    name,
    display_name,
    avatar_emoji,
    color,
    if (is.null(birth_date)) NA else as.character(birth_date)
  ))

  # Return the created member's ID
  result <- db_query("SELECT MAX(id) as id FROM family_members")
  result$id[1]
}

#' Update Family Member
#'
#' @param id The member ID.
#' @param ... Fields to update (name, display_name, avatar_emoji, color, birth_date, is_active).
#'
#' @return TRUE on success.
#'
#' @export
update_family_member <- function(id, ...) {
  updates <- list(...)
  if (length(updates) == 0) return(invisible(TRUE))

  valid_fields <- c("name", "display_name", "avatar_emoji", "color", "birth_date", "is_active")
  updates <- updates[names(updates) %in% valid_fields]

  if (length(updates) == 0) return(invisible(TRUE))

  set_clause <- paste(
    sapply(names(updates), function(n) paste0(n, " = ?")),
    collapse = ", "
  )
  set_clause <- paste(set_clause, ", updated_at = CURRENT_TIMESTAMP")

  params <- c(unname(updates), list(id))

  db_execute(
    paste("UPDATE family_members SET", set_clause, "WHERE id = ?"),
    params = params
  )

  invisible(TRUE)
}

#' Delete Family Member
#'
#' Soft-deletes a family member by setting is_active to FALSE.
#'
#' @param id The member ID.
#'
#' @return TRUE on success.
#'
#' @export
delete_family_member <- function(id) {
  update_family_member(id, is_active = FALSE)
}

# =============================================================================
# CHORES CRUD
# =============================================================================

#' Get Chores
#'
#' Retrieves all chores from the database.
#'
#' @param active_only If TRUE (default), only returns active chores.
#' @param category Optional category filter.
#'
#' @return A data frame of chores.
#'
#' @export
get_chores <- function(active_only = TRUE, category = NULL) {
  query <- "SELECT * FROM chores WHERE 1=1"
  params <- list()


if (active_only) {
    query <- paste(query, "AND is_active = TRUE")
  }

  if (!is.null(category)) {
    query <- paste(query, "AND category = ?")
    params <- c(params, list(category))
  }

  query <- paste(query, "ORDER BY title")

  if (length(params) == 0) {
    db_query(query)
  } else {
    db_query(query, params = params)
  }
}

#' Get Chore by ID
#'
#' @param id The chore ID.
#'
#' @return A single-row data frame or NULL if not found.
#'
#' @export
get_chore <- function(id) {
  result <- db_query("SELECT * FROM chores WHERE id = ?", params = list(id))
  if (nrow(result) == 0) NULL else result
}

#' Create Chore
#'
#' @param title The chore title.
#' @param description Optional description.
#' @param points Points awarded on completion (default: 10).
#' @param frequency How often: 'daily', 'weekly', 'monthly', 'once' (default: 'daily').
#' @param frequency_days For weekly: which days (JSON array like '["monday","wednesday"]').
#' @param category Category: 'kitchen', 'bedroom', 'bathroom', 'outdoor', 'general'.
#' @param estimated_minutes Estimated time in minutes (default: 15).
#' @param icon_emoji Emoji icon (default: broom).
#'
#' @return The ID of the created chore.
#'
#' @export
create_chore <- function(title,
                          description = NULL,
                          points = 10,
                          frequency = "daily",
                          frequency_days = NULL,
                          category = "general",
                          estimated_minutes = 15,
                          icon_emoji = "\U0001F9F9") {
  db_execute("
    INSERT INTO chores (title, description, points, frequency, frequency_days, category, estimated_minutes, icon_emoji)
    VALUES (?, ?, ?, ?, ?, ?, ?, ?)
  ", params = list(
    title,
    description,
    points,
    frequency,
    frequency_days,
    category,
    estimated_minutes,
    icon_emoji
  ))

  result <- db_query("SELECT MAX(id) as id FROM chores")
  result$id[1]
}

#' Update Chore
#'
#' @param id The chore ID.
#' @param ... Fields to update.
#'
#' @return TRUE on success.
#'
#' @export
update_chore <- function(id, ...) {
  updates <- list(...)
  if (length(updates) == 0) return(invisible(TRUE))

  valid_fields <- c("title", "description", "points", "frequency", "frequency_days",
                    "category", "estimated_minutes", "icon_emoji", "is_active")
  updates <- updates[names(updates) %in% valid_fields]

  if (length(updates) == 0) return(invisible(TRUE))

  set_clause <- paste(
    sapply(names(updates), function(n) paste0(n, " = ?")),
    collapse = ", "
  )
  set_clause <- paste(set_clause, ", updated_at = CURRENT_TIMESTAMP")

  params <- c(unname(updates), list(id))

  db_execute(
    paste("UPDATE chores SET", set_clause, "WHERE id = ?"),
    params = params
  )

  invisible(TRUE)
}

#' Delete Chore
#'
#' Soft-deletes a chore by setting is_active to FALSE.
#'
#' @param id The chore ID.
#'
#' @return TRUE on success.
#'
#' @export
delete_chore <- function(id) {
  update_chore(id, is_active = FALSE)
}

# =============================================================================
# ASSIGNMENTS CRUD
# =============================================================================

#' Get Assignments for Date
#'
#' Retrieves all chore assignments for a specific date.
#'
#' @param date The date to query (default: today).
#' @param member_id Optional member ID filter.
#' @param status Optional status filter ('pending', 'completed', 'skipped').
#'
#' @return A data frame of assignments with chore and member details.
#'
#' @export
get_assignments_for_date <- function(date = Sys.Date(), member_id = NULL, status = NULL) {
  query <- "
    SELECT
      a.*,
      c.title as chore_title,
      c.description as chore_description,
      c.points as chore_points,
      c.icon_emoji as chore_icon,
      c.category as chore_category,
      m.name as member_name,
      m.display_name as member_display_name,
      m.avatar_emoji as member_avatar,
      m.color as member_color
    FROM chore_assignments a
    JOIN chores c ON a.chore_id = c.id
    JOIN family_members m ON a.member_id = m.id
    WHERE a.assigned_date = ?
  "
  params <- list(as.character(date))

  if (!is.null(member_id)) {
    query <- paste(query, "AND a.member_id = ?")
    params <- c(params, list(member_id))
  }

  if (!is.null(status)) {
    query <- paste(query, "AND a.status = ?")
    params <- c(params, list(status))
  }

  query <- paste(query, "ORDER BY m.name, c.title")

  db_query(query, params = params)
}

#' Get Assignments for Member
#'
#' Retrieves assignments for a specific family member.
#'
#' @param member_id The member ID.
#' @param start_date Start of date range (default: today).
#' @param end_date End of date range (default: same as start_date).
#' @param status Optional status filter.
#'
#' @return A data frame of assignments.
#'
#' @export
get_assignments_for_member <- function(member_id,
                                        start_date = Sys.Date(),
                                        end_date = NULL,
                                        status = NULL) {
  if (is.null(end_date)) end_date <- start_date

  query <- "
    SELECT
      a.*,
      c.title as chore_title,
      c.description as chore_description,
      c.points as chore_points,
      c.icon_emoji as chore_icon,
      c.category as chore_category
    FROM chore_assignments a
    JOIN chores c ON a.chore_id = c.id
    WHERE a.member_id = ?
      AND a.assigned_date >= ?
      AND a.assigned_date <= ?
  "
  params <- list(member_id, as.character(start_date), as.character(end_date))

  if (!is.null(status)) {
    query <- paste(query, "AND a.status = ?")
    params <- c(params, list(status))
  }

  query <- paste(query, "ORDER BY a.assigned_date, c.title")

  db_query(query, params = params)
}

#' Create Assignment
#'
#' Creates a new chore assignment.
#'
#' @param chore_id The chore ID.
#' @param member_id The member ID.
#' @param assigned_date The date for the assignment.
#' @param due_time Optional due time.
#'
#' @return The ID of the created assignment.
#'
#' @export
create_assignment <- function(chore_id, member_id, assigned_date, due_time = NULL) {
  db_execute("
    INSERT INTO chore_assignments (chore_id, member_id, assigned_date, due_time)
    VALUES (?, ?, ?, ?)
  ", params = list(
    chore_id,
    member_id,
    as.character(assigned_date),
    due_time
  ))

  result <- db_query("SELECT MAX(id) as id FROM chore_assignments")
  result$id[1]
}

#' Complete Assignment
#'
#' Marks an assignment as completed and records the completion.
#'
#' @param assignment_id The assignment ID.
#' @param notes Optional completion notes.
#' @param verified_by Optional member ID who verified.
#'
#' @return The completion record ID.
#'
#' @export
complete_assignment <- function(assignment_id, notes = NULL, verified_by = NULL) {
  # Get assignment details
  assignment <- db_query("
    SELECT a.*, c.points as chore_points
    FROM chore_assignments a
    JOIN chores c ON a.chore_id = c.id
    WHERE a.id = ?
  ", params = list(assignment_id))

  if (nrow(assignment) == 0) {
    stop("Assignment not found: ", assignment_id)
  }

  # Calculate streak bonus
  streak_count <- get_current_streak(assignment$member_id[1])
  streak_bonus <- get_streak_bonus(streak_count + 1)  # +1 for this completion
  points_earned <- assignment$chore_points[1]

  # Update assignment status
  db_execute("
    UPDATE chore_assignments SET status = 'completed' WHERE id = ?
  ", params = list(assignment_id))

  # Record completion
  db_execute("
    INSERT INTO chore_completions
      (assignment_id, chore_id, member_id, points_earned, completion_notes, verified_by, streak_bonus)
    VALUES (?, ?, ?, ?, ?, ?, ?)
  ", params = list(
    assignment_id,
    assignment$chore_id[1],
    assignment$member_id[1],
    points_earned,
    notes,
    verified_by,
    streak_bonus
  ))

  result <- db_query("SELECT MAX(id) as id FROM chore_completions")
  result$id[1]
}

#' Skip Assignment
#'
#' Marks an assignment as skipped.
#'
#' @param assignment_id The assignment ID.
#'
#' @return TRUE on success.
#'
#' @export
skip_assignment <- function(assignment_id) {
  db_execute("
    UPDATE chore_assignments SET status = 'skipped' WHERE id = ?
  ", params = list(assignment_id))
  invisible(TRUE)
}

#' Uncomplete Assignment
#'
#' Reverts a completed assignment back to pending.
#'
#' @param assignment_id The assignment ID.
#'
#' @return TRUE on success.
#'
#' @export
uncomplete_assignment <- function(assignment_id) {
  # Remove completion record
  db_execute("
    DELETE FROM chore_completions WHERE assignment_id = ?
  ", params = list(assignment_id))

  # Reset assignment status
  db_execute("
    UPDATE chore_assignments SET status = 'pending' WHERE id = ?
  ", params = list(assignment_id))

  invisible(TRUE)
}

# =============================================================================
# COMPLETIONS QUERIES
# =============================================================================

#' Get Completions for Member
#'
#' Retrieves completion history for a family member.
#'
#' @param member_id The member ID.
#' @param start_date Optional start date filter.
#' @param end_date Optional end date filter.
#' @param limit Maximum number of records (default: 100).
#'
#' @return A data frame of completions.
#'
#' @export
get_completions_for_member <- function(member_id,
                                        start_date = NULL,
                                        end_date = NULL,
                                        limit = 100) {
  query <- "
    SELECT
      cc.*,
      c.title as chore_title,
      c.icon_emoji as chore_icon
    FROM chore_completions cc
    JOIN chores c ON cc.chore_id = c.id
    WHERE cc.member_id = ?
  "
  params <- list(member_id)

  if (!is.null(start_date)) {
    query <- paste(query, "AND DATE(cc.completed_at) >= ?")
    params <- c(params, list(as.character(start_date)))
  }

  if (!is.null(end_date)) {
    query <- paste(query, "AND DATE(cc.completed_at) <= ?")
    params <- c(params, list(as.character(end_date)))
  }

  query <- paste(query, "ORDER BY cc.completed_at DESC LIMIT", limit)

  db_query(query, params = params)
}

#' Get Recent Completions
#'
#' Retrieves recent completions across all family members.
#'
#' @param limit Maximum number of records (default: 20).
#'
#' @return A data frame of completions with member info.
#'
#' @export
get_recent_completions <- function(limit = 20) {
  db_query(paste0("
    SELECT
      cc.*,
      c.title as chore_title,
      c.icon_emoji as chore_icon,
      m.name as member_name,
      m.display_name as member_display_name,
      m.avatar_emoji as member_avatar
    FROM chore_completions cc
    JOIN chores c ON cc.chore_id = c.id
    JOIN family_members m ON cc.member_id = m.id
    ORDER BY cc.completed_at DESC
    LIMIT ", limit))
}

# =============================================================================
# DAILY ASSIGNMENT GENERATION
# =============================================================================

#' Generate Daily Assignments
#'
#' Creates assignments for today based on chore frequency settings.
#' Skips if assignments already exist for the date.
#'
#' @param date The date to generate assignments for (default: today).
#' @param member_ids Optional vector of member IDs. If NULL, uses all active members.
#'
#' @return Number of assignments created.
#'
#' @export
generate_daily_assignments <- function(date = Sys.Date(), member_ids = NULL) {
  # Get all active chores
  chores <- get_chores(active_only = TRUE)
  if (nrow(chores) == 0) return(0)

  # Get active members
  if (is.null(member_ids)) {
    members <- get_family_members(active_only = TRUE)
    if (nrow(members) == 0) return(0)
    member_ids <- members$id
  }

  created <- 0

  for (i in seq_len(nrow(chores))) {
    chore <- chores[i, ]

    # Check if chore should run today
    if (!should_run_today(chore, date)) next

    # Check for rotation config
    rotation <- get_rotation_config(chore$id)

    if (!is.null(rotation) && rotation$is_active) {
      # Rotation-based assignment
      assignee_id <- get_rotation_assignee(chore$id)
      if (!is.null(assignee_id)) {
        # Check if assignment already exists
        existing <- db_query("
          SELECT id FROM chore_assignments
          WHERE chore_id = ? AND member_id = ? AND assigned_date = ?
        ", params = list(chore$id, assignee_id, as.character(date)))

        if (nrow(existing) == 0) {
          create_assignment(chore$id, assignee_id, date)
          created <- created + 1
        }
      }
    } else {
      # No rotation: assign to all members (or could be modified to skip)
      for (member_id in member_ids) {
        existing <- db_query("
          SELECT id FROM chore_assignments
          WHERE chore_id = ? AND member_id = ? AND assigned_date = ?
        ", params = list(chore$id, member_id, as.character(date)))

        if (nrow(existing) == 0) {
          create_assignment(chore$id, member_id, date)
          created <- created + 1
        }
      }
    }
  }

  created
}

#' Should Chore Run Today
#'
#' Determines if a chore should be assigned on a given date based on frequency.
#'
#' @param chore A single-row chore data frame.
#' @param date The date to check.
#'
#' @return TRUE if the chore should run, FALSE otherwise.
#'
#' @keywords internal
should_run_today <- function(chore, date) {
  day_name <- tolower(format(date, "%A"))

  switch(chore$frequency,
    "daily" = TRUE,
    "weekly" = {
      if (!is.null(chore$frequency_days) && !is.na(chore$frequency_days)) {
        days <- tryCatch(
          jsonlite::fromJSON(chore$frequency_days),
          error = function(e) NULL
        )
        if (!is.null(days)) {
          day_name %in% tolower(days)
        } else {
          format(date, "%u") == "1"  # Default: Monday
        }
      } else {
        format(date, "%u") == "1"  # Default: Monday
      }
    },
    "monthly" = format(date, "%d") == "01",
    "once" = FALSE,  # Manual assignment only
    FALSE
  )
}

# =============================================================================
# ROTATION HELPERS
# =============================================================================

#' Get Rotation Config
#'
#' @param chore_id The chore ID.
#'
#' @return Rotation config data frame or NULL.
#'
#' @keywords internal
get_rotation_config <- function(chore_id) {
  result <- db_query("
    SELECT * FROM chore_rotations WHERE chore_id = ? AND is_active = TRUE
  ", params = list(chore_id))
  if (nrow(result) == 0) NULL else result[1, ]
}

#' Setup Rotation
#'
#' Creates a rotation configuration for a chore.
#'
#' @param chore_id The chore ID.
#' @param member_ids Vector of member IDs in rotation order.
#' @param frequency Rotation frequency: 'daily', 'weekly', 'monthly'.
#'
#' @return The rotation config ID.
#'
#' @export
setup_rotation <- function(chore_id, member_ids, frequency = "weekly") {
  # Deactivate any existing rotation
  db_execute("
    UPDATE chore_rotations SET is_active = FALSE WHERE chore_id = ?
  ", params = list(chore_id))

  db_execute("
    INSERT INTO chore_rotations (chore_id, rotation_members, rotation_frequency, current_index)
    VALUES (?, ?, ?, 0)
  ", params = list(
    chore_id,
    jsonlite::toJSON(member_ids),
    frequency
  ))

  result <- db_query("SELECT MAX(id) as id FROM chore_rotations")
  result$id[1]
}

#' Get Rotation Assignee
#'
#' Gets the current assignee based on rotation.
#'
#' @param chore_id The chore ID.
#'
#' @return The member ID who should be assigned, or NULL.
#'
#' @keywords internal
get_rotation_assignee <- function(chore_id) {
  rotation <- get_rotation_config(chore_id)
  if (is.null(rotation)) return(NULL)

  members <- tryCatch(
    jsonlite::fromJSON(rotation$rotation_members),
    error = function(e) NULL
  )
  if (is.null(members) || length(members) == 0) return(NULL)

  current_idx <- rotation$current_index %% length(members)
  members[current_idx + 1]
}

#' Advance Rotation
#'
#' Moves to the next person in the rotation.
#'
#' @param chore_id The chore ID.
#'
#' @return TRUE on success.
#'
#' @export
advance_rotation <- function(chore_id) {
  db_execute("
    UPDATE chore_rotations
    SET current_index = current_index + 1,
        last_rotated_at = CURRENT_TIMESTAMP
    WHERE chore_id = ? AND is_active = TRUE
  ", params = list(chore_id))
  invisible(TRUE)
}

#' Remove Rotation
#'
#' Deactivates rotation for a chore.
#'
#' @param chore_id The chore ID.
#'
#' @return TRUE on success.
#'
#' @export
remove_rotation <- function(chore_id) {
  db_execute("
    UPDATE chore_rotations SET is_active = FALSE WHERE chore_id = ?
  ", params = list(chore_id))
  invisible(TRUE)
}
