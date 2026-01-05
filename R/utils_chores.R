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
  # Helper to create sequences (logs warning on unexpected errors)
  create_sequence <- function(name) {
    tryCatch(
      DBI::dbExecute(con, paste0("CREATE SEQUENCE IF NOT EXISTS ", name, "_id_seq")),
      error = function(e) {
        # "already exists" is expected and safe to ignore
        if (!grepl("already exists", conditionMessage(e), ignore.case = TRUE)) {
          warning("Failed to create sequence ", name, "_id_seq: ", conditionMessage(e))
        }
        NULL
      }
    )
  }

  # Create sequences for auto-increment IDs
  sequences <- c("family_members", "chores", "chore_assignments",
                 "chore_completions", "chore_rotations")
  lapply(sequences, create_sequence)

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
      icon_base64 VARCHAR,
      is_active BOOLEAN DEFAULT TRUE,
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )
  ")

  # Migration: Add icon_base64 column to existing databases
  tryCatch({
    DBI::dbExecute(con, "ALTER TABLE chores ADD COLUMN icon_base64 VARCHAR")
  }, error = function(e) {
    # "already exists" or "duplicate column" is expected and safe to ignore
    if (!grepl("already|duplicate", conditionMessage(e), ignore.case = TRUE)) {
      warning("Migration failed (icon_base64): ", conditionMessage(e))
    }
  })

  # Migration: Add recurrence_rule column for flexible recurring options
  tryCatch({
    DBI::dbExecute(con, "ALTER TABLE chores ADD COLUMN recurrence_rule VARCHAR")
  }, error = function(e) {
    if (!grepl("already|duplicate", conditionMessage(e), ignore.case = TRUE)) {
      warning("Migration failed (recurrence_rule): ", conditionMessage(e))
    }
  })

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
  indexes <- list(
    "idx_assignments_date" = "CREATE INDEX IF NOT EXISTS idx_assignments_date ON chore_assignments(assigned_date)",
    "idx_assignments_member" = "CREATE INDEX IF NOT EXISTS idx_assignments_member ON chore_assignments(member_id)",
    "idx_assignments_status" = "CREATE INDEX IF NOT EXISTS idx_assignments_status ON chore_assignments(status)",
    "idx_completions_member" = "CREATE INDEX IF NOT EXISTS idx_completions_member ON chore_completions(member_id)",
    "idx_completions_date" = "CREATE INDEX IF NOT EXISTS idx_completions_date ON chore_completions(completed_at)"
  )
  for (idx_name in names(indexes)) {
    tryCatch({
      DBI::dbExecute(con, indexes[[idx_name]])
    }, error = function(e) {
      if (!grepl("already exists", conditionMessage(e), ignore.case = TRUE)) {
        warning("Failed to create index ", idx_name, ": ", conditionMessage(e))
      }
    })
  }

  invisible(TRUE)
}

# =============================================================================
# INPUT VALIDATION HELPERS
# =============================================================================

#' Validate Required String
#'
#' @param value The value to validate.
#' @param name The parameter name for error messages.
#' @param max_length Optional maximum length.
#'
#' @return The trimmed value if valid.
#' @keywords internal
validate_string <- function(value, name, max_length = 255) {

  if (is.null(value) || !is.character(value) || length(value) != 1) {
    stop(name, " must be a single character string", call. = FALSE)
  }
  value <- trimws(value)
  if (nchar(value) == 0) {
    stop(name, " cannot be empty", call. = FALSE)
  }
  if (nchar(value) > max_length) {
    stop(name, " exceeds maximum length of ", max_length, " characters", call. = FALSE)
  }
  value
}

#' Validate Positive Integer
#'
#' @param value The value to validate.
#' @param name The parameter name for error messages.
#' @param min Minimum value (default: 1).
#' @param max Maximum value (default: NULL for no max).
#'
#' @return The integer value if valid.
#' @keywords internal
validate_positive_int <- function(value, name, min = 1, max = NULL) {
  if (is.null(value) || !is.numeric(value) || length(value) != 1) {
    stop(name, " must be a single number", call. = FALSE)
  }
  value <- as.integer(value)
  if (value < min) {
    stop(name, " must be at least ", min, call. = FALSE)
  }
  if (!is.null(max) && value > max) {
    stop(name, " must be at most ", max, call. = FALSE)
  }
  value
}

#' Validate Enum Value
#'
#' @param value The value to validate.
#' @param name The parameter name for error messages.
#' @param allowed Vector of allowed values.
#'
#' @return The value if valid.
#' @keywords internal
validate_enum <- function(value, name, allowed) {
  if (is.null(value) || !value %in% allowed) {
    stop(name, " must be one of: ", paste(allowed, collapse = ", "), call. = FALSE)
  }
  value
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
  # Validate required fields
  name <- validate_string(name, "name", max_length = 100)
  avatar_emoji <- validate_string(avatar_emoji, "avatar_emoji", max_length = 10)

  # Validate optional fields (allow empty strings, just check length)
  if (!is.null(display_name) && nchar(trimws(display_name)) > 0) {
    if (nchar(display_name) > 100) {
      stop("display_name exceeds maximum length of 100 characters", call. = FALSE)
    }
    display_name <- trimws(display_name)
  } else {
    display_name <- NULL  # Treat empty string as NULL
  }

  # Validate color format (basic check for hex color)
  if (!grepl("^#[0-9A-Fa-f]{6}$", color)) {
    stop("color must be a valid hex color (e.g., #74B9FF)", call. = FALSE)
  }

  db_execute("
    INSERT INTO family_members (name, display_name, avatar_emoji, color, birth_date)
    VALUES (?, ?, ?, ?, ?)
  ", params = list(
    name,
    if (is.null(display_name)) NA_character_ else display_name,
    avatar_emoji,
    color,
    if (is.null(birth_date)) NA_character_ else as.character(birth_date)
  ))

  # Return the created member's ID
  result <- db_query("SELECT MAX(id) as id FROM family_members")
  result$id[1]
}

#' Dynamic Update Helper
#'
#' Builds and executes a parameterized UPDATE query with timestamp.
#'
#' @param table Table name.
#' @param id Record ID.
#' @param updates Named list of field=value pairs.
#' @param valid_fields Vector of allowed field names.
#'
#' @return TRUE on success.
#'
#' @keywords internal
db_update_record <- function(table, id, updates, valid_fields) {
  if (length(updates) == 0) return(invisible(TRUE))

  updates <- updates[names(updates) %in% valid_fields]
  if (length(updates) == 0) return(invisible(TRUE))

  set_clause <- paste(
    paste0(names(updates), " = ?"),
    collapse = ", "
  )
  set_clause <- paste(set_clause, ", updated_at = CURRENT_TIMESTAMP")

  db_execute(
    paste("UPDATE", table, "SET", set_clause, "WHERE id = ?"),
    params = c(unname(updates), list(id))
  )

  invisible(TRUE)
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
  db_update_record(
    table = "family_members",
    id = id,
    updates = list(...),
    valid_fields = c("name", "display_name", "avatar_emoji", "color", "birth_date", "is_active")
  )
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

  db_query(query, params = params)
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
#' @param frequency_days For weekly: which days (JSON array, e.g. `'["monday","wednesday"]'`).
#' @param category Category: 'kitchen', 'bedroom', 'bathroom', 'outdoor', 'general'.
#' @param estimated_minutes Estimated time in minutes (default: 15).
#' @param icon_emoji Emoji icon (default: broom).
#' @param icon_base64 Base64-encoded PNG icon image (optional).
#' @param recurrence_rule JSON string from create_recurrence_rule() for flexible scheduling.
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
                          icon_emoji = "\U0001F9F9",
                          icon_base64 = NULL,
                          recurrence_rule = NULL) {
  # Validate required fields
  title <- validate_string(title, "title", max_length = 200)
  points <- validate_positive_int(points, "points", min = 1, max = 1000)
  estimated_minutes <- validate_positive_int(estimated_minutes, "estimated_minutes", min = 1, max = 480)
  icon_emoji <- validate_string(icon_emoji, "icon_emoji", max_length = 10)


  # Validate enums
  frequency <- validate_enum(frequency, "frequency", c("daily", "weekly", "monthly", "once"))
  category <- validate_enum(category, "category", c(
    "general", "kitchen", "bedroom", "bathroom", "outdoor",
    "laundry", "living_room", "garage", "pets", "yard",
    "car", "errands", "meals", "organization", "maintenance"
  ))

  # Validate optional fields (allow empty strings, just check length)
  if (!is.null(description) && nchar(trimws(description)) > 0) {
    if (nchar(description) > 500) {
      stop("description exceeds maximum length of 500 characters", call. = FALSE)
    }
    description <- trimws(description)
  } else {
    description <- NULL  # Treat empty string as NULL
  }

  db_execute("
    INSERT INTO chores (title, description, points, frequency, frequency_days, category, estimated_minutes, icon_emoji, icon_base64, recurrence_rule)
    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
  ", params = list(
    title,
    if (is.null(description)) NA_character_ else description,
    points,
    frequency,
    if (is.null(frequency_days)) NA_character_ else frequency_days,
    category,
    estimated_minutes,
    icon_emoji,
    if (is.null(icon_base64)) NA_character_ else icon_base64,
    if (is.null(recurrence_rule)) NA_character_ else recurrence_rule
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
  db_update_record(
    table = "chores",
    id = id,
    updates = list(...),
    valid_fields = c("title", "description", "points", "frequency", "frequency_days",
                     "category", "estimated_minutes", "icon_emoji", "icon_base64",
                     "recurrence_rule", "is_active")
  )
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
      c.icon_base64 as chore_icon_base64,
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
      c.icon_base64 as chore_icon_base64,
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
  # Validate IDs
  chore_id <- validate_positive_int(chore_id, "chore_id")
  member_id <- validate_positive_int(member_id, "member_id")

  # Validate date
  if (!inherits(assigned_date, "Date")) {
    assigned_date <- tryCatch(
      as.Date(assigned_date),
      error = function(e) stop("assigned_date must be a valid date", call. = FALSE)
    )
  }

  # Verify chore exists
  chore <- get_chore(chore_id)
  if (is.null(chore)) {
    stop("Chore with ID ", chore_id, " not found", call. = FALSE)
  }

  # Verify member exists
  member <- get_family_member(member_id)
  if (is.null(member)) {
    stop("Family member with ID ", member_id, " not found", call. = FALSE)
  }

  db_execute("
    INSERT INTO chore_assignments (chore_id, member_id, assigned_date, due_time)
    VALUES (?, ?, ?, ?)
  ", params = list(
    chore_id,
    member_id,
    as.character(assigned_date),
    if (is.null(due_time)) NA_character_ else due_time
  ))

  result <- db_query("SELECT MAX(id) as id FROM chore_assignments")
  result$id[1]
}

#' Complete Assignment
#'
#' Marks an assignment as completed and records the completion.
#' Uses atomic conditional update to prevent race conditions.
#'
#' @param assignment_id The assignment ID.
#' @param notes Optional completion notes.
#' @param verified_by Optional member ID who verified.
#'
#' @return A list with `success` (TRUE/FALSE) and `completion_id` (if success).
#'
#' @export
complete_assignment <- function(assignment_id, notes = NULL, verified_by = NULL) {
  # Get assignment details (only if pending)
  assignment <- db_query("
    SELECT a.*, c.points as chore_points
    FROM chore_assignments a
    JOIN chores c ON a.chore_id = c.id
    WHERE a.id = ? AND a.status = 'pending'
  ", params = list(assignment_id))

  if (nrow(assignment) == 0) {
    return(list(success = FALSE, reason = "not_pending"))
  }

  # Calculate streak bonus
  streak_count <- get_current_streak(assignment$member_id[1])
  streak_bonus <- get_streak_bonus(streak_count + 1)  # +1 for this completion
  points_earned <- assignment$chore_points[1]

  # Atomic conditional update - only complete if still pending
  rows_updated <- db_execute("
    UPDATE chore_assignments
    SET status = 'completed'
    WHERE id = ? AND status = 'pending'
  ", params = list(assignment_id))

  # Check if update actually happened (race condition protection)
  if (is.null(rows_updated) || rows_updated == 0) {
    return(list(success = FALSE, reason = "already_completed"))
  }

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
    if (is.null(notes)) NA_character_ else notes,
    if (is.null(verified_by)) NA_integer_ else verified_by,
    streak_bonus
  ))

  result <- db_query("SELECT MAX(id) as id FROM chore_completions")
  list(success = TRUE, completion_id = result$id[1])
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
#' Uses atomic conditional update to prevent race conditions.
#'
#' @param assignment_id The assignment ID.
#'
#' @return A list with `success` (TRUE/FALSE).
#'
#' @export
uncomplete_assignment <- function(assignment_id) {
  # Atomic conditional update - only uncomplete if currently completed
  rows_updated <- db_execute("
    UPDATE chore_assignments
    SET status = 'pending'
    WHERE id = ? AND status = 'completed'
  ", params = list(assignment_id))

  # Check if update actually happened
  if (is.null(rows_updated) || rows_updated == 0) {
    return(list(success = FALSE, reason = "not_completed"))
  }

  # Remove completion record
  db_execute("
    DELETE FROM chore_completions WHERE assignment_id = ?
  ", params = list(assignment_id))

  list(success = TRUE)
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
      c.icon_emoji as chore_icon,
      c.icon_base64 as chore_icon_base64
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

  query <- paste(query, "ORDER BY cc.completed_at DESC LIMIT ?")
  params <- c(params, list(as.integer(limit)))

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
  db_query("
    SELECT
      cc.*,
      c.title as chore_title,
      c.icon_emoji as chore_icon,
      c.icon_base64 as chore_icon_base64,
      m.name as member_name,
      m.display_name as member_display_name,
      m.avatar_emoji as member_avatar
    FROM chore_completions cc
    JOIN chores c ON cc.chore_id = c.id
    JOIN family_members m ON cc.member_id = m.id
    ORDER BY cc.completed_at DESC
    LIMIT ?
  ", params = list(as.integer(limit)))
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
#' Determines if a chore should be assigned on a given date based on frequency
#' or recurrence rule.
#'
#' @param chore A single-row chore data frame.
#' @param date The date to check.
#'
#' @return TRUE if the chore should run, FALSE otherwise.
#'
#' @keywords internal
should_run_today <- function(chore, date) {
  # First check for recurrence_rule (flexible scheduling)
  if (!is.null(chore$recurrence_rule) && !is.na(chore$recurrence_rule) &&
      nchar(chore$recurrence_rule) > 0) {
    return(matches_recurrence(date, chore$recurrence_rule))
  }

  # Fall back to legacy frequency-based scheduling
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
