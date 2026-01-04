#' Initialize Database
#'
#' Creates the DuckDB database and required tables if they don't exist.
#'
#' @return Invisibly returns TRUE on success.
#'
#' @keywords internal
db_init <- function() {
  con <- db_connect()
  on.exit(DBI::dbDisconnect(con))

  # Create events table
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS events (
      id VARCHAR PRIMARY KEY,
      calendar_id VARCHAR,
      calendar_name VARCHAR,
      title VARCHAR,
      start TIMESTAMP,
      end_time TIMESTAMP,
      all_day BOOLEAN,
      location VARCHAR,
      description VARCHAR,
      color VARCHAR,
      recurring BOOLEAN,
      status VARCHAR,
      cached_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )
  ")

  # Create sequence for chat history IDs (DuckDB doesn't auto-increment PRIMARY KEY)
  tryCatch(
    DBI::dbExecute(con, "CREATE SEQUENCE IF NOT EXISTS chat_history_id_seq"),
    error = function(e) NULL
  )

  # Migration: if chat_history table exists without the sequence default, recreate it
  # This handles databases created before the sequence was added
  if (DBI::dbExistsTable(con, "chat_history")) {
    # Check if we need to migrate by testing an insert
    needs_migration <- tryCatch({
      DBI::dbExecute(con, "
        INSERT INTO chat_history (user_message, assistant_response)
        VALUES ('_migration_test_', '_migration_test_')
      ")
      # Clean up test row
      DBI::dbExecute(con, "
        DELETE FROM chat_history WHERE user_message = '_migration_test_'
      ")
      FALSE
    }, error = function(e) {
      grepl("NOT NULL constraint", e$message)
    })

    if (needs_migration) {
      # Recreate table with proper sequence (preserving data)
      DBI::dbExecute(con, "ALTER TABLE chat_history RENAME TO chat_history_old")
      DBI::dbExecute(con, "
        CREATE TABLE chat_history (
          id INTEGER PRIMARY KEY DEFAULT nextval('chat_history_id_seq'),
          user_message VARCHAR,
          assistant_response VARCHAR,
          created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
        )
      ")
      # Copy old data
      DBI::dbExecute(con, "
        INSERT INTO chat_history (user_message, assistant_response, created_at)
        SELECT user_message, assistant_response, created_at FROM chat_history_old
      ")
      DBI::dbExecute(con, "DROP TABLE chat_history_old")
    }
  } else {
    # Create chat history table (new database)
    DBI::dbExecute(con, "
      CREATE TABLE IF NOT EXISTS chat_history (
        id INTEGER PRIMARY KEY DEFAULT nextval('chat_history_id_seq'),
        user_message VARCHAR,
        assistant_response VARCHAR,
        created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
      )
    ")
  }

  # Create settings table
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS settings (
      key VARCHAR PRIMARY KEY,
      value VARCHAR,
      updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )
  ")

  # Create event_icons table for AI-generated calendar event icons
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS event_icons (
      event_id VARCHAR PRIMARY KEY,
      icon_base64 VARCHAR NOT NULL,
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )
  ")

  # Create index on events start time
  DBI::dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_events_start ON events(start)
  ")


  # Create sequence for countdown_events if not exists
  tryCatch(
    DBI::dbExecute(con, "CREATE SEQUENCE IF NOT EXISTS countdown_events_id_seq"),
    error = function(e) NULL
  )

  # Create countdown_events table for tracking special events
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS countdown_events (
      id INTEGER PRIMARY KEY DEFAULT nextval('countdown_events_id_seq'),
      event_id VARCHAR,
      title VARCHAR NOT NULL,
      target_date DATE NOT NULL,
      emoji VARCHAR DEFAULT '🎉',
      icon_base64 VARCHAR,
      color VARCHAR DEFAULT '#6366f1',
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      UNIQUE(event_id)
    )
  ")

  # Migration: add icon_base64 column if it doesn't exist
  tryCatch({
    DBI::dbExecute(con, "ALTER TABLE countdown_events ADD COLUMN icon_base64 VARCHAR")
  }, error = function(e) NULL)

  # Initialize chores tables
  db_init_chores(con)

  invisible(TRUE)
}

#' Connect to Database
#'
#' Opens a connection to the DuckDB database.
#'
#' @return A DBI connection object.
#'
#' @keywords internal
db_connect <- function() {
  db_path <- get_db_path()

  # Ensure directory exists
  db_dir <- dirname(db_path)
  if (!dir.exists(db_dir)) {
    dir.create(db_dir, recursive = TRUE)
  }

  duckdb::dbConnect(
    duckdb::duckdb(),
    dbdir = db_path,
    read_only = FALSE
  )
}

#' Get Database Path
#'
#' Returns the path to the DuckDB database file.
#'
#' @return Character string with the database path.
#'
#' @keywords internal
get_db_path <- function() {
  # Check for environment variable override
  db_path <- Sys.getenv("SKYLIGHT_DB_PATH")

  if (nchar(db_path) == 0) {
    # Default to user data directory
    db_path <- file.path(
      rappdirs::user_data_dir("skylight"),
      "skylight.duckdb"
    )
  }

  db_path
}

#' Execute Database Query
#'
#' A convenience wrapper for executing SQL queries.
#'
#' @param query SQL query string.
#' @param params Optional list of query parameters.
#'
#' @return Query result for SELECT, or number of affected rows.
#'
#' @keywords internal
db_query <- function(query, params = NULL) {
  con <- db_connect()
  on.exit(DBI::dbDisconnect(con))

  if (is.null(params)) {
    DBI::dbGetQuery(con, query)
  } else {
    DBI::dbGetQuery(con, query, params = params)
  }
}

#' Execute Database Statement
#'
#' A convenience wrapper for executing SQL statements (INSERT, UPDATE, DELETE).
#'
#' @param statement SQL statement string.
#' @param params Optional list of statement parameters.
#'
#' @return Number of affected rows.
#'
#' @keywords internal
db_execute <- function(statement, params = NULL) {
  con <- db_connect()
  on.exit(DBI::dbDisconnect(con))

  if (is.null(params)) {
    DBI::dbExecute(con, statement)
  } else {
    DBI::dbExecute(con, statement, params = params)
  }
}

#' Save Setting
#'
#' Stores a setting value in the database.
#'
#' @param key Setting key.
#' @param value Setting value (will be converted to JSON).
#'
#' @keywords internal
db_save_setting <- function(key, value) {
  value_json <- tryCatch(
    jsonlite::toJSON(value, auto_unbox = TRUE),
    error = function(e) {
      stop("Cannot save setting '", key, "': value is not JSON-serializable. ",
           "Error: ", conditionMessage(e), call. = FALSE)
    }
  )

  # DuckDB uses ON CONFLICT syntax (not INSERT OR REPLACE like SQLite)
  db_execute("
    INSERT INTO settings (key, value, updated_at)
    VALUES (?, ?, CURRENT_TIMESTAMP)
    ON CONFLICT (key) DO UPDATE SET
      value = EXCLUDED.value,
      updated_at = CURRENT_TIMESTAMP
  ", params = list(key, as.character(value_json)))

  invisible(TRUE)
}

#' Get Setting
#'
#' Retrieves a setting value from the database.
#'
#' @param key Setting key.
#' @param default Default value if setting doesn't exist.
#'
#' @return The setting value, or the default.
#'
#' @keywords internal
db_get_setting <- function(key, default = NULL) {
  result <- db_query("
    SELECT value FROM settings WHERE key = ?
  ", params = list(key))

  if (nrow(result) == 0) {
    return(default)
  }

  tryCatch(
    jsonlite::fromJSON(result$value[1]),
    error = function(e) {
      warning("Setting '", key, "' contains invalid JSON. Returning default value.",
              call. = FALSE)
      default
    }
  )
}

#' Clear Old Data
#'
#' Removes old cached data from the database.
#'
#' @param days Number of days to keep. Default is 30.
#'
#' @return Number of deleted rows.
#'
#' @keywords internal
db_cleanup <- function(days = 30) {
  cutoff <- Sys.time() - as.difftime(days, units = "days")

  events_deleted <- db_execute("
    DELETE FROM events WHERE cached_at < ?
  ", params = list(as.character(cutoff)))

  chat_deleted <- db_execute("
    DELETE FROM chat_history WHERE created_at < ?
  ", params = list(as.character(cutoff)))

  events_deleted + chat_deleted
}

#' Get Database Statistics
#'
#' Returns statistics about the database.
#'
#' @return A list with database stats.
#'
#' @keywords internal
db_stats <- function() {
  events_count <- db_query("SELECT COUNT(*) as n FROM events")$n
  chat_count <- db_query("SELECT COUNT(*) as n FROM chat_history")$n
  settings_count <- db_query("SELECT COUNT(*) as n FROM settings")$n

  db_path <- get_db_path()
  db_size <- if (file.exists(db_path)) {
    file.info(db_path)$size
  } else {
    0
  }

  list(
    events = events_count,
    chat_messages = chat_count,
    settings = settings_count,
    size_bytes = db_size,
    size_mb = round(db_size / 1024 / 1024, 2),
    path = db_path
  )
}
