# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Skylight is an R Shiny package for a family calendar display optimized for iPad. It integrates Google Calendar, includes an AI chat assistant (Claude via ellmer), weather widget, and offline support via DuckDB caching.

## Development Commands

```r
# Load package for development
devtools::load_all()

# Run the app
run_app()

# Run all tests
devtools::test()

# Run a single test file
testthat::test_file("tests/testthat/test-utils_dates.R")

# Rebuild documentation
devtools::document()

# Check package
devtools::check()
```

## Architecture

### Entry Point
- `run_app()` in `R/run_app.R` - starts the Shiny app, initializes DuckDB

### Core Structure
```
R/
├── app_ui.R          # Main UI using bslib::page_navbar
├── app_server.R      # Main server, orchestrates modules
├── mod_*.R           # Shiny modules (views + widgets)
└── utils_*.R         # Non-module utilities
```

### Module Pattern
Each module follows standard Shiny module convention with `mod_*_ui()` and `mod_*_server()` functions. View modules (week, month, day, agenda) receive shared reactive data:
- `events` - reactive of event data frame
- `selected_date` - reactiveVal for current date
- `calendars` - reactive of calendar list

### Key Subsystems

**Authentication (`utils_auth.R`)**: OAuth 2.0 flow with Google via httr2. Tokens cached in `rappdirs::user_cache_dir()`. Uses package environment `pkg_env` for runtime state.

**Calendar API (`utils_calendar.R`)**: `get_calendars()` and `get_events()` fetch from Google Calendar API. Events are cached in DuckDB. `create_event()` posts new events.

**Database (`utils_db.R`)**: DuckDB for offline caching. Tables: `events`, `chat_history`, `settings`. Location controlled by `SKYLIGHT_DB_PATH` env var.

**AI Chat (`mod_chat.R`)**: Uses ellmer package with `chat_claude()`. System prompt includes calendar context. Chat history stored in DuckDB.

**Offline Resilience (`utils_offline.R`)**: `fetch_events_resilient()` provides fallback to cached data when API unavailable.

**Theming (`utils_theme.R`, `inst/brand.yml`)**: bslib theming with brand.yml configuration. Includes light/dark mode.

### Demo Mode
When Google credentials are missing, app runs in demo mode with `generate_sample_events()` and `generate_sample_calendars()` from `utils_demo.R`.

## Environment Variables

Required:
- `GOOGLE_CLIENT_ID` / `GOOGLE_CLIENT_SECRET` - Google OAuth (or runs in demo mode)
- `ANTHROPIC_API_KEY` - for AI chat

Optional:
- `GEMINI_API_KEY` - AI-generated icons for chores and events (via gemini.R)
- `OPENWEATHER_API_KEY` - weather widget
- `WEATHER_LOCATION` - default weather location
- `SKYLIGHT_DB_PATH` - custom DuckDB path
- `SKYLIGHT_CACHE_DIR` - custom token cache

## Docker Deployment

```bash
# Build and run with docker-compose
docker-compose up -d

# View logs
docker-compose logs -f

# Stop
docker-compose down
```

Create a `.env` file with required environment variables:
```
ANTHROPIC_API_KEY=your_key
GOOGLE_CLIENT_ID=your_id
GOOGLE_CLIENT_SECRET=your_secret
GEMINI_API_KEY=your_key  # Optional, for AI icons
OPENWEATHER_API_KEY=your_key  # Optional, for weather
```

Data is persisted in Docker volumes:
- `skylight-data`: DuckDB database
- `skylight-cache`: OAuth token cache

## Issue Tracking

This project uses **bd** (beads) for issue tracking. See AGENTS.md for workflow details.
