# Skylight Calendar — Complete Roadmap

A family calendar display app for iPad, inspired by Skylight Calendar, built as a modern R package with Shiny and Google Calendar integration.

---

## Table of Contents

1. [Product Vision](#product-vision)
2. [Feature Roadmap](#feature-roadmap)
3. [Technical Architecture](#technical-architecture)
4. [Implementation Phases](#implementation-phases)
5. [API Reference](#api-reference)
6. [Deployment Guide](#deployment-guide)

---

## Product Vision

### What We're Building

A beautiful, always-on calendar display for iPad that:
- Shows the family's week at a glance
- Syncs automatically with Google Calendar
- Requires zero daily interaction (glanceable)
- Feels warm and inviting, not clinical
- Includes AI-powered natural language interaction via chat

### Design Principles

| Principle | Implementation |
|-----------|----------------|
| **Glanceable** | Large text, high contrast, minimal clutter |
| **Ambient** | Subtle animations, no jarring updates |
| **Family-friendly** | Warm colors, readable from across the room |
| **Reliable** | Works offline, recovers gracefully, auto-refreshes |
| **Conversational** | Natural language event creation and queries via AI chat |

---

## Feature Roadmap

### Phase 1: Core Calendar (MVP)

The minimum viable product to replace a wall calendar.

| Feature | Description | Priority |
|---------|-------------|----------|
| **Weekly View** | 7-day grid showing all events | P0 |
| **Today Highlight** | Visual emphasis on current day | P0 |
| **Live Clock** | Current time display, updates every second | P0 |
| **Event Display** | Title, time, color per event | P0 |
| **All-Day Events** | Special treatment for full-day events | P0 |
| **Multi-Calendar Support** | Aggregate multiple Google calendars | P0 |
| **Color Coding** | Each calendar gets a distinct color | P0 |
| **Auto-Refresh** | Poll for updates every 5 minutes | P0 |
| **Week Navigation** | Previous/next week, jump to today | P0 |
| **OAuth Authentication** | Secure Google Calendar connection | P0 |
| **Offline Resilience** | Show cached events when offline | P1 |
| **Pull to Refresh** | Manual refresh gesture | P1 |

### Phase 2: Enhanced Display

Polish and additional views for better usability.

| Feature | Description | Priority |
|---------|-------------|----------|
| **Month View** | Optional 30-day overview | P1 |
| **Day View** | Detailed single-day schedule | P1 |
| **Agenda View** | Scrolling list of upcoming events | P1 |
| **Event Details Modal** | Tap event to see full details, location, notes | P1 |
| **Current Time Indicator** | Red line showing "now" in day view | P1 |
| **Recurring Event Icons** | Visual indicator for repeating events | P2 |
| **Event Duration Bars** | Visual representation of event length | P2 |
| **Smart Density** | Adjust text size based on event count | P2 |
| **Landscape/Portrait** | Support both orientations | P1 |
| **Dark Mode** | Reduced brightness for evening display | P1 |
| **Auto Dark Mode** | Switch based on time of day | P2 |

### Phase 3: Family Features

Features that make this a family hub, not just a calendar.

| Feature | Description | Priority |
|---------|-------------|----------|
| **Weather Widget** | Current conditions + forecast | P1 |
| **Photo Frame Mode** | Slideshow when idle (Google Photos integration) | P2 |
| **Birthdays & Anniversaries** | Special display for annual events | P2 |
| **Countdown Widget** | "X days until vacation" style counters | P2 |
| **Chore List** | Simple task list synced with Google Tasks | P2 |
| **Meal Planner** | Weekly meal display (manual or from calendar) | P3 |
| **Family Member Avatars** | Show who's involved in each event | P3 |
| **School Calendar Import** | Support for ICS feeds from schools | P2 |
| **Holiday Highlighting** | Auto-detect and style holidays | P2 |

### Phase 4: Interactivity

Touch features for when family members interact directly.

| Feature | Description | Priority |
|---------|-------------|----------|
| **Quick Add Event** | Tap a day to add event (syncs back to Google) | P2 |
| **Drag to Reschedule** | Move events by dragging | P3 |
| **Voice Add** | "Hey, add soccer practice Tuesday at 4" | P3 |
| **Family Notes** | Sticky notes on specific days | P2 |
| **RSVP Display** | Show who's attending shared events | P3 |
| **Swipe Navigation** | Swipe left/right to change weeks | P1 |
| **Pinch to Zoom** | Switch between week/month views | P2 |

### Phase 5: Notifications & Automation

Proactive features that anticipate needs.

| Feature | Description | Priority |
|---------|-------------|----------|
| **Upcoming Event Alert** | Screen flash or sound for imminent events | P2 |
| **Morning Briefing** | Summary screen shown at configured time | P3 |
| **Calendar Conflicts** | Highlight overlapping events | P2 |
| **Travel Time Warnings** | "Leave now" based on event location | P3 |
| **Weekly Digest** | Email summary of upcoming week | P3 |

### Phase 6: Multi-Device & Admin

For households with multiple displays or admin needs.

| Feature | Description | Priority |
|---------|-------------|----------|
| **Web Admin Panel** | Configure settings from computer | P2 |
| **Multiple Display Sync** | Same config across multiple iPads | P3 |
| **Individual Views** | Different calendars per device | P3 |
| **Kiosk Mode** | Lock iPad to calendar app only | P2 |
| **Wake on Motion** | Screen on when someone approaches (if supported) | P3 |
| **Screensaver Mode** | Dim/clock display when inactive | P2 |

---

## Technical Architecture

### System Overview

```
┌─────────────────────────────────────────────────────────────────┐
│                         iPad (Browser)                          │
│  ┌─────────────────────────────────────────────────────────┐   │
│  │                   Shiny Application                      │   │
│  │  ┌─────────────┐  ┌─────────────┐  ┌─────────────────┐  │   │
│  │  │  WeekView   │  │  DayView    │  │  AgendaView     │  │   │
│  │  │  (bslib)    │  │  (bslib)    │  │  (bslib)        │  │   │
│  │  └─────────────┘  └─────────────┘  └─────────────────┘  │   │
│  │  ┌─────────────┐  ┌─────────────┐  ┌─────────────────┐  │   │
│  │  │  Weather    │  │  AI Chat    │  │  ChoreList      │  │   │
│  │  │  Widget     │  │ (shinychat) │  │  Widget         │  │   │
│  │  └─────────────┘  └─────────────┘  └─────────────────┘  │   │
│  └─────────────────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────────────────┘
                             │ WebSocket (Shiny)
                             ▼
┌─────────────────────────────────────────────────────────────────┐
│                    R/Shiny Server                               │
│  ┌──────────────────────────────────────────────────────────┐  │
│  │                   skylight Package                        │  │
│  │  ┌────────────┐  ┌────────────┐  ┌────────────────────┐  │  │
│  │  │ UI Modules │  │ Server     │  │ AI Integration     │  │  │
│  │  │ (bslib +   │  │ Logic      │  │ (ellmer +          │  │  │
│  │  │ brand.yml) │  │            │  │  shinychat)        │  │  │
│  │  └────────────┘  └────────────┘  └────────────────────┘  │  │
│  └──────────────────────────────────────────────────────────┘  │
│                              │                                  │
│  ┌──────────────┐  ┌────────┴────────┐  ┌─────────────────┐   │
│  │ OAuth Tokens │  │ Calendar Module  │  │ LLM Provider    │   │
│  │ (httr2)      │  │ (googlecalendar) │  │ (ellmer)        │   │
│  └──────────────┘  └─────────────────┘  └─────────────────┘   │
└─────────────────────────────────────────────────────────────────┘
                             │
                             ▼
┌─────────────────────────────────────────────────────────────────┐
│                     External Services                           │
│  ┌────────────────┐  ┌────────────────┐  ┌─────────────────┐   │
│  │ Google         │  │ OpenWeatherMap │  │ LLM APIs        │   │
│  │ Calendar API   │  │ API            │  │ (Claude, etc.)  │   │
│  └────────────────┘  └────────────────┘  └─────────────────┘   │
└─────────────────────────────────────────────────────────────────┘
```

### Tech Stack

| Layer | Technology | Rationale |
|-------|------------|-----------|
| **Framework** | Shiny | Reactive web apps in R, real-time updates |
| **UI Components** | bslib | Modern Bootstrap 5, responsive layouts |
| **Theming** | brand.yml | Centralized brand/design tokens |
| **AI Chat** | shinychat | Chat UI components for LLM interaction |
| **LLM Integration** | ellmer | Unified interface to LLM providers |
| **Auth** | httr2 + Google OAuth 2.0 | Secure token management |
| **Calendar API** | googlecalendar / gargle | Google Calendar integration |
| **Weather** | OpenWeatherMap API | Free tier sufficient |
| **Data Storage** | DuckDB | Fast, embedded analytics database |
| **Package Mgmt** | renv | Reproducible dependencies |
| **Deployment** | Shiny Server / Posit Connect / Docker | Production hosting |

### R Package Dependencies

```r
# Core Shiny stack
shiny        # Web application framework
bslib        # Bootstrap 5 theming and components
htmltools    # HTML generation utilities

# AI/Chat
shinychat    # Chat interface components
ellmer       # LLM provider abstraction

# Google Integration
gargle       # Google API authentication
httr2        # HTTP requests with OAuth support

# Data Storage
duckdb       # Embedded analytics database
DBI          # Database interface

# Utilities
lubridate    # Date/time manipulation
jsonlite     # JSON parsing
cachem       # Caching layer
```

### Project Structure

```
skylight/
├── DESCRIPTION                   # R package metadata
├── NAMESPACE                     # Package exports
├── LICENSE
├── README.md
├── VISION.md                     # This file
├── renv.lock                     # Locked dependencies
├── brand.yml                     # Brand/theme configuration
├── .Renviron                     # Environment secrets (gitignored)
├── .Renviron.example             # Template for secrets
│
├── R/
│   ├── skylight-package.R        # Package documentation
│   ├── run_app.R                 # Main app launcher
│   │
│   ├── app_ui.R                  # Main UI definition
│   ├── app_server.R              # Main server logic
│   │
│   ├── mod_calendar.R            # Calendar view module
│   ├── mod_week_view.R           # Weekly calendar display
│   ├── mod_day_view.R            # Daily calendar display
│   ├── mod_agenda_view.R         # Agenda list display
│   │
│   ├── mod_chat.R                # AI chat module (shinychat)
│   ├── mod_weather.R             # Weather widget module
│   ├── mod_clock.R               # Live clock module
│   ├── mod_chores.R              # Chore list module
│   │
│   ├── utils_calendar.R          # Calendar data utilities
│   ├── utils_google.R            # Google API helpers
│   ├── utils_auth.R              # OAuth token management
│   ├── utils_ai.R                # ellmer/LLM utilities
│   ├── utils_db.R                # DuckDB database helpers
│   ├── utils_theme.R             # brand.yml theming helpers
│   └── utils_dates.R             # Date formatting helpers
│
├── inst/
│   ├── app/
│   │   └── www/
│   │       ├── styles.css        # Custom CSS overrides
│   │       ├── custom.js         # Custom JavaScript
│   │       └── fonts/            # Self-hosted fonts
│   │
│   └── brand.yml                 # Default brand configuration
│
├── man/                          # Generated documentation
│
├── tests/
│   ├── testthat/
│   │   ├── test-calendar.R
│   │   ├── test-auth.R
│   │   └── test-utils.R
│   └── testthat.R
│
├── data/
│   └── skylight.duckdb           # Local DuckDB database (gitignored)
│
└── data-raw/
    └── settings.R                # Default settings generation
```

### brand.yml Configuration

The app uses `brand.yml` for centralized theming:

```yaml
meta:
  name: Skylight Calendar

color:
  palette:
    white: "#FFFFFF"
    warm-white: "#FDF8F3"
    charcoal: "#2D3436"
    soft-blue: "#74B9FF"
    coral: "#FF7675"
    mint: "#55EFC4"
    lavender: "#A29BFE"
    sunshine: "#FFEAA7"

  primary: soft-blue
  secondary: coral
  background: warm-white
  foreground: charcoal

typography:
  fonts:
    - family: Inter
      source: google
    - family: DM Sans
      source: google

  base:
    family: Inter
    size: 18px

  headings:
    family: DM Sans
    weight: 600

defaults:
  shiny:
    theme:
      preset: shiny
```

### AI Chat Integration

The app uses `ellmer` for LLM integration and `shinychat` for the UI:

```r
# Example chat module setup
mod_chat_server <- function(id, calendar_data) {
  moduleServer(id, function(input, output, session) {

    chat <- ellmer::chat_anthropic(
      system_prompt = "You are a helpful family calendar assistant..."
    )

    shinychat::chat_server(
      id = "chat",
      chat = chat,
      user_input = input$user_message
    )
  })
}
```

**AI Features:**
- Natural language event creation: "Add soccer practice Tuesday at 4pm"
- Calendar queries: "What's happening this weekend?"
- Smart suggestions: "You have a conflict on Thursday"
- Family reminders: "Remind me about the school play"

### DuckDB Data Storage

DuckDB provides fast, embedded analytics for local data persistence:

```r
# Database connection helper
get_db <- function() {
duckdb::dbConnect(
    duckdb::duckdb(),
    dbdir = app_sys("data/skylight.duckdb")
  )
}

# Example: Cache events locally
cache_events <- function(events) {
  con <- get_db()
  on.exit(DBI::dbDisconnect(con))

  DBI::dbWriteTable(con, "events", events, overwrite = TRUE)
}

# Example: Query cached events
get_cached_events <- function(start, end) {
  con <- get_db()
  on.exit(DBI::dbDisconnect(con))

  DBI::dbGetQuery(con, "
    SELECT * FROM events
    WHERE start >= ? AND end <= ?
    ORDER BY start
  ", params = list(start, end))
}
```

**DuckDB Use Cases:**
- **Event caching**: Persist Google Calendar events locally for offline access
- **Chat history**: Store conversation logs for context continuity
- **User preferences**: Save settings and calendar selections
- **Analytics**: Track usage patterns and popular views
- **Search**: Fast full-text search across cached events

---

## Implementation Phases

### Phase 1: Foundation

#### 1.1 R Package Setup

- [ ] Create R package structure with `usethis::create_package()`
- [ ] Set up `renv` for dependency management
- [ ] Configure `DESCRIPTION` with package metadata
- [ ] Add core dependencies: `shiny`, `bslib`, `htmltools`
- [ ] Create `.Renviron.example` template for secrets
- [ ] Set up basic `run_app()` function

#### 1.2 Google Cloud & OAuth Setup

- [ ] Create Google Cloud project
- [ ] Enable Google Calendar API
- [ ] Configure OAuth consent screen
  - App name: "Skylight Calendar"
  - Scopes: `calendar.readonly`
  - Test users: your email
- [ ] Create OAuth 2.0 credentials (Web application)
  - Redirect URI: `http://localhost:8080`
- [ ] Download credentials JSON
- [ ] Configure `gargle` for OAuth flow

#### 1.3 OAuth Flow with gargle/httr2

- [ ] Create `utils_auth.R` with OAuth helpers
- [ ] Implement `calendar_auth()` function
  - Use `gargle::token_fetch()` for auth
  - Cache tokens with `gargle::gargle_oauth_cache()`
- [ ] Implement `is_authenticated()` check
- [ ] Implement `calendar_deauth()` for logout
- [ ] Implement automatic token refresh
- [ ] Create auth UI module for Shiny

#### 1.4 Calendar API Integration

- [ ] Create `utils_google.R` for API helpers
- [ ] Create `utils_calendar.R` for data functions
- [ ] Implement `get_calendars()` function
  - List all accessible calendars
  - Return tibble with id, name, color
- [ ] Implement `get_events()` function
  - Accept start and end dates
  - Fetch from multiple calendars
  - Return normalized tibble
- [ ] Add `cachem` for in-memory caching (5 min TTL)
- [ ] Test functions interactively in R

#### 1.5 Shiny App Foundation

- [ ] Create `app_ui.R` with bslib layout
- [ ] Create `app_server.R` with main server logic
- [ ] Set up `brand.yml` for theming
- [ ] Implement `mod_auth.R` module
  - Show connect button when not authenticated
  - Show user info when authenticated
- [ ] Create reactive calendar data source
  - Wrap `get_events()` in `reactiveVal`
  - Auto-refresh with `reactiveTimer`
- [ ] Test app launches and auth works

#### 1.6 Week View Module

- [ ] Create `mod_week_view.R` module
- [ ] Build week grid layout with bslib
- [ ] Create `event_card()` UI component
- [ ] Implement week date calculations with lubridate
- [ ] Add today highlighting with CSS
- [ ] Add week navigation (prev/next/today buttons)
- [ ] Style with brand.yml colors and typography
- [ ] Add loading spinner and empty states

#### 1.7 Polish & Testing

- [ ] Create `mod_clock.R` for live time display
- [ ] Add CSS transitions and animations
- [ ] Test on iPad Safari
- [ ] Fix any responsive layout issues
- [ ] Add PWA manifest for "Add to Home Screen"
- [ ] Write testthat tests for utility functions

### Phase 2: Enhanced Display

- [ ] Create `mod_day_view.R` with hourly slots
- [ ] Create `mod_agenda_view.R` with scrolling list
- [ ] Add view switcher in navigation
- [ ] Create event details modal (bslib modal)
- [ ] Add current time indicator line
- [ ] Implement dark mode with bslib theming
- [ ] Add auto dark mode (time-based reactive)
- [ ] Support landscape + portrait layouts
- [ ] Add touch gesture support (shinyjs)

### Phase 3: AI Chat Integration

- [ ] Add `ellmer` and `shinychat` dependencies
- [ ] Create `mod_chat.R` module
- [ ] Configure ellmer with Anthropic provider
- [ ] Design chat system prompt for calendar assistant
- [ ] Implement calendar context injection
- [ ] Add natural language event parsing
- [ ] Create tool functions for calendar queries
- [ ] Style chat to match brand.yml theme

### Phase 4: Family Features

- [ ] Create `mod_weather.R` widget
- [ ] Integrate OpenWeatherMap API with httr2
- [ ] Implement birthday/anniversary detection
- [ ] Create countdown widget component
- [ ] Add Google Tasks API integration
- [ ] Create `mod_chores.R` module
- [ ] Implement holiday detection
- [ ] Add holiday styling

### Phase 5: Deployment

- [ ] Create Dockerfile for containerized deployment
- [ ] Configure for Shiny Server / Posit Connect
- [ ] Set up environment variable handling
- [ ] Configure for Raspberry Pi (optional)
- [ ] Set up systemd service for auto-start
- [ ] Configure iPad for always-on display
- [ ] Enable Guided Access (kiosk mode)
- [ ] Document complete setup process

---

## API Reference

### Package Functions

#### Authentication

```r
# Authenticate with Google Calendar
calendar_auth(
  email = NULL,
  cache = TRUE,
  use_oob = FALSE
)

# Check if authenticated
is_authenticated()
# Returns: TRUE/FALSE

# Clear authentication
calendar_deauth()
```

---

#### Calendar Data

```r
# List available calendars
get_calendars()
# Returns tibble:
# | id                              | name   | color   | primary |
# |--------------------------------|--------|---------|---------|
# | primary                        | James  | #4285F4 | TRUE    |
# | family@group.calendar.google   | Family | #0B8043 | FALSE   |

# Fetch events in date range
get_events(
  start = Sys.Date(),
  end = Sys.Date() + 7,
  calendars = NULL  # NULL = all calendars
)
# Returns tibble:
# | id     | calendar_id | title        | start               | end                 |
# |--------|-------------|--------------|---------------------|---------------------|
# | abc123 | primary     | Team Standup | 2024-01-15 09:00:00 | 2024-01-15 09:30:00 |
#
# Additional columns: all_day, location, description, color, recurring
```

---

#### App Entry Point

```r
# Run the Shiny application
run_app(
  host = "0.0.0.0",
  port = 8080,
  launch_browser = TRUE
)
```

---

### Shiny Modules

#### Week View Module

```r
# UI
mod_week_view_ui(id)

# Server
mod_week_view_server(
  id,
  events,           # reactive tibble of events
  selected_date     # reactive date
)
```

#### Chat Module

```r
# UI
mod_chat_ui(id)

# Server
mod_chat_server(

  id,
  calendar_data,    # reactive calendar context
  chat_provider     # ellmer chat object
)
```

---

### Settings

Settings are stored in `.Renviron` and/or `brand.yml`:

```r
# .Renviron
GOOGLE_CLIENT_ID=your_client_id
GOOGLE_CLIENT_SECRET=your_client_secret
ANTHROPIC_API_KEY=your_api_key
OPENWEATHER_API_KEY=your_weather_key
WEATHER_LOCATION="Midland, MI"

# Accessed via
Sys.getenv("GOOGLE_CLIENT_ID")
```

App configuration in `brand.yml` handles theming (see Technical Architecture section).

---

## Deployment Guide

### Option A: Docker (Recommended)

**Dockerfile:**

```dockerfile
FROM rocker/shiny:4.4.0

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

# Install R packages
RUN R -e "install.packages(c('shiny', 'bslib', 'htmltools', 'httr2', 'gargle', 'lubridate', 'jsonlite', 'cachem'), repos='https://cloud.r-project.org/')"
RUN R -e "install.packages(c('ellmer', 'shinychat'), repos='https://cloud.r-project.org/')"

# Copy app
COPY . /srv/shiny-server/skylight
WORKDIR /srv/shiny-server/skylight

# Install package
RUN R -e "devtools::install('.')"

EXPOSE 8080

CMD ["R", "-e", "skylight::run_app(host='0.0.0.0', port=8080)"]
```

**Run with Docker:**

```bash
# Build
docker build -t skylight .

# Run with environment variables
docker run -d \
  -p 8080:8080 \
  -e GOOGLE_CLIENT_ID=your_id \
  -e GOOGLE_CLIENT_SECRET=your_secret \
  -e ANTHROPIC_API_KEY=your_key \
  --name skylight \
  skylight
```

### Option B: Raspberry Pi

**Hardware:**
- Raspberry Pi 4 (4GB+ RAM recommended for R)
- MicroSD card (32GB+)
- Power supply

**Setup:**

```bash
# 1. Install R
sudo apt-get update
sudo apt-get install -y r-base r-base-dev

# 2. Install system dependencies
sudo apt-get install -y libcurl4-openssl-dev libssl-dev libxml2-dev

# 3. Clone repository
git clone https://github.com/youruser/skylight.git
cd skylight

# 4. Configure environment
cp .Renviron.example .Renviron
nano .Renviron  # Add your credentials

# 5. Install R packages and app
R -e "install.packages('renv'); renv::restore()"
R -e "devtools::install('.')"

# 6. Create systemd service
sudo nano /etc/systemd/system/skylight.service
```

**systemd service file:**

```ini
[Unit]
Description=Skylight Calendar
After=network.target

[Service]
Type=simple
User=pi
WorkingDirectory=/home/pi/skylight
ExecStart=/usr/bin/Rscript -e "skylight::run_app(host='0.0.0.0', port=8080, launch_browser=FALSE)"
Restart=on-failure
Environment=R_LIBS_USER=/home/pi/R/library

[Install]
WantedBy=multi-user.target
```

```bash
# 7. Enable and start
sudo systemctl enable skylight
sudo systemctl start skylight

# 8. Access at http://raspberrypi.local:8080
```

### Option C: Posit Connect / shinyapps.io

For managed cloud deployment:

**shinyapps.io:**
```r
# Deploy directly from R
rsconnect::deployApp(
  appDir = ".",
  appName = "skylight",
  account = "your-account"
)
```

**Posit Connect:**
1. Create manifest: `rsconnect::writeManifest()`
2. Push to git repository
3. Deploy via Connect dashboard
4. Configure environment variables in Connect

**Important:** Update Google OAuth redirect URI to production URL.

### iPad Configuration

1. **Add to Home Screen:**
   - Open Safari → Navigate to `http://your-server:8080`
   - Share → Add to Home Screen
   - Name it "Skylight"

2. **Guided Access (Kiosk Mode):**
   - Settings → Accessibility → Guided Access → On
   - Open calendar app
   - Triple-click side button → Start Guided Access
   - Disable hardware buttons, touch areas as needed

3. **Display Settings:**
   - Settings → Display → Auto-Lock → Never
   - Reduce brightness or enable Night Shift for evening

4. **Disable Notifications:**
   - Settings → Focus → Do Not Disturb schedule

---

## Future Considerations

### Potential Enhancements

- **Apple Calendar Support:** Use CalDAV protocol via `httr2` for iCloud calendars
- **Outlook Support:** Microsoft Graph API integration with `AzureGraph`
- **Voice Control:** Speech-to-text integration via ellmer for voice commands
- **Offline-first:** Full offline capability with DuckDB sync
- **E-ink Display:** Alternative to iPad for lower power consumption
- **Multi-language:** Internationalization support
- **Advanced AI Tools:** Calendar conflict resolution, smart scheduling suggestions

### Known Limitations

- OAuth tokens require initial browser-based authentication
- Google API quotas (10,000 requests/day free tier usually sufficient)
- iPad must stay powered and connected to WiFi
- No native push notifications (Shiny uses polling/websockets)
- R memory usage higher than Node.js for small apps

### R Ecosystem Advantages

- **Reactive programming:** Shiny's reactive model fits calendar updates naturally
- **Data manipulation:** Tidyverse tools for event processing
- **Visualization:** ggplot2 potential for calendar analytics
- **Statistics:** Built-in tools for usage analytics
- **Package ecosystem:** Easy to extend with CRAN packages

---

## Changelog

| Version | Date | Changes |
|---------|------|---------|
| 0.1.0 | TBD | Initial MVP - Week view with OAuth |
| 0.2.0 | TBD | Enhanced views (Day, Month, Agenda) |
| 0.3.0 | TBD | AI chat integration (ellmer + shinychat) |
| 0.4.0 | TBD | Weather widget, dark mode |
| 0.5.0 | TBD | Chore list, countdowns, DuckDB storage |
| 1.0.0 | TBD | Stable release, deployment docs |