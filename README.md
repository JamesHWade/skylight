# skylight

A beautiful, always-on family calendar display app built with R and Shiny.

## Features

- **Week/Day/Agenda Views** - Multiple ways to visualize your schedule
- **Google Calendar Integration** - Syncs with your existing calendars
- **AI Chat Assistant** - Natural language calendar queries powered by Claude
- **Modern UI** - Built with bslib and brand.yml theming
- **Offline Support** - DuckDB caching for reliable access
- **iPad Optimized** - Designed for always-on tablet display

## Installation

```r
# Install from GitHub
# install.packages("pak")
pak::pak("coatless-rpkg/skylight")
```

## Quick Start

1. Copy `.Renviron.example` to `.Renviron` and add your API keys:

```bash
cp .Renviron.example .Renviron
```

2. Edit `.Renviron` with your credentials:
   - `GOOGLE_CLIENT_ID` and `GOOGLE_CLIENT_SECRET` from [Google Cloud Console](https://console.cloud.google.com/)
   - `ANTHROPIC_API_KEY` from [Anthropic Console](https://console.anthropic.com/)

3. Run the app:

```r
library(skylight)
run_app()
```

## Tech Stack

| Component | Technology |
|-----------|------------|
| Framework | Shiny |
| UI | bslib + Bootstrap 5 |
| Theming | brand.yml |
| AI Chat | ellmer + shinychat |
| Auth | httr2 + Google OAuth |
| Database | DuckDB |

## Development

```r
# Install development dependencies
pak::pak(c("devtools", "testthat", "pkgload"))

# Load package for development
devtools::load_all()

# Run tests
devtools::test()

# Run the app
run_app()
```

## License

MIT