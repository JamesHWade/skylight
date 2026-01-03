# Skylight Calendar - Shiny App Entry Point
#
# This file allows running the app directly via:
# - RStudio's "Run App" button
# - VS Code Shiny extension
# - shiny::runApp("inst/app")

# Suppress bslib color contrast warnings
options(bslib.color_contrast_warnings = FALSE)

# Load the package (change to library(skylight) for deployment)
devtools::load_all()

# Run the app
shiny::shinyApp(
  ui = app_ui(),
  server = app_server, 
)
