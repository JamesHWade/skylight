#' Load Brand Configuration
#'
#' Loads the brand.yml configuration file for theming.
#'
#' @param path Optional path to brand.yml file. If NULL, searches in
#'   standard locations.
#'
#' @return A list containing brand configuration.
#'
#' @keywords internal
load_brand <- function(path = NULL) {
  # Search order for brand.yml
  if (is.null(path)) {
    search_paths <- c(
      "brand.yml",
      "inst/brand.yml",
      app_sys("brand.yml"),
      app_sys("app/brand.yml")
    )

    for (p in search_paths) {
      if (file.exists(p)) {
        path <- p
        break
      }
    }
  }

  # Return defaults if no brand.yml found
  if (is.null(path) || !file.exists(path)) {
    return(default_brand())
  }

  # Load and parse YAML
  tryCatch({
    brand <- yaml::read_yaml(path)
    validate_brand(brand)
    resolve_brand_colors(brand)
  }, error = function(e) {
    warning("Failed to load brand.yml: ", e$message, ". Using defaults.")
    default_brand()
  })
}

#' Default Brand Configuration
#'
#' Returns the default brand configuration when brand.yml is not available.
#'
#' @return A list with default brand settings.
#'
#' @keywords internal
default_brand <- function() {
  list(
    meta = list(
      name = "Skylight Calendar"
    ),
    color = list(
      palette = list(
        white = "#FFFFFF",
        `warm-white` = "#FDF8F3",
        charcoal = "#2D3436",
        `soft-blue` = "#74B9FF",
        coral = "#FF7675",
        mint = "#55EFC4",
        lavender = "#A29BFE",
        sunshine = "#FFEAA7"
      ),
      primary = "#74B9FF",
      secondary = "#FF7675",
      background = "#FDF8F3",
      foreground = "#2D3436"
    ),
    typography = list(
      fonts = list(
        list(family = "Inter", source = "google"),
        list(family = "DM Sans", source = "google")
      ),
      base = list(
        family = "Inter",
        size = "18px"
      ),
      headings = list(
        family = "DM Sans",
        weight = 600
      )
    )
  )
}

#' Validate Brand Configuration
#'
#' Ensures required fields are present in the brand configuration.
#'
#' @param brand Brand configuration list.
#'
#' @return The validated brand configuration.
#'
#' @keywords internal
validate_brand <- function(brand) {
  defaults <- default_brand()

  # Ensure required sections exist
  if (is.null(brand$color)) {
    brand$color <- defaults$color
  }

  if (is.null(brand$typography)) {
    brand$typography <- defaults$typography
  }

  brand
}

#' Resolve Brand Color References
#'
#' Resolves color references (e.g., "soft-blue" -> "#74B9FF").
#'
#' @param brand Brand configuration list.
#'
#' @return Brand configuration with resolved colors.
#'
#' @keywords internal
resolve_brand_colors <- function(brand) {
 palette <- brand$color$palette %||% list()

  # Normalize palette keys (remove hyphens, make lowercase)
  palette_normalized <- stats::setNames(
    as.list(unlist(palette)),
    gsub("-", "", tolower(names(palette)))
  )

  # Function to resolve a single color
  resolve_color <- function(color) {
    if (is.null(color)) {
      return(NULL)
    }

    # If it's a hex color, return as-is
    if (grepl("^#[0-9A-Fa-f]{6}$", color)) {
      return(color)
    }

    # Normalize the lookup key
    lookup_key <- gsub("-", "", tolower(color))

    # Try to resolve from palette (try multiple key formats)
    result <- palette[[color]] %||%
              palette[[gsub("-", "_", color)]] %||%
              palette_normalized[[lookup_key]] %||%
              NULL

    # Return resolved color or a default
    if (!is.null(result) && grepl("^#[0-9A-Fa-f]{6}$", result)) {
      return(result)
    }

    # Return default if resolution failed
    "#74B9FF"
  }

  # Resolve standard color fields
  brand$color$primary <- resolve_color(brand$color$primary)
  brand$color$secondary <- resolve_color(brand$color$secondary)
  brand$color$background <- resolve_color(brand$color$background)
  brand$color$foreground <- resolve_color(brand$color$foreground)

  brand
}

#' Get Brand Color
#'
#' Retrieves a specific color from the brand configuration.
#'
#' @param name Color name (e.g., "primary", "coral", "soft-blue").
#' @param brand Optional brand configuration. If NULL, loads from brand.yml.
#'
#' @return A hex color string.
#'
#' @keywords internal
get_brand_color <- function(name, brand = NULL) {
  if (is.null(brand)) {
    brand <- load_brand()
  }

  # Check direct color assignments first
  if (!is.null(brand$color[[name]])) {
    return(brand$color[[name]])
  }

  # Check palette
  if (!is.null(brand$color$palette[[name]])) {
    return(brand$color$palette[[name]])
  }

  # Try with underscores
  name_underscore <- gsub("-", "_", name)
  if (!is.null(brand$color$palette[[name_underscore]])) {
    return(brand$color$palette[[name_underscore]])
  }

  # Default fallback
  "#74B9FF"
}

#' Get Brand Font
#'
#' Retrieves font family from the brand configuration.
#'
#' @param type Font type: "base" or "headings".
#' @param brand Optional brand configuration.
#'
#' @return Font family string.
#'
#' @keywords internal
get_brand_font <- function(type = "base", brand = NULL) {
  if (is.null(brand)) {
    brand <- load_brand()
  }

  if (type == "base") {
    brand$typography$base$family %||% "Inter"
  } else if (type == "headings") {
    brand$typography$headings$family %||% "DM Sans"
  } else {
    "Inter"
  }
}

#' Generate CSS Variables from Brand
#'
#' Creates CSS custom properties from brand configuration.
#'
#' @param brand Optional brand configuration.
#'
#' @return A character string of CSS.
#'
#' @keywords internal
brand_to_css_vars <- function(brand = NULL) {
  if (is.null(brand)) {
    brand <- load_brand()
  }

  vars <- c()

  # Add palette colors
  if (!is.null(brand$color$palette)) {
    for (name in names(brand$color$palette)) {
      css_name <- gsub("_", "-", name)
      vars <- c(vars, glue::glue("  --brand-{css_name}: {brand$color$palette[[name]]};"))
    }
  }

  # Add semantic colors
  vars <- c(vars, glue::glue("  --brand-primary: {brand$color$primary %||% '#74B9FF'};"))
  vars <- c(vars, glue::glue("  --brand-secondary: {brand$color$secondary %||% '#FF7675'};"))
  vars <- c(vars, glue::glue("  --brand-bg: {brand$color$background %||% '#FDF8F3'};"))
  vars <- c(vars, glue::glue("  --brand-fg: {brand$color$foreground %||% '#2D3436'};"))

  # Add typography
  vars <- c(vars, glue::glue("  --brand-font-base: '{get_brand_font('base', brand)}', sans-serif;"))
  vars <- c(vars, glue::glue("  --brand-font-headings: '{get_brand_font('headings', brand)}', sans-serif;"))

  paste0(":root {\n", paste(vars, collapse = "\n"), "\n}")
}
