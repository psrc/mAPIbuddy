#' Base geocoding function that handles common logic and returns EPSG:2285 coordinates
#'
#' @param address_line Character vector of street addresses
#' @param locality Character vector of cities/localities
#' @param postal_code Character vector of postal/ZIP codes
#' @param api_key Character string with API key
#' @param geocode_fn Function that performs the actual geocoding API call
#'
#' @return A data frame with x and y coordinates in EPSG:2285 (NAD83 / Washington North)
#'         or NA for failed requests
#' @importFrom sf st_as_sf st_point st_set_crs st_transform st_coordinates
geocode_base <- function(address_line, locality, postal_code, api_key, geocode_fn) {

  # Ensure inputs are vectors of the same length
  n <- max(length(address_line), length(locality), length(postal_code))
  address_line <- rep_len(address_line, n)
  locality <- rep_len(locality, n)
  postal_code <- rep_len(postal_code, n)

  # Initialize results data frame
  results <- data.frame(
    input_address = paste(address_line, locality, postal_code, sep = ", "),
    lat = rep(NA_real_, n),
    lng = rep(NA_real_, n),
    stringsAsFactors = FALSE
  )

  # Process each address using the provided geocoding function
  for (i in seq_len(n)) {
    # Clean address
    clean_address <- gsub(" Hwy ", " WA-", address_line[i])
    clean_address <- gsub("#", "", clean_address)

    # Call the specific geocoding function
    geocode_result <- geocode_fn(
      clean_address, locality[i], postal_code[i],
      api_key
    )

    # Update results with geocoding output (only coordinates)
    results$lat[i] <- geocode_result$lat
    results$lng[i] <- geocode_result$lng
  }

  # Initialize output data frame with NAD83 / Washington North coordinates
  output <- data.frame(
    input_address = results$input_address,
    x = rep(NA_real_, n),
    y = rep(NA_real_, n),
    stringsAsFactors = FALSE
  )

  # Transform coordinates only for valid points
  valid_coords <- !is.na(results$lng) & !is.na(results$lat)

  if (any(valid_coords)) {
    # Create sf object for valid coordinates
    valid_sf <- sf::st_as_sf(
      results[valid_coords, ],
      coords = c("lng", "lat"),
      crs = 4326  # WGS84
    )

    # Transform to NAD83 / Washington North (EPSG:2285)
    transformed_sf <- sf::st_transform(valid_sf, 2285)

    # Extract coordinates
    coords <- sf::st_coordinates(transformed_sf)

    # Update output data frame
    output$x[valid_coords] <- format(round(coords[, "X"], 2), nsmall = 2)
    output$y[valid_coords] <- format(round(coords[, "Y"], 2), nsmall = 2)
    output$xy <- paste(output$x, output$y, sep = ",")

  }

  return(output$xy)
}

#' Geocode a single address with Bing Maps API
#' @importFrom utils URLencode
#' @importFrom xml2 read_xml xml_text xml_find_first
#' @noRd
geocode_bing_single <- function(address, locality, postal_code, api_key) {
  result <- list(
    status = NA_character_,
    confidence = NA_character_,
    lat = NA_real_,
    lng = NA_real_
  )

  tryCatch({
    url <- paste0(
      "http://dev.virtualearth.net/REST/v1/Locations/",
      "?countryRegion=US",
      "&adminDistrict=WA",
      "&locality=", URLencode(locality),
      "&postalCode=", URLencode(postal_code),
      "&addressLine=", URLencode(address),
      "&key=", api_key,
      "&output=xml"
    )

    response <- xml2::read_xml(url)

    # Create a namespace manager for the default namespace
    # The default namespace is "http://schemas.microsoft.com/search/local/ws/rest/v1"
    ns <- c(rs = "http://schemas.microsoft.com/search/local/ws/rest/v1")

    # Use the namespace when querying
    status_code <- xml2::xml_text(xml2::xml_find_first(response, "//rs:StatusCode", ns))
    result$status <- status_code

    if (status_code == "200") {
      # Get geocode point coordinates - use the first one
      lat_node <- xml2::xml_find_first(response, "//rs:GeocodePoint/rs:Latitude", ns)
      lng_node <- xml2::xml_find_first(response, "//rs:GeocodePoint/rs:Longitude", ns)

      if (!is.na(lat_node) && !is.na(lng_node)) {
        lat <- xml2::xml_text(lat_node)
        lng <- xml2::xml_text(lng_node)

        result$lat <- as.numeric(lat)
        result$lng <- as.numeric(lng)
      }

      # Get confidence
      confidence_node <- xml2::xml_find_first(response, "//rs:Confidence", ns)
      if (!is.na(confidence_node)) {
        confidence <- xml2::xml_text(confidence_node)
        result$confidence <- confidence
      }
    }

  }, error = function(e) {
    result$status <- paste("Error:", e$message)
  })

  return(result)
}

#' Geocode a single address with Google Maps API
#' @importFrom utils URLencode
#' @importFrom xml2 read_xml xml_text xml_find_first
#' @noRd
geocode_google_single <- function(address, locality, postal_code, api_key) {
  result <- list(
    status = NA_character_,
    confidence = NA_character_,
    lat = NA_real_,
    lng = NA_real_
  )

  tryCatch({
    full_address <- paste(
      address,
      locality,
      "WA",
      postal_code,
      "US",
      sep = ", "
    )

    url <- paste0(
      "https://maps.googleapis.com/maps/api/geocode/xml",
      "?address=", URLencode(full_address),
      "&key=", api_key
    )

    response <- xml2::read_xml(url)

    status <- xml2::xml_text(xml2::xml_find_first(response, "//status"))
    result$status <- status

    if (status == "OK") {
      location_type <- xml2::xml_text(xml2::xml_find_first(response, "//location_type"))

      confidence <- switch(location_type,
                           "ROOFTOP" = "High",
                           "RANGE_INTERPOLATED" = "Medium",
                           "GEOMETRIC_CENTER" = "Medium",
                           "APPROXIMATE" = "Low",
                           "Low")

      result$confidence <- confidence

      if (confidence %in% c("High", "Medium")) {
        lat <- xml2::xml_text(xml2::xml_find_first(response, "//location/lat"))
        lng <- xml2::xml_text(xml2::xml_find_first(response, "//location/lng"))

        if (!is.na(lat) && !is.na(lng)) {
          result$lat <- as.numeric(lat)
          result$lng <- as.numeric(lng)
        }
      }
    }
  }, error = function(e) {
    result$status <- paste("Error:", e$message)
  })

  return(result)
}

#' Geocode addresses and return coordinates in NAD83 / Washington North (EPSG:2285)
#'
#' @param address_line Character vector of street addresses
#' @param locality Character vector of cities/localities
#' @param postal_code Character vector of postal/ZIP codes
#' @param api_key Character string with Bing Maps API key
#'
#' @return A vector with concatenated x and y coordinates (EPSG:2285)
#' @export
geocode_bing <- function(address_line, locality, postal_code, api_key) {
  geocode_base(address_line, locality, postal_code, api_key, geocode_bing_single)
}

#' Geocode addresses and return coordinates in NAD83 / Washington North (EPSG:2285)
#'
#' @param address_line Character vector of street addresses
#' @param locality Character vector of cities/localities
#' @param postal_code Character vector of postal/ZIP codes
#' @param api_key Character string with Google Maps API key
#'
#' @return A vector with concatenated x and y coordinates (EPSG:2285)
#' @export
geocode_google <- function(address_line, locality, postal_code, api_key) {
  geocode_base(address_line, locality, postal_code, api_key, geocode_google_single)
}
