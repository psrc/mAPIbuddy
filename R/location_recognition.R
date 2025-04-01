#' Recognize location type using Bing Maps Location Recognition API
#'
#' @param lng Numeric vector of longitudes
#' @param lat Numeric vector of latitudes
#' @param key_b Character string with Bing Maps API key
#'
#' @return A character vector with location types ("Residential" or business types),
#'         or NA for failed requests
#' @importFrom xml2 read_xml xml_find_first xml_text
#' @export
loc_recognize <- function(lng, lat, key_b) {
  # Ensure inputs are vectors of the same length
  n <- max(length(lng), length(lat))
  lng <- rep_len(lng, n)
  lat <- rep_len(lat, n)

  # Initialize results vector
  results <- rep(NA_character_, n)

  # Process each location
  for (i in seq_len(n)) {
    tryCatch({
      # Check for NA inputs
      if (is.na(lng[i]) || is.na(lat[i])) {
        next
      }

      # Format URL
      url_template <- "http://dev.virtualearth.net/REST/v1/locationrecog/%s,%s?radius=.01&top=1&distanceunit=mi&key=%s&output=xml"
      url <- sprintf(
        url_template,
        as.character(lat[i]),   # Changed from trimws to as.character
        as.character(lng[i]),   # Changed from trimws to as.character
        trimws(key_b)
      )

      # Send request
      response <- xml2::read_xml(url)

      # Check status code - use a more precise XPath with namespace consideration
      status_code_node <- xml2::xml_find_first(response, "//*[local-name()='StatusCode']")
      if (is.na(status_code_node)) {
        next
      }
      status_code <- xml2::xml_text(status_code_node)
      if (status_code != "200") {
        next
      }

      # Check if it's a private residence - use local-name() to handle namespaces
      is_private_residence_node <- xml2::xml_find_first(
        response,
        "//*[local-name()='Resource']/*[local-name()='IsPrivateResidence']"
      )

      if (!is.na(is_private_residence_node)) {
        is_private_residence <- xml2::xml_text(is_private_residence_node)
        if (tolower(is_private_residence) == "true") {  # Changed to tolower for case insensitivity
          results[i] <- "Residential"
        } else {
          # Check for business type with improved XPath
          business_type_node <- xml2::xml_find_first(
            response,
            "//*[local-name()='BusinessesAtLocation']/*[local-name()='BusinessLocationEntity']/*[local-name()='BusinessInfo']/*[local-name()='Types']"
          )

          if (!is.na(business_type_node)) {
            business_type <- xml2::xml_text(business_type_node)
            # Truncate business type to 255 characters if necessary
            if (nchar(business_type) > 255) {
              business_type <- substr(business_type, 1, 255)
            }
            results[i] <- business_type
          } else {
            # If no business type is found but it's not residential, set a default
            results[i] <- "Unknown"
          }
        }
      }
    }, error = function(e) {
      # Output error message for debugging
      message("Error processing location ", i, ": ", e$message)
      # Leave as NA on error
    })
  }

  return(results)
}
