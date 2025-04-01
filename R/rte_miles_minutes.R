#' Calculate route distance and travel time using Bing Maps API
#'
#' @param o_lng Numeric vector of origin longitudes
#' @param o_lat Numeric vector of origin latitudes
#' @param d_lng Numeric vector of destination longitudes
#' @param d_lat Numeric vector of destination latitudes
#' @param tmode Character vector of travel modes (e.g., "Driving", "Walking", "Transit")
#' @param key_b Character string with Bing Maps API key
#'
#' @return A character vector with comma-separated distance (miles) and travel time (minutes),
#'         or NA for failed requests
#' @importFrom xml2 read_xml xml_find_first xml_text
#' @export
rte_miles_minutes <- function(o_lng, o_lat, d_lng, d_lat, tmode="Driving", key_b) {
  # Ensure inputs are vectors of the same length
  n <- max(length(o_lng), length(o_lat), length(d_lng), length(d_lat), length(tmode))
  o_lng <- rep_len(o_lng, n)
  o_lat <- rep_len(o_lat, n)
  d_lng <- rep_len(d_lng, n)
  d_lat <- rep_len(d_lat, n)
  tmode <- rep_len(tmode, n)

  # Initialize results vector
  results <- rep(NA_character_, n)

  # Process each route request
  for (i in seq_len(n)) {
    tryCatch({
      # Check for NA inputs
      if (is.na(o_lng[i]) || is.na(o_lat[i]) || is.na(d_lng[i]) || is.na(d_lat[i]) || is.na(tmode[i])) {
        next
      }

      # Format URL
      url_template <- "http://dev.virtualearth.net/REST/V1/Routes/%s?wp.0=%s,%s&wp.1=%s,%s&distanceUnit=mi&optmz=distance&output=xml&key=%s"
      url <- sprintf(
        url_template,
        trimws(tmode[i]),
        trimws(o_lat[i]),
        trimws(o_lng[i]),
        trimws(d_lat[i]),
        trimws(d_lng[i]),
        trimws(key_b)
      )

      # Send request
      response <- xml2::read_xml(url)

      # Define the namespace
      ns <- xml2::xml_ns_rename(xml2::xml_ns(response), d1 = "ns")

      # Check status code with namespace
      status_code_node <- xml2::xml_find_first(response, "//ns:StatusCode", ns = ns)
      if (is.na(status_code_node)) {
        next
      }

      status_code <- xml2::xml_text(status_code_node)
      if (status_code != "200") {
        next
      }

      # Extract travel distance and duration with namespace
      travel_distance_node <- xml2::xml_find_first(response, "//ns:ResourceSets/ns:ResourceSet/ns:Resources/ns:Route/ns:TravelDistance", ns = ns)
      travel_duration_node <- xml2::xml_find_first(response, "//ns:ResourceSets/ns:ResourceSet/ns:Resources/ns:Route/ns:TravelDuration", ns = ns)

      if (is.na(travel_distance_node) || is.na(travel_duration_node)) {
        next
      }

      travel_miles <- xml2::xml_text(travel_distance_node)
      travel_seconds <- xml2::xml_text(travel_duration_node)

      # Convert seconds to minutes
      travel_minutes <- as.numeric(travel_seconds) / 60.0

      # Format result string
      results[i] <- paste(travel_miles, travel_minutes, sep = ",")

    }, error = function(e) {
      # Leave as NA on error
    })
  }

  return(results)
}
