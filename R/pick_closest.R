#' function to infer per second the nearest location
#' @param dataset dataset having at least columns 'Date', 'Time', 'Unit number',
#' and 'Transponder code'
#' @param prev_day_location location of fish at previous day (if known)
#' @param used_site_map site map, result of load(site_map) or load(review_map)
#' @param extrapolate_until_midnight should the data extrapolate until midnight?
#' @return tibble
#' @export
#' @rawNamespace useDynLib(stickleR, .registration=TRUE)
#' @rawNamespace importFrom(Rcpp, evalCpp)
pick_closest <- function(dataset,
                         prev_day_location = NA,
                         used_site_map,
                         extrapolate_until_midnight = TRUE) {
  max_sec <- 3600 * 24
  if (is.null(prev_day_location)) prev_day_location <- -1

  times <- as.numeric(dataset$Time)

  #if (!extrapolate_until_midnight) { # review data
  #  max_sec <- max(times)
  #  if (max_sec > 80000) max_sec <- 3600 * 24
  #}

  res <- get_all_locations_cpp(max_sec,
                               times[order(times)],
                               as.vector(dataset$`Unit number`)[order(times)],
                               used_site_map,
                               prev_day_location)

  output <- cbind(1:max_sec, res)

  colnames(output) <- c("times", "location")
  output <- tibble::as_tibble(output)
  output$location[output$location < 0] <- NA

  return(output)
}
