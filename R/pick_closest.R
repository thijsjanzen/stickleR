#' function to infer per second the nearest location
#' @param dataset dataset having at least columns 'Date', 'Time', 'Unit number',
#' and 'Transponder code'
#' @param prev_day_location location of fish at previous day (if known)
#' @param used_site_map site map, result of load(site_map) or load(review_map)
#' @param start_time starting time of recording
#' @param end_time ending time of recording
#' @return tibble
#' @export
#' @rawNamespace useDynLib(stickleR, .registration=TRUE)
#' @rawNamespace importFrom(Rcpp, evalCpp)
pick_closest <- function(dataset,
                         prev_day_location = NA,
                         used_site_map,
                         start_time,
                         end_time) {

  if (is.na(prev_day_location)) prev_day_location <- -1

  times <- as.numeric(dataset$Time)


  res <- get_all_locations_cpp(as.numeric(start_time),
                               as.numeric(end_time),
                               times[order(times)],
                               as.vector(dataset$`Unit number`)[order(times)],
                               used_site_map,
                               prev_day_location)

  output <- cbind(start_time:end_time, res)

  colnames(output) <- c("times", "location")
  output <- tibble::as_tibble(output)
  output$location[output$location < 0] <- NA

  return(output)
}
