#' function to infer per second the nearest location
#' @param dataset dataset having at least columns 'Date', 'Time', 'Unit number',
#' and 'Transponder code'
#' @return tibble
#' @export
#' @rawNamespace useDynLib(stickleR, .registration=TRUE)
#' @rawNamespace importFrom(Rcpp, evalCpp)
pick_closest <- function(dataset,
                         prev_day_location) {
  max_sec <- 3600 * 24
  data(site_map)
  if (is.null(prev_day_location)) prev_day_location <- -1

  times <- as.numeric(dataset$Time)

  res <- get_all_locations_cpp(max_sec,
                               times[order(times)],
                               as.vector(dataset$`Unit number`)[order(times)],
                               as.matrix(site_map),
                               prev_day_location)

  output <- cbind(1:max_sec, res)

  colnames(output) <- c("times", "location")
  output <- tibble::as_tibble(output)
  output$location[output$location < 0] <- NA
  return(output)
}
