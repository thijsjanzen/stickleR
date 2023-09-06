#' function to infer per second the nearest location
#' @param dataset dataset having at least columns 'Date', 'Time', 'Unit number',
#' and 'Transponder code'
#' @return tibble
#' @export
#' @rawNamespace useDynLib(stickleR, .registration=TRUE)
#' @rawNamespace importFrom(Rcpp, evalCpp)
pick_closest <- function(dataset) {
  max_sec <- 3600 * 24
  data(site_map)

  times <- as.numeric(dataset$Time)

  res <- get_all_locations_cpp(max_sec,
                               times,
                               as.vector(dataset$`Unit number`),
                               as.matrix(site_map))

  output <- cbind(1:max_sec, res)

  colnames(output) <- c("times", "location")
  output <- tibble::as_tibble(output)
  return(output)
}
