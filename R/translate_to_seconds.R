#' @keywords internal
get_location <- function(focal_sec, paired_reads) {
  focal_loc <- NA
  if (focal_sec > max(paired_reads$time1)) {
    focal_loc <- tail(paired_reads$new_location, 1)
  } else if (focal_sec < min(paired_reads$time1)) {
    focal_loc <- paired_reads$old_location[1]
  } else {
    aa <- min(which(paired_reads$time1 >= focal_sec))
    focal_loc <- paired_reads$old_location[aa]
  }

  return(focal_loc)
}

#' function to calculate location at all times during a single day
#' @param paired_reads paired reads output
#' @return tibble with two columns: time and location
#' @export
extract_location_at_all_seconds <- function(paired_reads) {
  max_sec <- 3600*24

  res <- sapply(1:max_sec, get_location, paired_reads)
  output <- cbind(1:max_sec, res)
  colnames(output) <- c("times", "location")
  output <- tibble::as_tibble(output)
  return(output)
}



#' sometimes, fish don't move, so we have to infer where they are from
#' edge reads
#' @param dataset dataset
#' @return tibble with two columns: time and location
#' @export
reconstruct_location <- function(dataset) {
  unique_reads <- unique(dataset$`Unit number`)
  data(site_map)
  get_site <- function(read, site_map) {
    a <- subset(site_map, site_map$transponder == read)
    return(a$location)
  }
  vv <- sapply(unique_reads, get_site, site_map)
  focal_site <- unique(vv)
  seconds <- 1:(3600*24)
  output <- cbind(seconds, rep(focal_site, length(seconds)))
  colnames(output) <- c("times", "location")
  output <- tibble::as_tibble(output)
  return(output)
}
