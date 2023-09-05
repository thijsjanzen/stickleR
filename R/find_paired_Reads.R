#' @keywords internal
is_in_location_map <- function(location_map, loc1, loc2) {
  loc_map_set <- subset(location_map, location_map$first == loc1 &
                          location_map$second == loc2)
  if (length(loc_map_set$first) > 0) {
    return(TRUE)
  }
  return(FALSE)
}

#' function to extract paired reads that signify a change in container
#' @param dataset dataset
#' @return tibble with extracted information
#' @export
find_paired_reads <- function(dataset) {
  data(location_map)
  location_map <- tibble::as_tibble(location_map)
  paired_reads <- c()
  for (i in 2:length(dataset$Time)) {
    loc1 <- dataset$`Unit number`[[i - 1]]
    loc2 <- dataset$`Unit number`[[i]]
    if (is_in_location_map(location_map, loc1, loc2)) {
      diff_time <- dataset$Time[[i]] - dataset$Time[[i - 1]]
      find_pos <- infer_location(loc1, loc2)

      to_add <- c(loc1, loc2, dataset$Time[[i - 1]], dataset$Time[[i]], diff_time,
                  find_pos$old_location, find_pos$new_location,
                  dataset$Date[[i]])
      paired_reads <- rbind(paired_reads, to_add)
    } else {
      data(site_map)
      get_site <- function(read, site_map) {
        a <- subset(site_map, site_map$transponder == read)
        return(a$location)
      }
      p1 <- get_site(loc1, site_map)
      p2 <- get_site(loc2, site_map)

    }
  }
  if (is.null(paired_reads)) {
    paired_reads <- rbind(rep(NA, 8), rep(NA, 8))
  }
  colnames(paired_reads) <- c("read1", "read2", "time1", "time2",
                              "difftime", "old_location", "new_location", "date")
  paired_reads <- tibble::as_tibble(paired_reads)
  paired_reads <- paired_reads %>%
      dplyr::mutate_at(1:7, as.numeric)
  return(paired_reads)
}
