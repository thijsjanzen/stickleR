#' function to infer the movement based on antennae reads
#' @param first_read first antennae recording
#' @param second_read matching second antennae recording
#' @return list with 'old_location' and 'new_location', indicating where the
#' fish came from and where it moved to
#' @export
infer_location <- function(first_read, second_read) {
  data(location_map)
  location_map <- tibble::as_tibble(location_map)

  focal_loc <- subset(location_map,
                      location_map$first == first_read &
                      location_map$second == second_read)

  if (length(focal_loc$`old location`) > 0) {
    return(list("old_location" = focal_loc$`old location`[[1]],
                "new_location" = focal_loc$`new location`[[1]]))
  } else {
    return(NA)
  }
}
