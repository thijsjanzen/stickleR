#' function to convert read data into per second score
#' @param dataset data set
#' @param location_map map to transcode transponders to ponds
#' @param review_data flag to indicate what experiment we are transcoding
#' @export
extract_data <- function(dataset, location_map, review_data = FALSE) {

  pb <- txtProgressBar(max = length(unique(dataset$`Transponder code`)), style = 3)
  cnt <- 1

  all_dates <- unique(dataset$Date)
  all_dates2 <- lubridate::dmy(all_dates)
  all_dates <- all_dates[order(all_dates2)]

  all_results <- c()

  for (focal_fish in unique(dataset$`Transponder code`)) {
    fish_results <- c()
    for (i in 1:length(all_dates)) {
      focal_date <- all_dates[i]
      day_subset <- subset(dataset, dataset$Date == focal_date)

      focal_data <- subset(day_subset, day_subset$`Transponder code` == focal_fish)
      prev_day_location <- NA
      if (i > 1) {
        prev_day_dataset <- subset(fish_results, fish_results$date == all_dates[i - 1])
        prev_day_fish <- subset(prev_day_dataset, prev_day_dataset$fish == focal_fish)
        prev_day_location <- tail(prev_day_fish$location, 1)
      }

      if (length(focal_data$Date) < 1) {
        # TODO WHAT IF FOCAL_DATA IS EMPTY: GET location from previous day if available.
        fake_times <- 1:(3600 * 24)
        # look up location at previous day:
        if (is.null(prev_day_location)) prev_day_location <- NA
        all_seconds <- cbind(fake_times, prev_day_location)
        colnames(all_seconds) <- c("times", "location")
        all_seconds <- tibble::as_tibble(all_seconds)
      } else {
        # this flag indicates that the data should be extrapolated until
        # midnight
        # normally, this is TRUE.
     #   midnight <- TRUE
     #   if (review_data && i == 2) midnight <- FALSE


        all_seconds <- pick_closest(focal_data,
                                    prev_day_location,
                                    as.matrix(location_map))
      }
      all_seconds$fish <- focal_fish
      all_seconds$date <- focal_date
      fish_results <- rbind(fish_results, all_seconds)
    }

    all_results <- rbind(all_results, fish_results)
    cnt <- cnt + 1
    setTxtProgressBar(pb, cnt)
  }

  return(all_results)
}
