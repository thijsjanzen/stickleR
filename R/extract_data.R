#' function to convert read data into per second score
#' @param dataset data set
#' @param location_map map to transcode transponders to ponds
#' @param fish_id_list list of fish id's that need to be included
#' @export
extract_data <- function(dataset,
                         location_map,
                         fish_id_list = NULL,
                         start_time,
                         end_time) {

  if (is.null(fish_id_list)) fish_id_list <- unique(dataset$`Transponder code`)

  pb <- txtProgressBar(max = length(fish_id_list$tag_id), style = 3)
  cnt <- 1

  all_dates <- unique(dataset$Date)
  all_dates2 <- lubridate::dmy(all_dates)
  all_dates <- all_dates[order(all_dates2)]

  all_results <- c()

  starting_times <- c(lubridate::local_time(start_time)[[1]], 0)
  ending_times   <- c(3600*24, lubridate::local_time(end_time)[[1]])


  for (focal_fish in fish_id_list$tag_id) {
    exper <- fish_id_list$Experiment[which(fish_id_list$tag_id == focal_fish)]
    starting_location <- 5
    if (exper == "B") starting_location <- 10

    cat(focal_fish, exper, starting_location, "\n")

    fish_results <- c()
    for (i in 1:length(all_dates)) {
      focal_date <- all_dates[i]
      day_subset <- subset(dataset, dataset$Date == focal_date)

      date_start <- starting_times[i]
      date_end   <- ending_times[i]

      focal_data <- subset(day_subset, day_subset$`Transponder code` == focal_fish)
      prev_day_location <- starting_location
      if (i == 2) {
        prev_day_dataset <- subset(fish_results, fish_results$date == all_dates[i - 1])
        prev_day_fish <- subset(prev_day_dataset, prev_day_dataset$fish == focal_fish)
        prev_day_location <- tail(prev_day_fish$location, 1)
      }

      if (length(focal_data$Date) < 1) {
        # if the dataset is empty, we use previously known information.
        fake_times <- date_start:date_end
        # look up location at previous day:
        if (is.null(prev_day_location)) prev_day_location <- starting_location
        all_seconds <- cbind(fake_times, prev_day_location)
        colnames(all_seconds) <- c("times", "location")
        all_seconds <- tibble::as_tibble(all_seconds)
      } else {

        all_seconds <- pick_closest(focal_data,
                                    prev_day_location,
                                    as.matrix(location_map),
                                    date_start,
                                    date_end)
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
