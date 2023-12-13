get_fish_info <- function(group_info, system_info, experiment_number) {


  test_id_label <- paste0("mixed", experiment_number)
  group_info <- subset(group_info, group_info$test_id == test_id_label)

  output <- cbind(group_info$tag_id, group_info$test_id)
  colnames(output) <- c("tag_id", "Experiment")
  output <- as.data.frame(output)

  for (i in 1:length(output$tag_id)) {
    experiment_group <- group_info$group_id[group_info$tag_id == output$tag_id[i]]
    experiment_label <- system_info$pond_system[system_info$group == experiment_group]
    output$Experiment[i] <- experiment_label
  }
  return(output)
}



require(tidyverse)

# modify this path
begin <- "/Users/thijsjanzen/MEGAsync2/Jakob/REVIEW/raw_reads"
dd <- list.dirs(path = begin, recursive = TRUE)
dd

# modify this path
time_data <- read_delim("/Users/thijsjanzen/MEGAsync2/Jakob/REVIEW/starting_times.csv",
                        delim = ";")

group_data <- read_delim("/Users/thijsjanzen/MEGAsync2/Jakob/REVIEW/group_overview.csv")
group_data <- group_data %>%
  select(test_id, group_id, tag_id)

system_data <- read_tsv("/Users/thijsjanzen/MEGAsync2/Jakob/REVIEW/system_overview.txt")

require(stickleR)
data(review_map)

for (f in dd) {
  if (f != begin) {
    cat(f, "\n")
    files <- list.files(path = f, recursive = TRUE, pattern = "*.CSV")

    splt1 <- str_split(f, "_")
    no2 <- tail(splt1[[1]], 1)
    focal_number <- as.numeric(no2)



    focal_data <- c()
    for (x in files) {
      vx <- read_delim(paste0(f,"/", x), delim = ";", show_col_types = FALSE)
      focal_data <- rbind(focal_data, vx)
    }

    fish_id_subset <- get_fish_info(group_data, system_data, focal_number)


    index <- which(time_data$X == focal_number)
    focal_start_time <- time_data$Start_time_pond[index]
    focal_start_time <- lubridate::ymd_hms(focal_start_time)
    focal_end_time <- time_data$End_time_pond[index]
    focal_end_time <- lubridate::ymd_hms(focal_end_time)

    focal_data$complete_time <- lubridate::dmy_hms(paste(focal_data$Date, focal_data$Time))

    focal_data <- focal_data %>%
      filter(complete_time >= focal_start_time &
               complete_time <=  focal_end_time)


    # now that we have our focal dataset, we can interpolate locations for
    # every second:
    transcribed_data <- stickleR::extract_data(dataset = focal_data,
                                               location_map = as.matrix(review_map),
                                               fish_id_list = fish_id_subset)

    if (1 == 2) {
      all_dates <- unique(transcribed_data$date)
      transcribed_data %>%
        filter(date == all_dates[1]) %>%
        filter(times < 60000) %>%
        ggplot(aes(x = times ,y = location, col = fish)) +
        geom_line()
    }

    # remove NA entries:
    transcribed_data <- transcribed_data %>% filter(!is.na(location))

    # the 'times' column indicates the number of seconds counting since the start
    # of the day (this was useful in the other analysis).
    # To re-aquire the full time, we recalculate this with some lubridate magic:
    transcribed_data$full_time <- lubridate::dmy_hms(paste(transcribed_data$date, " 00:00:00 UTC"))
    transcribed_data$full_time <- transcribed_data$full_time + transcribed_data$times

    transcribed_data$experiment <- 1
    transcribed_data$experiment[transcribed_data$location > 5] <- 2

    index <- which(time_data$X == focal_number)
    focal_start_time <- time_data$Start_time_pond[index]
    focal_start_time <- lubridate::ymd_hms(focal_start_time)
    focal_end_time <- time_data$End_time_pond[index]
    focal_end_time <- lubridate::ymd_hms(focal_end_time)

    if (1 == 2) {
      focal_dates <- sort(unique(transcribed_data$date))

      transcribed_data %>%
        filter(date == focal_dates[1]) %>%
        filter(times < 70000) %>%
        filter(experiment == 1) %>%
        ggplot(aes(x = times, y = location, col = fish)) +
        geom_line()

      transcribed_data %>%
        filter(date == focal_dates[1]) %>%
        #filter(times < 70000) %>%
        filter(times %in% seq(0, 3600*24, by = 1)) %>%
        filter(experiment == 1) %>%
        mutate("jitter_location" = jitter(location)) %>%
        ggplot(aes(x = (full_time), y = jitter_location, col = fish, group = fish)) +
        geom_line() +
        geom_point() +
        #scale_color_manual(values = colors_used1) +
        theme_classic() +
        coord_flip() +
        ylim(5.5, 0.5) +
        ylab("Pond") +
        xlab("Time in Experiment") +
        ggtitle(exper_name)


      transcribed_data %>%
        filter(date == focal_dates[1]) %>%
        #filter(times < 70000) %>%
        filter(times %in% seq(0, 3600*24, by = 1)) %>%
        filter(experiment == 1) %>%
        filter(fish == "0007A57907") %>%
        ggplot(aes(x = full_time, y = jitter(location),col = fish)) +
        geom_line()
    }

    # this part is redundant now, but just for safekeeping:
    transcribed_data <- transcribed_data %>%
      filter(full_time >= focal_start_time &
               full_time <=  focal_end_time)





    out_file <- paste0(f, "/per_second.rds")
    saveRDS(transcribed_data, file = out_file)
  }
}
