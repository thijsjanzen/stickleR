require(stringr)
require(dplyr)
require(ggplot2)
# needs right path ofc, modify to perform correctly:
begin <- "/Users/thijsjanzen/MEGAsync2/Jakob/REVIEW/raw_reads/"

f <- list.files(begin, pattern = "per_second.rds", recursive = TRUE)

group_data <- read_delim("/Users/thijsjanzen/MEGAsync2/Jakob/REVIEW/group_overview.csv")

plots <- list()
cnt <- 1
for (x in f) {
  cat(x, "\n")
  exper_name <- str_split(x, "/per_second.rds")[[1]][1]

  file_name <- paste0(begin, x)
  ff <- readRDS(file_name)

  a1 <- ff %>%
    filter(location < 6) %>%
    summarise("num_fish" = length(unique(fish)))
  a2 <- ff %>%
    filter(location >= 6) %>%
    summarise("num_fish" = length(unique(fish)))

  colors_used1 <- ggpubr::get_palette(palette = "Paired", k = a1$num_fish[[1]])
  colors_used1 <- rev(colors_used1[1:2]) # silly way of getting light and dark blue inverted
  colors_used2 <- ggpubr::get_palette(palette = "Paired", k = a2$num_fish[[1]])
  colors_used2 <- rev(colors_used2[1:2])

  all_dates <- sort(unique(ff$date))

  ff$type <- "Unknown"

  exper_number <- as.numeric(tail(str_split(exper_name, "_")[[1]], 1))

  focal_types <- subset(group_data, group_data$test_id == paste0("mixed", exper_number))
  Residents <- subset(focal_types, focal_types$origin == "Resident")
  Migrants <- subset(focal_types, focal_types$origin == "Migrant")
  ff$type[ff$fish %in% Residents$tag_id] <- "Resident"
  ff$type[ff$fish %in% Migrants$tag_id] <- "Migrant"


  p1 <- ff %>%
    filter(date == all_dates[1]) %>%
    filter(times %in% seq(0, 3600*24, by = 300)) %>%
    filter(location <= 5) %>%
    mutate("jitter_location" = jitter(location)) %>%
    #  mutate("jitter_location" = (location)) %>%
    ggplot(aes(x = (full_time), y = jitter_location, col = type, group = fish)) +
    geom_line() +
    geom_point() +
    scale_color_manual(values = colors_used1) +
    theme_classic() +
    coord_flip() +
    ylim(5.5, 0.5) +
    ylab("Pond") +
    xlab("Time in Experiment") +
    ggtitle(exper_name)

  p2 <- ff %>%
    filter(times %in% seq(0, 3600*24, by = 300)) %>%
    filter(location > 5) %>%
    mutate("jitter_location" = jitter(location)) %>%
    # mutate("jitter_location" = (location)) %>%
    ggplot(aes(x = (full_time), y = jitter_location, col = type, group = fish)) +
    geom_line() +
    geom_point() +
    scale_color_manual(values = colors_used2) +
    theme_classic() +
    coord_flip() +
    ylim(10.5, 5.5) +
    ylab("Pond") +
    xlab("Time in Experiment") +
    ggtitle(exper_name)

  p3 <- egg::ggarrange(p1, p2, nrow = 1)

  plots[[cnt]] <- p3
  cnt <- cnt + 1
}

pdf("all_trajectories_jitter_5min_resmigr.pdf", width = 16, height = 12)
for (i in 1:length(plots)) {
  print(plots[[i]])
}
dev.off()
