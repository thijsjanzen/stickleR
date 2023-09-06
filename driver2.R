require(stickleR)

vx <- readr::read_delim("/Users/thijsjanzen/Dropbox/projects/Jakob/stick2/20220513.CSV",
                        delim = ";")
vx <- dplyr::select(vx, c('Date', 'Time', 'Unit number', 'Transponder code'))

all_results <- c()
for (focal_fish in unique(vx$`Transponder code`)) {
  focal_data <- subset(vx, vx$`Transponder code` == focal_fish)

  # and now, we do not remove paired reads!
  all_seconds <- pick_closest(focal_data)
  all_seconds$fish = focal_fish
  all_results <- rbind(all_results, all_seconds)
  cat(focal_fish, "\n")
}

all_results2 <- subset(all_results, all_results$times > 10000 & all_results$times < 11000 )

inter_mat <- stickleR::create_interaction_matrix(all_results2)

inter_mat[1, 1]
