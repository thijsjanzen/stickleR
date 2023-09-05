#' function to filter out breeding spot antennae
#' @param dataset input dataset
#' @param return filtered dataset
#' @description
#' This function filters out recordings made at the breeding spot antennae,
#' being: 30, 40, 50, 60, 70, 80, 90, 100, but also:
#' 10, 15, 25, 45.
#' Hence, this filters out all reads done by antennae that are not placed at
#' corridors.
#' @export
filter_breeding_spot <- function(dataset) {
  breeding_spots <- c(30,  40, 50,  60,  70,  80,  90, 100,
                      10, 15, 25, 45)

  dataset2 <- subset(dataset, !(dataset$`Unit number` %in% breeding_spots))

  return(dataset2)
}
