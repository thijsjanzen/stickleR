#' function remove reads of a fish moving in front of an entrance antenna
#' @param dataset dataset tibble
#' @return filtered dataset
#' @description
#' function to remove duplicate entries
#'
#' @export
remove_duplicate_reads <- function(dataset) {
  ax <- which(diff(dataset$`Unit number`) == 0)
  dataset2 <- dataset[-ax, ]
  return(dataset2)
}
