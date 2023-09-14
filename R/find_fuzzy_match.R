#' function to find an approximate match to a fish name
#' @param focal_fish focal fish tag
#' @param fish_names list with 'true' fish tags
#' @return either the approximate match, or it's own name in case no match is
#' found
#' @export
find_fuzzy_match <- function(focal_fish, fish_names) {

  fuz_match <- function(y) {
    return(adist(focal_fish, y))
  }

  ax <- sapply(fish_names, fuz_match)
  ay <- which(ax < 4)
  if (length(ay) == 1) {
   return(names(ay)[[1]])
  } else {
    return(focal_fish)
  }
}
