#' wrapper function to use RcppArmadillo to create interaction matrix
#' @param dataset dataset
#' @return interaction matrix
#' @export
create_interaction_matrix <- function(dataset) {
  uniq_fish <- unique(dataset$fish)
  dataset$fish_no <- 0
  for (i in 1:length(uniq_fish)) {
    dataset$fish_no[dataset$fish == uniq_fish[i]] <- i - 1 # minus one for C++
  }
  inter_mat <- make_matrix_class(as.vector(dataset$times),
                           as.vector(dataset$location),
                           as.vector(dataset$fish_no),
                           length(uniq_fish))

  colnames(inter_mat) <- uniq_fish
  rownames(inter_mat) <- uniq_fish

  return(inter_mat)
}
