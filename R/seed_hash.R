#' print seed
#'
#' @param prefix of choice
#'
#' @return text
#' @export
#'
#' @examples
#' cat(seed_hash)
#'
seed_hash <- function(prefix = "Hash of seed: ") {
  paste0(prefix, substr(digest::digest(.GlobalEnv$.Random.seed), 1, 6), "\n")
}
