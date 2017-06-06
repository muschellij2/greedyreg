#' @title Find Greedy Executable
#' @description Returns a path for the greedy executable
#'
#' @return Character vector of path
#' @export
greedy_cmd = function() {
  system.file("bin", "greedy", package = "greedyreg")
}