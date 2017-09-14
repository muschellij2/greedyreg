#' @title Greedy Registration with Affine
#'
#' @description Registration from Greedy library
#'
#' @param fixed Fixed image
#' @param moving Moving Image to register to fixed image
#' @param affine Should an affine registration be run before greedy?
#' @param dimension  Number of image dimension
#' @param args.affine list of arguments for affine registration and
#' \code{\link{greedy_affine}}
#' @param args.greedy list of arguments for greedy registration and
#' \code{\link{greedy_estimate}}
#' @param verbose print diagnostic messages
#' @param ... additional arguments passed to \code{\link{greedy_apply}},
#' like \code{interpolator}
#'
#' @return List of output from all registrations and application
#' @export
greedy = function(
  fixed,
  moving,
  affine = TRUE,
  dimension = 3,
  args.affine = list(
    niters = "200x200x200",
    omat = tempfile(fileext = ".mat"),
    metric = "NMI",
    dof = 12,
    opts = ""),
  args.greedy = list(
    initial_transform = args.affine$omat,
    niters = args.affine$niters,
    metric = args.affine$metric,
    warpfile = tempfile(fileext = ".nii.gz"),
    invwarpfile = tempfile(fileext = ".nii.gz"),
    opts = ""),
  verbose = TRUE,
  ...
) {

  if (affine) {
    args = args.affine
    args$verbose = verbose
    args$fixed = fixed
    args$moving = moving
    args$dimension = dimension

    affine_res = do.call("greedy_affine", args = args)
  } else {
    affine_res = NULL
  }

  args = args.greedy
  args$verbose = verbose
  args$fixed = fixed
  args$moving = moving
  args$dimension = dimension

  nonlin_res = do.call("greedy_estimate", args = args)

  # dumb way for now, but should work
  # all should be character for now
  transforms = list(nonlin_res$warpfile)
  if (affine) {
    transforms = list(nonlin_res$warpfile, affine_res$omat)
  }
  apply_res = greedy_apply(
    fixed = fixed,
    moving = moving,
    transforms = transforms,
    dimension = dimension,
    verbose = verbose,
    ...
  )

  apply_res$affine = affine_res
  apply_res$greedy = nonlin_res
  return(apply_res)
}