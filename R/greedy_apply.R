#' @title Apply Transformations from Greedy
#' @description Apply Registration from Greedy library
#'
#' @param fixed Fixed image
#' @param moving Moving Image(s) to apply transformations
#' @param transforms List of transformations
#' @param moving_outfile List of output filenames corresponding to the moving
#' files
#' @param interpolator interpolation operation to use (NN, LINEAR*, LABEL sigma)
#' @param warpfile Output composed warp field filename
#' @param jacobian Output Jacobian determinant image
#' @param dimension Number of image dimension
#' @param opts additional options
#' @param verbose print diagnostic messages
#'
#' @return List of output warps and inverses
#' @export
#'
#' @importFrom neurobase checkimg
greedy_apply = function(
  fixed,
  moving,
  transforms = list(),
  moving_outfile = NULL,
  interpolator = "NN",
  warpfile = tempfile(fileext = ".nii.gz"),
  jacobian = tempfile(fileext = ".nii.gz"),
  dimension = 3,
  opts = "",
  verbose = TRUE
) {
  fixed = neurobase::checkimg(fixed)
  fixed = paste("-rf", fixed)
  moving = neurobase::checkimg(moving)


  transforms = neurobase::checkimg(transforms)
  transforms = paste(transforms, collapse = " ")
  transforms = paste("-r", transforms)

  interpolator = paste("-ri", interpolator)

  if (is.null(moving_outfile)) {
    moving_outfile = sapply(
      seq(length(moving)),
      tempfile,
      fileext = ".nii.gz")
    names(moving_outfile) = moving
  }
  stopifnot(length(moving) == length(moving_outfile))

  moving = mapply(function(x, y) {
    paste("-rm", x, y)
  }, moving, moving_outfile, SIMPLIFY = TRUE)

  xwarpfile = warpfile
  if (!is.null(warpfile)) {
    warpfile = paste("-rc", warpfile)
  }
  xjacobian = jacobian
  if (!is.null(jacobian)) {
    jacobian = paste("-rj", jacobian)
  }

  dimension = paste("-d", dimension)

  # can have more than one
  opts = c(fixed,
           transforms,
           interpolator,
           moving,
           warpfile,
           jacobian,
           dimension,
           opts)

  opts = opts[ opts != "" ]
  opts = paste(opts, collapse = " ")

  cmd = greedy_cmd()
  cmd = paste(cmd, opts)
  if (verbose) {
    message(cmd)
  }
  res = system(cmd)
  if (res != 0) {
    warning(paste0("Result does not indicate success ",
                   "- function may not work as expected!"))
  }
  L = list( outfiles = moving_outfile,
           result = res)
  L$warpfile = xwarpfile
  L$jacobian = xjacobian
  return(L)
}