#' @title Deformation Registration from Greedy
#' @description Deformation Registration from Greedy library
#'
#' @param fixed Fixed image
#' @param moving Moving Image to register to fixed image
#' @param dimension  Number of image dimension
#' @param warpfile Output warp field filename
#' @param invwarpfile Inverse of the warp field filename
#' @param initial_transform Mat file of affine intialization
#' @param metric Metric to use for registraation
#' @param opts additional options
#' @param niters number of iterations per level of multi-res
#' @param verbose print diagnostic messages
#'
#' @return List of output warps and inverses
#' @export
#'
#' @importFrom neurobase checkimg
greedy_estimate = function(fixed,
                  moving,
                  initial_transform = NULL,
                  warpfile = tempfile(fileext = ".nii.gz"),
                  invwarpfile = tempfile(fileext = ".nii.gz"),
                  niters = "100x50x10",
                  dimension = 3,
                  metric = "NCC 2x2x2",
                  opts = "",
                  verbose = TRUE
) {
  fixed = neurobase::checkimg(fixed)
  moving = neurobase::checkimg(moving)
  imgs = paste("-i", fixed, moving)

  if (!is.null(initial_transform)) {
    initial_transform = paste(initial_transform, collapse = " ")
    initial_transform = paste("-it", initial_transform)
  } else {
    warning("No affine initalization was set, use with caution!")
  }
  xinitial_transform = initial_transform

  xwarpfile = warpfile
  xinvwarpfile = invwarpfile
  warpfile = paste0("-o ", warpfile)
  invwarpfile = paste0("-oinv", invwarpfile)

  niters = paste0("-n ", niters)
  dimension = paste("-d", dimension)
  metric = paste0("-m ", metric)

  # can have more than one


  opts = c(imgs,
           initial_transform,
           warpfile,
           invwarpfile,
           niters,
           dimension,
           metric,
           opts)

  opts = opts[ opts != "" ]
  opts = paste(opts, collapse = " ")

  cmd = greedy_cmd()
  cmd = paste(cmd, opts)
  res = system(cmd)
  if (res != 0) {
    warning(paste0("Result does not indicate success ",
                   "- function may not work as expected!"))
  }
  L = list(warpfile = xwarpfile,
           invwarpfile = xinvwarpfile,
           result = res)
  L$initial_transform = xinitial_transform
  return(L)
}