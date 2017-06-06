#' @title Affine Registration from Greedy
#' @description Affine Registration from Greedy library
#'
#' @param fixed Fixed image
#' @param moving Moving Image to register to fixed image
#' @param dimension  Number of image dimension
#' @param omat Output affine matrix
#' @param intial_affine Initalization affine -either filename or identity
#' @param dof degrees of freedom
#' @param metric Metric to use for registraation
#' @param opts additional options
#' @param niters number of iterations per level of multi-res
#' @param verbose print diagnostic messages
#'
#' @return List of output
#' @export
#'
#' @importFrom neurobase checkimg
greedy_affine = function(fixed,
                         moving,
                         # output = tempfile(fileext = ".nii.gz"),
                         niters = "100x50x10",
                         dimension = 3,
                         omat = tempfile(fileext = ".mat"),
                         intial_affine = "identity",
                         dof = 12,
                         metric = "NCC 2x2x2",
                         opts = "",
                         verbose = TRUE
) {
  if (intial_affine == "identity") {
    ia = "-ia-identity"
  } else {
    ia = paste0("-ia ", intial_affine)
  }
  dof = paste0("-dof ", dof)
  metric = paste0("-m ", metric)
  fixed = neurobase::checkimg(fixed)
  moving = neurobase::checkimg(moving)

  imgs = paste("-i", fixed, moving)
  niters = paste0("-n ", niters)
  # output = paste0("-o ", output)
  dimension = paste("-d", dimension)
  xomat = omat
  omat = paste("-o", omat)

  opts = c("-a",
           imgs,
           niters,
           opts,
           dimension,
           omat,
           ia,
           dof,
           metric)

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
  L = list(omat = xomat, result = res)
  return(L)
}