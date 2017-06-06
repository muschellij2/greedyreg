#' @title Affine Registration from Greedy
#' @description Affine Registration from Greedy library
#'
#' @param fixed Fixed image
#' @param moving Moving Image to register to fixed image
#' @param dimension  Number of image dimension
#' @param omat Output affine matrix
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
# @param initial_affine Initalization affine -either filename or identity
greedy_affine = function(fixed,
                         moving,
                         # output = tempfile(fileext = ".nii.gz"),
                         niters = "200x200x200",
                         dimension = 3,
                         omat = tempfile(fileext = ".mat"),
                         # initial_affine = "identity",
                         dof = 12,
                         metric = "NMI",
                         opts = "",
                         verbose = TRUE
) {
  # if (initial_affine == "identity") {
  #   ia = "-ia-identity"
  # } else {
  #   ia = paste0("-ia ", initial_affine)
  # }
  dof = paste0("-dof ", dof)
  xmetric = metric
  metric = paste0("-m ", metric)
  fixed = neurobase::checkimg(fixed)
  moving = neurobase::checkimg(moving)

  imgs = paste("-i", fixed, moving)
  xniters = niters
  niters = paste0("-n ", niters)
  # output = paste0("-o ", output)
  xdimension = dimension
  dimension = paste("-d", dimension)
  xomat = omat
  omat = paste("-o", omat)

  xopts = opts
  opts = c("-a",
           imgs,
           niters,
           opts,
           dimension,
           omat,
           # ia,
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
  L = list(
    fixed = fixed,
    moving = moving,
    omat = xomat,
           result = res,
           metric = xmetric,
           niters = xniters,
           dimension = xdimension,
           additional_options = xopts)
  return(L)
}