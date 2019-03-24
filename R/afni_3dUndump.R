#' @title AFNI 3dUndump function
#' @description Wrapper for AFNI \code{3dUndump} function
#' @param in_file_coord Input coordinate file
#' @param in_file_ref Input file that coordinate file will reference for coordinate space
#' @param in_file_mask Input file that will mask output file. specifically, sphere regions inside the mask will be set to 1 while sphere regions outside the mask will be set to 0
#' @param sphere_radius Radius of sphere in millimeteres
#' @param opts Additional options to passed into function
#' @param out_file Output file 
#' @param verbose FALSE (default). If true, will print afni path and command
#'
#' @return Returns output of command execution
#' @export
#'
afni_3dUndump <- function(in_file_coord,
                          in_file_ref = NULL,
                          in_file_mask = NULL,
                          sphere_radius = NULL,
                          opts = NULL,
                          out_file = NULL,
                          verbose = FALSE) {
  afni_path <- get_afni()

  afni_func <- "3dUndump"
  afni_cmd <- afni_func

  if (!is.null(in_file_ref)) {
    afni_master <- paste("-master", in_file_ref)
    afni_cmd <- paste(afni_cmd, afni_master)
  }

  if (!is.null(in_file_mask)) {
    afni_mask <- paste("-mask", in_file_mask)
    afni_cmd <- paste(afni_cmd, afni_master)
  }

  if (!is.null(sphere_radius)) {
    afni_srad <- paste("-srad", sphere_radius)
    afni_cmd <- paste(afni_cmd, afni_srad)
  }

  if (!is.null(opts)) {
    afni_opts <- opts
    afni_cmd <- paste(afni_cmd, opts)
  }

  if (!is.null(out_file)) {
    afni_prefix <- paste("-prefix", out_file)
    afni_cmd <- paste(afni_cmd, afni_prefix)
  }

  afni_cmd <- paste(afni_cmd, in_file_coord)

  if (verbose == TRUE) {
    cat(paste("\nAFNI Path:\n", afni_path, "\n\nAFNI Command:\n", afni_cmd, "\n\nAFNI Output:\n"))
  }

  system(paste0(afni_path, afni_cmd))
}