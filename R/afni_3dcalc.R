#' @title AFNI 3dcalc function
#' @description Wrapper for AFNI \code{3dcalc} function
#'
#' @param file nifti object or NIfTI filename.  If more than one is given,
#' they are given names of the letters up to \code{z}.
#' @param expression Calculation expression, with single quotes
#' @param outfile Output filename, should have a \code{.nii.gz}
#' extension
#' @param retimg Should an image be returned (\code{TRUE}) or a filename?
#' @param opts Additional options passed to \code{3dcalc}
#' @param ... additional arguments to \code{\link{afni_3dAFNItoNIFTI}}
#'
#' @return Output filename of the brik
#' @export
afni_3dcalc <- function(in_files, expression, out_file, extra_options = NULL) {
  # initialize list -----
  afni_list <- list(path = afnir::get_afni(),
                    program = "3dcalc",
                    options = NULL)
  
  # append options -----
  # set in_files
  for (i in 1:length(in_files)){
    afni_list[["options"]][[letters[i]]] <- in_files[i]
  }
  
  # set expression
  afni_list[["options"]][["expr"]] <- paste0("'", expression, "'")
  
  # set extra options
  if (!is.null(extra_options)) {
    afni_list[["options"]][["extra_options"]] <- extra_options
  }
  
  # set out_file
  afni_list[["options"]][["prefix"]] <- out_file
  
  # write out command -----
  # using for loop
  # if then for exceptions
  options_list = names(afni_list$options)
  afni_command <- paste0(afni_list$program, " \\\n")
  for (option in options_list) {
    if (option == "prefix") {
      afni_command <- paste0(afni_command, "-", option, " ", afni_list[['options']][["prefix"]])
    } else if (option == "extra_options") {
      afni_command <- paste0(afni_command, afni_list[['options']][["extra_options"]], " \\\n")
    } else {
      afni_command <- paste0(afni_command, "-", option, " ", afni_list[['options']][[option]], " \\\n")
    }
  }
  
  afni_list[["command"]] <-  paste0(afni_list$path, afni_command)
  afni_list <<- afni_list
  
  cat(paste0("Command:\n", afni_command))
  
  # combine afni path and command
  afni_command <- paste0(afni_list$command)
  
  # run in command-line
  cat("\n\nRunning:\n")
  system(afni_command)
  
}
