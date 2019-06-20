#' @title AFNI 3dLME function
#' @description Wrapper for AFNI \code{3dLME} function by Ekarin Pongpipat
#'
#' 3dLME is a AFNI Group Analysis Program with Linear Mixed-Effects Modeling
#' Approach created by Gang Chen.
#' https://afni.nimh.nih.gov/pub/dist/doc/program_help/3dLME.html
#'
#' @param in_data_table dataframe with columns Subj, InputFile, and any other fixed (between) or random (within) variables
#' @param in_glt_table dataframe with columns n_glt, glt_label, and glt_code
#' @param in_glf_table dataframe with columns n_glf, glt_labef, and glf_code
#' @param out_file filename/path of output file
#' @param model predictors to model
#' @param quant_vars specify quantitative variables
#' @param rand_eff random effects to model
#' @param cor_str autocorrelation setting (default: AR1)
#' @param ss_type sums of squares type (default: 3)
#' @param jobs number of cores (default: all)
#' @param extra_options extra not common options
#'
#' @return out_file and afni_list, which contains the afni path, program, options, and command
#'
#' @examples
#' in_glt_table <- read_csv("in_glt_table.csv")
#' in_glf_table <- read_csv("in_glf_table.csv")
#' in_data_table <- read_csv("in_data_table.csv")
#' afni_3dLME(in_data_table = in_data_table, in_glf_table = in_glf_table, out_file = "output.nii.gz", model = "cond*RT+age", quant_vars = "RT,age", rand_eff = "~1+RT")
#'
#' @export
afni_3dLME <- function(in_data_table, in_glt_table = NULL, in_glf_table = NULL, out_file, model, rand_eff = NULL, quant_vars = NULL, ss_type = 3, jobs = "all", extra_options = NULL, run_cmd = TRUE) {

  # load packages -----
  packages <- c("tidyverse", "glue", "nlme", "afnir", "lme4", "phia", "snow")
  xfun::pkg_attach(packages, message = F, install = T)

  # initialize list -----
  afni_list <- list(path = afnir::get_afni(),
                    program = "3dLME",
                    options = NULL)

  # append options -----
  # set out_file
  afni_list[["options"]][["prefix"]] <- out_file

  # set jobs
  if (jobs == "all") {
    afni_list[["options"]][["jobs"]] <- parallel::detectCores()
  } else {
    afni_list[["options"]][["jobs"]] <- parallel::detectCores()
  }

  # set model
  if (!is.null(model)) {
    afni_list[["options"]][["model"]] <- paste0('"', model, '"')
  }

  # set vars
  if (!is.null(quant_vars)) {
    afni_list[["options"]][["qVars"]] <- paste0('"', quant_vars, '"')
  }

  # set rand effects
  if (!is.null(rand_eff)) {
    afni_list[["options"]][["ranEff"]] <- paste0('"', rand_eff, '"')
  }

  # ss type
  if (!is.null(ss_type)) {
    afni_list[["options"]][["SS_type"]] <- ss_type
  }

  # glt table
  if (!is.null(in_glt_table)) {
    afni_list[["options"]][["gltTable"]] <- list(in_glt_table)
  }

  # glf table
  if (!is.null(in_glf_table)) {
    afni_list[["options"]][["glfTable"]] <- list(in_glf_table)
  }

  # extra options
  extra_options
  if (!is.null(in_glf_table)) {
    afni_list[["options"]][["extra"]] <- extra_options
  }

  # data table
  if (!is.null(in_data_table)) {
    if (typeof(in_data_table) == "character") {
      afni_list[["options"]][["dataTable"]] <- in_data_table
    } else {
      afni_list[["options"]][["dataTable"]] <- list(in_data_table)
    }
  }
  
  # write out command -----
  # using for loop
  # if then for exceptions
  options_list = names(afni_list$options)
  afni_command <- paste0(afni_list$program, " \\\n")
  for (option in options_list) {
    if (option == "gltTable") {
      afni_in_glt_table <- afni_list[["options"]][["gltTable"]][[1]] %>%
        mutate(afni_glt_table = glue("-gltLabel {n_glt} '{glt_label}' -gltCode {n_glt} '{glt_code}'"))
      afni_in_glt_table <- paste0(afni_in_glt_table$afni_glt_table, collapse = " \\\n")
      afni_in_glt_table <- paste0("-num_glt ", nrow(in_glt_table), " \\\n", afni_in_glt_table, " \\\n")
      afni_command <- paste0(afni_command, afni_in_glt_table)
    } else if (option == "glfTable") {
      afni_in_glf_table <- afni_list[["options"]][["glfTable"]][[1]] %>%
        mutate(afni_glf_table = glue("-glfLabel {n_glf} '{glf_label}' -glfCode {n_glf} '{glf_code}'"))
      afni_in_glf_table <- paste0(afni_in_glf_table$afni_glf_table, collapse = " \\\n")
      afni_in_glf_table <- paste0("-num_glf ", nrow(in_glf_table), " \\\n", afni_in_glf_table, " \\\n")
      afni_command <- paste0(afni_command, afni_in_glf_table)
    } else if (option == "dataTable") {
      if (typeof(in_data_table) == "character") {
        afni_in_data_table <- paste0("-dataTable @", afni_list[['options']][[option]])
      } else {
        afni_in_data_table <- unite(afni_list[["options"]][["dataTable"]][[1]], "afni_data_table", colnames(afni_list[["options"]][["dataTable"]][[1]]), sep = "\t")
        afni_in_data_table <- paste0(afni_in_data_table$afni_data_table, collapse = " \\\n")
        afni_in_data_table <- paste0(c("-dataTable", paste0(colnames(in_data_table), collapse = "\t"), afni_in_data_table), collapse = " \\\n")
      }
      afni_command <- paste0(afni_command, afni_in_data_table)
    } else if (option == "extra") {
      afni_command <- paste0(afni_command, extra_options, " \\\n")
    } else {
      afni_command <- paste0(afni_command, "-", option, " ", afni_list[['options']][[option]], " \\\n")
    }
  }
  
  afni_list[["command"]] <-  paste0(afni_list$path, afni_command)
  afni_list <<- afni_list

  cat(paste0("Command:\n", afni_command))

  # combine afni path and command
  afni_command <- paste0(afni_list$command)
  
  if (run_cmd == TRUE) {
    # run in command-line
    cat("\n\nRunning Command:\n")
    system(afni_command)
  }
  

}
