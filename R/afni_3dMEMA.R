#rm(list = ls())
#cat("\014")

afni_3dMEMA <- function(in_data_table, out_file, in_mask = NULL, in_cov_table = NULL, intercept = NULL, slope = NULL, jobs = "all") {

  # load packages -----
  packages <- c("tidyverse", "glue")
  xfun::pkg_attach(packages, message = F, install = T)

  # get afni path and function -----
  afni_path <- afnir::get_afni()
  afni_func <- paste0("3dMEMA", " \\\n")

  # set out_file
  afni_prefix <- paste0("-prefix ", out_file, " \\\n")

  # set jobs
  if (jobs == "all") {
    afni_jobs <- paste0('-jobs ', parallel::detectCores(), " \\\n")
  } else {
    afni_jobs <- paste0('-jobs ', jobs, " \\\n")
  }

  # create data tables -----
  afni_in_data_table <- NULL
  afni_in_data_table_raw <- unite(in_data_table, "afni_data_table", colnames(in_data_table)[-1], sep = "\t")

  # obtain unique sets
  set_list <- unique(in_data_table$set)
  for (set in set_list) {
    afni_in_data_table <- paste0(afni_in_data_table, "-set ", set, " \\\n")
    afni_in_data_table_set <- paste0(afni_in_data_table_raw %>% select(afni_data_table) %>% unlist(), collapse = " \\\n")
    afni_in_data_table <- paste0(afni_in_data_table, afni_in_data_table_set, " \\\n")
  }

  cmd <- paste0(afni_func, afni_prefix, afni_jobs, afni_in_data_table, collapse = "")

  # mask
  if (!is.null(in_mask)) {
    afni_in_mask <- paste0("-mask ", in_mask, " \\\n")
    cmd <- paste0(cmd, afni_in_mask, collapse = "")
  }

  # covariates
  if (!is.null(in_cov_table)) {
    n_covariates <- length(colnames(read_tsv(in_cov_table_path))[-1])
    afni_cov_path <- paste0("-covariates ", in_cov_table_path, " \\\n")
    afni_cov_center <- paste0("-covariates_center ", paste0(rep(0, n_covariates), collapse = " "), " \\\n")
    afni_cov_model <- paste0("-covariates_model center=", intercept, " slope=", slope)
    cmd <- paste0(cmd, afni_cov_path, afni_cov_center, afni_cov_model, collapse = "")
  }

  # command
  cat(paste0("Command:\n", cmd))

  # combine afni path and command
  afni_cmd <- paste0(afni_path, cmd)

  # run in command-line
  cat("\n\nRunning:\n")
  system(afni_cmd)

}

# example
#in_data_table <- read_csv("R/in_data_table_mema.csv")
#in_cov_table_path <- "R/in_cov_table_mema.txt"

#afni_3dMEMA(in_data_table = in_data_table, out_file = "output.nii.gz", in_mask = "test.nii.gz", in_cov_table = in_cov_table_path, intercept = "different", slope = "different")
