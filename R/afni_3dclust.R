afni_3dclust <- function(in_file, out_file, nearest_neighbor, cluster_min, extra_options = NULL) {
  
  # initialize list -----
  afni_list <- list(path = afnir::get_afni(),
                    program = "3dclust",
                    options = NULL)

  afni_list[["options"]][[paste0("NN", nearest_neighbor)]] <- cluster_min
  
  afni_list[["options"]][["prefix"]] <- out_file
  
  if (!is.null(extra_options)) {
    afni_list[["options"]][["extra_options"]] <- extra_options
  }
  
  afni_list[["options"]][["in_file"]] <- in_file
  
  options_list = names(afni_list$options)
  afni_command <- paste0(afni_list$program, " \\\n")
  for (option in options_list) {
    if (option == "in_file") {
      afni_command <- paste0(afni_command, afni_list[['options']][[option]])
    } else if (option == "extra_options") {
      afni_command <- paste0(afni_command, afni_list[['options']][[option]], " \\\n")
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


