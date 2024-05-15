profiling_target <- function(...){
  opts <- list(...)
  root_dir <- getOption("targets_profile_dir", default=getwd())
  target_name <- quote(opts$name) |> as.character()
  prof_filename <- paste0(target_name, ".txt")
  prof_file <- file.path(root_dir, prof_filename)

  targets::tar_target(
    command = {
      utils::Rprof(memory.profiling = TRUE, filename = prof_file)
      result <- eval(opts$command)
      Rprof(NULL)
      attributes(result, "prof") <- prof_file
      result
    },
    ...
  )
}
