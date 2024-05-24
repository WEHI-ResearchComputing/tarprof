#' Monitor a process
#'
#' This is not unique to Targets, and may be useful elsewhere.
#' Note that this function will only ever terminate if the process it's
#' monitoring terminates. Keep this in mind.
#'
#' @param pid Pid of the process to monitor. Integer vector with length 1.
#' @param interval Amount of time in seconds between resource polls. Double vector of length 1.
#' @export
#' @return A data frame of resource results, one row per timepoint
#' @concept profiling
monitor_pid <- function(pid, interval = 0.1){
  handle <- ps::ps_handle(pid = as.integer(pid))
  i <- 1
  results <- vector("list", length = 10000)

  while (TRUE) {

    # Execute current poll
    result <- try({
      memory <- handle |> ps::ps_memory_full_info() |> as.list()
      cpu <- handle |> ps::ps_cpu_times() |> as.list()

      result <- modifyList(memory, cpu)
      result$time <- Sys.time()
      result$pid <- pid

      tibble::as_tibble_row(result)
    }, silent = TRUE)

    if (inherits(result, "try-error")) break

    results[[i]] <- result

    # Expand the list as necessary
    i <- i + 1
    if (i > length(results)){
      results <- c(results, vector("list", length = 10000))
    }

    # Wait for next poll
    Sys.sleep(interval)
  }

  results |>
    purrr::discard(is.null) |>
    purrr::list_rbind()
}

#' Monitor a process and save the results
#'
#' Users will typically not need to call this function directly.
#' Instead, prefer [monitor_pid].
#'
#' @param path Path to the RDS where the results will be saved. Character scalar.
#' @inheritDotParams monitor_pid pid interval
#' @return Nothing
#' @export
#' @concept profiling
monitor_save <- function(path, ...){
  df <- tarprof::monitor_pid(...)
  saveRDS(df, path)
}

#' callr-compatible function that monitors the process's resources
#'
#' This should be used as the value of `callr_function` for [tar_make()].
#'
#' @inheritParams callr::r_bg
#' @param monitor_path Path to the RDS where the results will be saved. Character scalar.
#' @param monitor_interval Amount of time in seconds between resource polls. Double vector of length 1.
#' @return The result of the function, identically to a standard callr function
#' @export
#' @concept profiling
callr_profile <- function(..., monitor_path, monitor_interval = 0.1){
  dir.create(monitor_path, showWarnings = FALSE)
  # We need to explicitly use "" to ensure that the stderr is forwarded
  # to the R console
  process <- callr::r_bg(..., stderr = "", stdout = "")
  pid <- process$get_pid()
  filename <- paste0(pid, ".rds")
  monitor <- callr::r_bg(tarprof::monitor_save, args = list(
    pid = pid,
    path = file.path(monitor_path, filename),
    interval = monitor_interval
  ), error = "error")
  process$wait()

  # Wait for monitor to finish, then handle any errors
  monitor$wait()
  if (monitor$get_exit_status() != 0){
    stop(monitor$read_error())
  }

  # Return the regular result
  process$get_result()
}
