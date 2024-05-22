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

#' Read the monitoring data
#'
#' @param monitor_path Path to the directory previously passed to [monitor_pid].
#'  Character scalar.
#' @return Tibble containing monitoring data. Each row is one timepoint, and
#'  the columns correspond to various process metrics.
#' @export
profile_data <- function(monitor_path){
  list.files(monitor_path, pattern = ".*\\.rds", full.names = TRUE) |>
    purrr::map(readRDS) |>
    purrr::list_rbind()
}

#' Plot the memory usage over time
#'
#' @inheritParams profile_data
#' @return A `ggplot` object
#' @export
#' @import ggplot2
memory_plot <- function(monitor_path, meta_args = list()){

  profile <- monitor_path |>
    profile_data() |>
    dplyr::mutate(memory_usage = rss / 1E6, pid = as.factor(pid)) |>
    dplyr::select(time, memory_usage, pid)

  targets <- do.call(targets::tar_meta, meta_args) |>
    dplyr::filter(
      !is.na(time)
    ) |>
    dplyr::mutate(
      # When the target ended
      end_time = time,
      # When the target started
      start_time = end_time - seconds,
      # Middle of execution
      mid_time = end_time - (seconds / 2),
      # The end of the previous target
      preceding_end = dplyr::lag(end_time, default = min(profile$time)),
      # Time between the previous target and this target ending
      time_between = end_time - preceding_end,
      # Midpoint between the previous target and this target ending
      display_midpoint = preceding_end + (time_between / 2)
    )

  combined <- dplyr::inner_join(profile, targets, by = dplyr::join_by(
    dplyr::between(time, start_time, end_time)
  ), suffix = c("_profile", "_targets"))

  max_mem <- combined |>
    dplyr::slice_max(memory_usage, by = name, with_ties = FALSE)

  ggplot() +
    geom_line(aes(x = time, y = memory_usage), data = profile) +
    xlab("Time") +
    ylab("Memory Usage (MB)") +
    geom_rect(aes(xmin = start_time, xmax = end_time, ymin = -Inf, ymax = Inf, fill = name, colour = name), data = targets, alpha=0.3) +
    geom_point(aes(x = time_profile, y = memory_usage, color = name), data = max_mem) +
    geom_label(aes(x = time_profile, y = memory_usage, label = format(memory_usage, digits = 1), color = name), data = max_mem, vjust = "bottom", nudge_y = 200, size = 3) +
    scale_x_datetime(
      timezone = Sys.timezone(),
      date_labels = "%H:%M:%S"
    )
}
