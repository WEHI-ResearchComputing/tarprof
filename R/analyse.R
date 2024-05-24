
#' Read the monitoring data
#' @param monitor_path Path to the directory previously passed to [monitor_pid].
#'  Character scalar.
#' @return Tibble containing monitoring data. Each row is one timepoint, and
#'  the columns correspond to various process metrics.
#' @export
#' @concept profiling-analysis
profile_data <- function(monitor_path){
  list.files(monitor_path, pattern = ".*\\.rds", full.names = TRUE) |>
    purrr::map(readRDS) |>
    purrr::list_rbind()
}

#' Add some time-based variables to targets metadata
#' @param targets_metadata The result of `targets::tar_meta()` for your pipeline
#'   of interest
#' @return A copy of `targets_metadata`, with some additional columns
#' @export
#' @examples
#' process_meta()
#' @concept profiling-analysis
process_meta <- function(targets_metadata = targets::tar_meta()){
  if (nrow(targets_metadata) == 0) cli::cli_abort(
    'No targets store could be detected. Are you sure you ran the pipeline?
    If you ran your pipeline in a directory other than the current directory,
    you need to specify the path to the store by using
    {.code memory_plot(profile_path, targets_metadata = targets::tar_meta(store = "/some/path/_targets"))}'
  )

  targets_metadata|>
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
}

#' Groups profile entries per target
#' @param profile A data frame obtained from [profile_data]
#' @param targets A data frame obtained from [process_meta]
#' @return A data frame with all the columns from both `profile` and `target`
#' @export
#' @concept profiling-analysis
profile_per_target <- function(profile, targets = process_meta()){
  combined <- dplyr::inner_join(profile, targets, by = dplyr::join_by(
    dplyr::between(time, start_time, end_time)
  ), suffix = c("_profile", "_targets"))
}

#' Calculate some resources summaries for each target
#' @param combined The result of [profile_per_target]
#' @return A summary data frame, with one row per target. The columns include:
#'
#'   * `name`: The target name
#'   * `peak_memory`: The maximum memory that this target used
#'   * `peak_memory_time`: The time at which the maximum memory usage was reached
#'   * `cpu_efficiency`: The percentage of the time that this target used the CPU
#' @export
#' @concept profiling-analysis
summarise_targets <- function(combined){
  combined |>
    dplyr::summarise(
      peak_memory = max(rss),
      peak_memory_time = time_profile[[which.max(rss)]],
      start_user = min(user),
      start_sys = min(system),
      end_user = max(user),
      end_sys = max(system),
      total_user = end_user - start_user,
      total_sys = end_sys - start_sys,
      cpu_efficiency = (total_user + total_sys) / seconds[[1]],
      .by = name
    ) |>
    dplyr::select(
      name,
      peak_memory,
      peak_memory_time,
      cpu_efficiency
    )
}

#' Plot the memory usage over time
#' @inheritParams profile_per_target
#' @return A `ggplot` object
#' @export
#' @import ggplot2
#' @concept profiling-analysis
memory_plot <- function(profile, targets = process_meta()){

  per_target <- profile_per_target(profile, targets)

  summary <- per_target |>
    summarise_targets() |>
    dplyr::mutate(
      peak_memory = peak_memory / 1E6
    )

  profile_mb <- profile |>
    dplyr::mutate(
      memory_usage = rss / 1E6,
      pid = as.factor(pid)
    )

  ggplot() +
    geom_line(aes(x = time, y = memory_usage), data = profile_mb) +
    xlab("Time") +
    ylab("Memory Usage (MB)") +
    geom_rect(aes(xmin = start_time, xmax = end_time, ymin = -Inf, ymax = Inf, fill = name, colour = name), data = per_target, alpha=0.3) +
    geom_point(aes(x = peak_memory_time, y = peak_memory, color = name), data = summary) +
    geom_label(aes(x = peak_memory_time, y = peak_memory, label = format(peak_memory, digits = 1), color = name), data = summary, vjust = "bottom", nudge_y = 200, size = 3) +
    scale_x_datetime(
      timezone = Sys.timezone(),
      date_labels = "%H:%M:%S"
    )
}
