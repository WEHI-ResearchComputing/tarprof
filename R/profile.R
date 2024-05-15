#' Monitor a process
#'
#' @param pid Pid of the process to monitor. Integer vector with length 1.
#' @param interval Amount of time in seconds between resource polls. Double vector of length 1.
#'
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
#' @param path Path to the RDS where the results will be saved. Character scalar.
#' @inheritParams monitor_pid
#' @return Nothing
#' @export
monitor_save <- function(path, ...){
  df <- tarprof::monitor_pid(...)
  saveRDS(df, path)
}

#' callr-compatible function that monitors the process's resources
#'
#' @inheritParams callr::r_bg
#' @param monitor_path
#' @param monitor_interval
#'
#' @return
#' @export
#'
#' @examples
callr_profile <- function(..., monitor_path, monitor_interval = 0.1){
  dir.create(monitor_path, showWarnings = FALSE)
  process <- callr::r_bg(...)
  pid <- process$get_pid()
  filename <- paste0(pid, ".rds")
  monitor <- callr::r_bg(tarprof::monitor_save, args = list(
    pid = pid,
    path = file.path(monitor_path, filename),
    interval = monitor_interval
  ))
  process$wait()
  monitor$wait()
  process$get_result()
}

profile_data <- function(monitor_path){
  list.files(monitor_path, pattern = ".*\\.rds", full.names = TRUE) |>
    purrr::map(readRDS) |>
    purrr::list_rbind()
}

#' Title
#'
#' @param monitor_path
#'
#' @return
#' @export
#' @import ggplot2
#' @examples
memory_plot <- function(monitor_path, meta_args = list()){
  meta <- do.call(targets::tar_meta, meta_args)

  memory <- monitor_path |>
    profile_data() |>
    dplyr::mutate(memory_usage = rss / 1E6, pid = as.factor(pid)) |>
    dplyr::select(time, memory_usage, pid)

  time_start <- min(meta$time, memory$time)



    ggplot() +
      geom_line(aes(x = time, y = memory_usage, color = pid), data = memory) +
      xlab("Time") +
      ylab("Memory Usage (MB)") +
      geom_vline(aes(xintercept = time), data = meta) +
      geom_text(aes(x = time, label = name), data = meta, y = 0, angle = 90, hjust = "left", vjust = "top")
}

#' Title
#'
#' @param monitor_path
#'
#' @return
#' @export
#' @import ggplot2
#' @examples
cpu_plot <- function(monitor_path){
  plot <- monitor_path |>
    profile_data() |>
    dplyr::mutate(memory_usage = rss / 1E6, pid = as.factor(pid)) |>
    dplyr::select(time, memory_usage, pid) |>
    ggplot(aes(x = time, y = memory_usage, color = pid))

  plot +
    geom_line() +
    xlab("Time") +
    ylab("Memory Usage (MB)")
}
