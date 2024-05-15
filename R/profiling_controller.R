profiling_launcher_class <- R6::R6Class(
  classname = "profiling_launcher_class",
  inherit = crew::crew_class_launcher,
  public = list(
    launch_worker = function(call, name, launcher, worker, instance) {
      bin <- file.path(R.home("bin"), "R")
      processx::process$new(
        command = bin,
        args = c("-e", call),
        cleanup = FALSE
      )
    },
    terminate_worker = function(handle) {
      handle$signal(crew::crew_terminate_signal())
    }
  )
)

crew_controller_custom <- function(
    inner
) {
  client <- crew::crew_client(
    name = name,
    workers = workers,
    host = host,
    port = port,
    tls = tls,
    seconds_interval = seconds_interval,
    seconds_timeout = seconds_timeout
  )
  launcher <- custom_launcher_class$new(
    name = name,
    seconds_interval = seconds_interval,
    seconds_timeout = seconds_timeout,
    seconds_launch = seconds_launch,
    seconds_idle = seconds_idle,
    seconds_wall = seconds_wall,
    tasks_max = tasks_max,
    tasks_timers = tasks_timers,
    reset_globals = reset_globals,
    reset_packages = reset_packages,
    reset_options = reset_options,
    garbage_collection = garbage_collection,
    launch_max = launch_max,
    tls = tls
  )
  controller <- crew::crew_controller(client = client, launcher = launcher)
  controller$validate()
  controller
}
