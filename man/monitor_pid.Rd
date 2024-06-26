% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/profile.R
\name{monitor_pid}
\alias{monitor_pid}
\title{Monitor a process}
\usage{
monitor_pid(pid, interval = 0.1)
}
\arguments{
\item{pid}{Pid of the process to monitor. Integer vector with length 1.}

\item{interval}{Amount of time in seconds between resource polls. Double vector of length 1.}
}
\value{
A data frame of resource results, one row per timepoint
}
\description{
This is not unique to Targets, and may be useful elsewhere.
Note that this function will only ever terminate if the process it's
monitoring terminates. Keep this in mind.
}
\concept{profiling}
