% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/benchmark.R
\name{genBenchJobs}
\alias{genBenchJobs}
\title{Parallelize a benchmark on data.table by calling benchmark on equal subsets 
of its lines.}
\usage{
genBenchJobs(f, fname = NULL, script.file = fname, table.file,
  table.name = "NULL", ids = NULL, params = "",
  paramsEvalAtCall = "", perJob = 10, type = c("shell", "bsub",
  "qsub"), command = NULL, requires = c(), sources = c(),
  code = "", bsub.W = "12:00", bsub.n = 1, bsub.mem = 1000,
  bsub.other = "", qsub.q = "sc02.q", sleep.every = 50,
  sleep.secs = 300)
}
\arguments{
\item{f}{a character or a function object called on each line of benchdata.}

\item{fname}{a character indicating the name of the column to be filled.}

\item{script.file}{a character containing a part of the file-name of the 
generated scripts:}

\item{table.file}{a character indicating the name of a .RData file containing
a data.table object.}

\item{table.name}{a character string indicating the name of the data.table
variable found in `table.file`.}

\item{ids}{a vector indicating a possible subset of the id-column of benchdata 
on which to perform the benchmark. Defaults to NULL in which case all lines 
are consiedered.}

\item{params}{a character containing the additional parameters to f as they 
should be written in the call to it, ex. "foo1=5, foo2=6".}

\item{paramsEvalAtCall}{a character, either an empty string '', or an R expression or comma separated expressions
such as 'parFixed=c(theta=mean(p$v))), parMax=c(theta=max(p$v))'. At evaluation these expressions have access to 
the list called p which is passed as a first argument to f. This character string should not contain 
double-quotes.}

\item{perJob}{number of lines per subset, defaults to 10.}

\item{type}{the type of command to generate, defaults to 'shell'. Currently, 
should be one of 'shell', 'bsub', 'qsub'.}

\item{command}{a character indicating a custom command to be used, defaults 
to NULL. If specified, the type parameter is ignored.}

\item{requires}{a vector of strings indicating R-pacages to be loaded prior 
to executing the benchmark}

\item{sources}{a vector of strings indicating R-files to be sourced prior to
executing the benchmark.}

\item{code}{a string containing any r-code that is to be prepended to the R-script 
before the call to f.}

\item{bsub.W}{a character indicating job-waiting time if type  'bsub', 
defaults to '12:00'}

\item{bsub.n}{a character indicating job-number of processes if type ='bsub', 
defaults to 1.}

\item{bsub.mem}{an integer indicating number of megabites used by a job if
type = 'bsub', defaults to 1000.}

\item{bsub.other}{Character containing other options of bsub to be
written first.}

\item{qsub.q}{a character indicating queuename if type='qsub', defaults to 
'sc02.q'}

\item{sleep.every}{integer, number of job-submit commands between a sleep 
command}

\item{sleep.secs}{integer, number of seconds to sleep after every sleep.every
job-submissions}
}
\value{
a character vector containing the generated shell commands 
(see details).
}
\description{
Parallelize a benchmark on data.table by calling benchmark on equal subsets 
of its lines.
}
\details{
This function generates two files: 
1. an R-script, j_<script.file>_.R, which exctracts table.file and 
line-numbers from the command-line arguments of the R-process and calls 
benchmark executing f on those lines;
2. a shell script, j_<type>_<script.file>.sh of commands for starting an R 
process and executing the R-script on specific lines of the data.table.
}
