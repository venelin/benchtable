library(data.table)

#' @title Apply a function to each row of a data.table and add the result as a column
#' 
#' @param f a character or a function object
#' @param benchdata a data.table, can be NULL in which case the table.file is 
#' loaded expecting it to contain a data.table object.
#' @param fname a character indicating the column-name to be used for the column 
#' of result, can be NULL in which case the name of the function specified by f
#' is used.
#' @param table.file a character specifying a .RData file where the resulting 
#' data.table object is to be stored. Defaults to NULL. See 
#' benchdata.
#' @param report a reporting function \code{(p, obj)} called after executing f 
#' on a row, used for logging purpose.
#' @param result.only logical indicating whether only the list of result objects should be returned, default FALSE
#' @param id.result.only logical indicating whether the returned data.table shall be limited to the id and fname 
#' columns only, default FALSE. Ignored if result.only is specified as TRUE. 
#' @param paramsEvalAtCall a character, either an empty string '', or an R expression or comma separated expressions
#' such as 'parFixed=c(theta=mean(p$v))), parMax=c(theta=max(p$v))'. At evaluation these expression have access to 
#' the list p that is passed to f. 
#' @param ... additional parameters passed to f
#' @return If result.only is FALSE (default) a data.table object equal to the original benchdata with a column 
#' named as fname for the results of calling f. If a column of the same name
#' already exists, it is overwritten. If the parameter id.result.only is FALSE (default) all the remaining columns
#' are also contained in the returned data.table. Otherwise only the columns id, and fname are returned. If result.only
#' is TRUE, than only a list of the resulting objects for each row of the original data.table is returned.
#' 
#' @export
benchmark <- function(f, benchdata, fname=NULL, table.file=NULL, 
                      report=function(p, obj) {}, result.only=F, id.result.only=F, 
                      paramsEvalAtCall='', ...) {
  if(!is.null(paramsEvalAtCall)&paramsEvalAtCall!='') {
    paramsEvalAtCall <- paste0('list(', paramsEvalAtCall, ')')
  }
  addParam <- list(...)
  if(is.character(f)) {
    fn <- get(f)
  } else {
    fn <- f
  }
  if(is.null(fname)) {
    if(is.character(f)) 
      fname <- f
    else
      fname <- as.character(match.call()$f)
  }
  
  benchdata.name <- 'benchdata'
  if(is.null(benchdata) & !is.null(table.file)) {
    newEnv <- new.env()
    load(table.file, envir=newEnv)
    benchdata.name <- tables(env=newEnv)[1,NAME]
    benchdata <- get(benchdata.name, envir=newEnv)
  }
  
  time <- system.time(
    fits <- apply(benchdata, 1, function(p) {
      time1 <- proc.time()[3]  # nested calls to system.time() too slow!
      obj <- try(
        do.call(fn, c(list(p), eval(parse(text=paramsEvalAtCall)), list(...))), 
        silent = T)
      
      if(!is.null(obj)) {
        if(is.null(attr(obj, 'time'))) {
          attr(obj, 'time') <- proc.time()[3]-time1
        } else {
          attr(obj, 'time') <- attr(obj, 'time') + proc.time()[3] - time1
        }
        attr(obj, 'YmdH') <- as.integer(format(Sys.time(), "%Y%m%d%H"))
        attr(obj, 'addParam') <- addParam
        attr(obj, 'paramsEvalAtCall') <- paramsEvalAtCall
      }
      
      report(p, obj)
      obj
    })
  )
  cat("Fitting time:", time[3], "s", '\n')
  
  if(result.only) {
    fits
  } else {
    if(!is.null(table.file)) {
      # reload the table file to update from possible parallel runs (thread
      # unsafe)
      if(file.exists(table.file))
        load(table.file)  
      benchdata <- get(benchdata.name)
      benchdata[[fname]] <- fits
      assign(benchdata.name, benchdata)
      save(list=benchdata.name, file=table.file)  
    } else {
      benchdata[[fname]] <- fits
    }
    
    if(id.result.only) {
      benchdata[, c('id', fname), with=F]
    } else {
      benchdata
    }
  }
}


#' @title Extract command-line arguments specifying a call to benchmark. 
#' @note This is an internal function
#' @export
doBenchJob <- function(fname, f, table.file, ids, ...) {
  cat('Executing ', f, ' on ', table.file, 
      ', ids: ', do.call(paste, as.list(ids)), '\n', sep='')
  
  newEnv <- new.env()
  load(table.file, envir=newEnv)
  benchdata.name <- tables(env=newEnv)[1,NAME]

  cat('Found data.table object:', benchdata.name, '\n')
  benchdata <- get(benchdata.name, envir=newEnv)
  
  ..ids.. <- ids
  b <- benchdata[id %in% ..ids..]
  print('Executing job on the following part of the data.table:')
  print(b)
  
  rm(list=benchdata.name, envir=newEnv)
  
  b <- benchmark(f, b, fname, NULL, id.result.only=T, ...)
  save(b, file=paste0('job_', fname, '_', ids[1], '_.RData'))
  b
}

#' @title Parallelize a benchmark on data.table by calling benchmark on equal subsets 
#' of its lines.
#' 
#' @param f a character or a function object called on each line of benchdata.
#' @param fname a character indicating the name of the column to be filled.
#' @param script.file a character containing a part of the file-name of the 
#'    generated scripts: 
#' @param table.file a character indicating the name of a .RData file containing
#' a data.table object.
#' @param ids a vector indicating a possible subset of the id-column of benchdata 
#' on which to perform the benchmark. Defaults to NULL in which case all lines 
#' are consiedered.
#' @param params a character containing the additional parameters to f as they 
#'   should be written in the call to it, ex. "foo1=5, foo2=6".
#' @param paramsEvalAtCall a character, either an empty string '', or an R expression or comma separated expressions
#' such as 'parFixed=c(theta=mean(p$v))), parMax=c(theta=max(p$v))'. At evaluation these expressions have access to 
#' the list called p which is passed as a first argument to f. This character string should not contain 
#' double-quotes.
#' @param perJob number of lines per subset, defaults to 10.
#' @param type the type of command to generate, defaults to 'shell'. Currently, 
#' should be one of 'shell', 'bsub', 'qsub'.
#' @param command a character indicating a custom command to be used, defaults 
#' to NULL. If specified, the type parameter is ignored.
#' @param requires a vector of strings indicating R-pacages to be loaded prior 
#' to executing the benchmark
#' @param sources a vector of strings indicating R-files to be sourced prior to
#' executing the benchmark.
#' @param code a string containing any r-code that is to be prepended to the R-script 
#' before the call to f.
#' @param bsub.W a character indicating job-waiting time if type  'bsub', 
#' defaults to '12:00'
#' @param bsub.n a character indicating job-number of processes if type ='bsub', 
#' defaults to 1.
#' @param bsub.mem an integer indicating number of megabites used by a job if
#' type = 'bsub', defaults to 1000.
#' @param qsub.q a character indicating queuename if type='qsub', defaults to 
#' 'sc02.q'
#' @param sleep.every integer, number of job-submit commands between a sleep 
#' command
#' @param sleep.secs integer, number of seconds to sleep after every sleep.every
#' job-submissions
#' @return a character vector containing the generated shell commands 
#' (see details).
#' @details This function generates two files: 
#' 1. an R-script, j_<script.file>_.R, which exctracts table.file and 
#' line-numbers from the command-line arguments of the R-process and calls 
#' benchmark executing f on those lines;
#' 2. a shell script, j_<type>_<script.file>.sh of commands for starting an R 
#' process and executing the R-script on specific lines of the data.table. 
#' 
#' @export
#' 
genBenchJobs <- function(f, fname=NULL, script.file=fname, table.file, ids=NULL, 
                         params='', paramsEvalAtCall='',
                         perJob=10, 
                         type=c('shell', 'bsub', 'qsub'), command=NULL,
                         requires=c(), sources=c(), code="",
                         bsub.W='12:00', bsub.n=1, bsub.mem=1000, 
                         qsub.q='sc02.q', 
                         sleep.every=50, sleep.secs=300) {
  
  jobCommand <- function(type, name='', 
                         bsub.W='', bsub.n='', bsub.mem='', 
                         qsub.q='') {
    c(shell='sh -c', 
      bsub=paste('bsub -W ', bsub.W, ' -n ', bsub.n, 
                 ' -R "rusage[mem=', bsub.mem, ']"', ' -J ', name, ' ', sep=''), 
      qsub=paste('qsub -V -cwd ', '-N ', name, ' -q ', qsub.q, 
                 ' -b y -shell y ',  sep='') )[type]
  }
  
  if(is.character(f)) {
    charf <- f
  } else {
    charf <- as.character(match.call()$f)
  }
  if(is.null(fname)) {
    fname <- charf
  } 
  
  if(is.null(script.file)) {
    script.file <- fname
  }
  
  cmd.file <- paste('jsub_', script.file, '.sh', sep='')
  
  if(is.null(ids)) {
    newEnv <- new.env()
    load(table.file, envir=newEnv)
    benchdata.name <- tables(env=newEnv)[1,NAME]
    ids <- get(benchdata.name, envir=newEnv)[, id]
  }
  
  set.seed(1)
  samp <- if(length(ids)==1) {
    ids
  } else {
    sample(ids, length(ids), replace=F)
  }
  ids <- matrix(samp, ncol=perJob, byrow=T)
  
  requires <- c('benchtable', requires)
  if(length(requires) > 0) {
    reqs <- do.call(paste, as.list(paste("require(", requires, ")\n", 
                                         sep='')))
  } else {
    reqs <- ''
  }
  
  if(length(sources) > 0) {
    src <- do.call(paste, as.list(paste("source('", sources, "')\n", 
                                        sep='')))
  } else {
    src <- ''
  }

  if(paramsEvalAtCall!='') {
    paramsEvalAtCall <- paste0(', paramsEvalAtCall="', paramsEvalAtCall, '"')
  }
  
  if(params!='') 
    params <- paste0(', ', params)
  
  args <- 
'args <- commandArgs(trailingOnly = TRUE)
f <- as.character(args[1])
table.file <- as.character(args[2])
ids <- as.integer(args[-(1:2)])
'
  
  rscript <- paste0(reqs, src, '\n#user code \n', code, '\n# end of user code\n', 
                    args, 'b <- doBenchJob(fname="', fname, '", f, table.file, ids', paramsEvalAtCall, params, ')\n')
  rscriptFile <- paste('j_', script.file, '.R', sep='')
  write(rscript, file=rscriptFile)
  
  backgr <- ifelse(type=='shell', '&', '')
  cmd <- apply(ids, 1, function(id) {
    id <- sort(id)
    if(is.null(command)) {
      command <- jobCommand(type[1], paste('j_', id[1], '_', fname, sep=''),
                            bsub.W, bsub.n, bsub.mem, qsub.q)
    }
    
    paste(command, "'", "R --vanilla --slave", "-f", rscriptFile, 
          "--args", charf, table.file, do.call(paste, as.list(id)), "'", backgr, "\n")
  })
  
  sleeps <- rep('', length(cmd))
  if(length(cmd)>sleep.every) {
    sleeps[seq(sleep.every, length(cmd), by=sleep.every)] <- 
      paste('sleep', sleep.secs, '\n')
  }
  
  write(do.call(paste0, as.list(rbind(cmd, sleeps))), 
        paste0('j_', type, '_', script.file, '.sh'))
  cmd
}

#' @title Collect benchmark results from .RData files 
#' 
#' @param fs a character vector containing function names used for executing 
#' previous benchmarks
#' @param benchdata a data.table where the collected results will be stored as
#' columns, defaults to NULL in which case table.file should be specified.
#' @param table.file a file containing a data.table object where the 
#' results will be added to, defaults to NULL in which case benchdata should be
#' specified.
#' @param ids NULL or vector of integers or a list of integer vectors of length
#' equal to length(fs) containing id's of lines for which results are to be 
#' collected. 
#' @param res.ids.only logical indicating whether to restrict the result to the 
#' lines indicated by ids. Default is TRUE.
#' @param dir.res character indicating the directory where result-files are 
#' stored, default is '.'.
#' @param write.file a logical indicating whether to write the resulting 
#' data.table to the file table.file, defaults to FALSE.
#' @param verbose logical indicating whether some file-loading messages should be output
#' 
#' @return the resulting data.table object with added (or updated) columns for 
#' each element of fs.
#' @seealso \code{\link{genBenchJobs}}
#' @export
collectBenchRes <- function(fs, benchdata=NULL, table.file=NULL, ids=NULL,
                            res.ids.only=T, dir.res='.', write.file=F, verbose=F) {
  if(is.null(benchdata) & !is.null(table.file)) {
    if(verbose) 
      cat('Loading file ', table.file, '...\n')
    newEnv <- new.env()
    load(table.file, envir=newEnv)
    benchdata.name <- tables(env=newEnv)[1,NAME]
    benchdata <- get(benchdata.name, envir=newEnv)
  }
  if(!is.null(ids)) {
    if(!is.list(ids)) {
      idsf <- ids
      ids <- lapply(fs, function(f) idsf)
    } else {
      if(!(is.list(ids) & length(ids)==length(fs))) {
        stop('Argument ids should be NULL or vector or a list of length equal
              to length(fs) containing integer vectors.')
      }
    }
    names(ids) <- fs
    if(!is.null(benchdata) & res.ids.only==T) {
      benchdata <- benchdata[data.table(id=unique(unlist(ids)))]
    }
  }
  if(!is.null(benchdata)) {
    cs <- match(fs, names(benchdata))
    cs <- cs[!is.na(cs)]
    if(length(cs)) {
      warning(paste('Matching entries in column(s)', toString(names(benchdata)[cs]), 'will be overwritten.'))
      #benchdata <- benchdata[, -cs, with=F]
    }
  }
  for(f in fs) {
    if(verbose) {
      cat('Collecting ', f, '...\n', sep='')
    }
    if(is.character(f)) 
      fname <- f
    else
      fname <- as.character(match.call()$f)
  
    if(is.null(ids)) {
      jobres <- system(paste0('find ', dir.res, '/ -name "job_', fname, '_*RData"'), 
                       intern=T)
    } else {
      idsf <- ids[[fname]]
      jobres <- sapply(idsf, function(id) {
        system(paste0('find ', dir.res, '/ -name "job_', fname, '_', id, '_*RData"'), 
               intern=T)
      })
    }
    benchd <- NULL
  
    if(verbose) {
      cat(length(jobres), 'files were found\n')
    }
    for(jobr in jobres) {
      if(length(jobr)) {
        if(verbose) 
          cat('Loading file ', jobr, '...\n')
        
        loadObj <- try(load(jobr))
        if(class(loadObj)!='try-error') {
          if(is.null(benchd)) {
            benchd <- b
          } else {
            benchd <- rbind(benchd, b)
          }
        } else {
          warning(paste("Couldn't read file", jobr, '.'))
        }
      }
    }
   
    benchd <- benchd[order(benchd$id)]
    setkey(benchd, id)
    
    if(is.null(benchdata)) {
      benchdata <- benchd[, list(id, eval(parse(text=fname)))]
    } else {
      if(key(benchdata)!='id') {
        warning('Setting key in supplied data.table to id.')
        setkey(benchdata, id)
      }
      if(nrow(benchd)==1) {
        benchdata[benchd, eval(parse(text=paste0(fname, ':=list(i.', fname,')')))]  
      } else {
        benchdata[benchd, eval(parse(text=paste0(fname, ':=i.', fname)))]
      }
      
    }
  }

  if(write.file & !is.null(table.file)) {
    assign(benchdata.name, benchdata)
    save(list=benchdata.name, file=table.file)
  }
    
  benchdata
}

#' @title Clean result-files from a previous benchmark.
#' @param f name of the function or a function object
#' @export
cleanBenchRes <- function(f) {
  if(is.character(f)) 
    fname <- f
  else
    fname <- as.character(match.call()$f)
  system(paste('rm job_', fname, '*.RData', sep=''), intern=T)
  system(paste('rm j_', fname, '*.R', sep=''), intern=T)
}

#' @title Get a list representing a line of a data.table
#' @param benchdata a data.table with a column called 'id' as key
#' @param i a single integer specifying the id of a line
#' @return a named list of objects found in the columns of benchdata at the 
#'   specified line.
#' 
#' @export
getLineAsList <- function(benchdata, i) {
  as.list(t(benchdata[id==i])[,1])
}
