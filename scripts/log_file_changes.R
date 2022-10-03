library("log4r")

startup <- function(my_logfile = "/data_change_logfile.txt"){
  
  root       <- rprojroot::find_rstudio_root_file() 
  
  if (!is.null(match.call()$my_logfile)) {
    my_logfile <- paste0(root,"/", my_logfile, ".txt") 
  } else {
    my_logfile <- paste0(root, my_logfile)
  }
  
  # when logfile name is null, will prompt a name
  # if (is.null(my_logfile)) {
  #   file_name <- readline("Name log file (it will have .txt)")
  #   my_logfile <- paste0(root,"/", file_name, "txt") 
  # } else {
  #   # defaults to 
  #   my_logfile <- paste0(root, my_logfile)
  # }
  
  custom_layout <- function(time_format = "%Y-%m-%d %H:%M:%S") {
  stopifnot(is.character(time_format))
  log4r:::verify_time_format(time_format)
  function(level, ...) {
    msg <- paste0(..., collapse = "")
    sprintf("[%s]\t %-5s\t%s\n", log4r:::fmt_current_time(time_format), level, 
            msg)
    }
  }
  
  if (!file.exists(my_logfile)) {
    cat(sprintf("\nCreated Log File: %s\n\n", basename(my_logfile)))
    cat(sprintf('%-22s\t%-5s\t%14s\t%14s\t%14s\t%14s\t%s', 
             "Time", "Type", "File_Name", "Sheet", "Cells", "Name", "Message"),
        file = my_logfile, sep = "\n")
   
  } else {
    cat(sprintf("\nLog File: %s\n\n", basename(my_logfile)))
  }
  
  # my_console_appender = console_appender(layout = default_log_layout())
  my_console_appender <-  console_appender(layout = custom_layout())
  
  
  my_file_appender <-  file_appender(my_logfile, 
                                   append = T, 
                                   layout = custom_layout())
  
  
  my_logger <<- invisible(log4r::logger(
    threshold = "INFO",
    appenders = list(my_console_appender, my_file_appender)
  ))
   # return(my_logger)
}


# my_logger <- startup()

# TODO: add function to change between logs?
chg_log <- function(log_name) {
  root     <- rprojroot::find_rstudio_root_file() 
  log_file <- paste0(root,"/", log_name, ".txt") 
  
  # checks if log files exists
  stopifnot("This log may not exist yet!" = file.exists(log_file))
  
  # changes log
  startup(log_name)
}
# stopifnot("fil" = file.exists(log_file))

# log_name ="data_change_logfile"

# shows current log
current_log <- function() {
  stopifnot("Need to startup log first" = exists("my_logger"))
  environment(my_logger[["appenders"]][[2]])[["file"]]
  }

log4r_info <- function() {
  file   <- readline("What file did you change? ")
  sheet  <- readline("What sheet did you change (number of name)? ")
  loc    <- readline("Which cells? (i.e. A12, or A13, A31, A1:B31) ")
  name   <- readline("Who made this change? ")
  change <- readline("What did you change? ")
  info   <- sprintf('%14s\t%14s\t%14s\t%14s\t%s', file, sheet, loc, name, change)
  log4r::info(my_logger, info)
}


# ---- how to use ----
# log4r_info()
# WS234983
# fieldlog
# A12
# Seb
# changed time and datae
# 
# 
# 
# test <- read.delim("data_change_logfile.txt")
# View(test)