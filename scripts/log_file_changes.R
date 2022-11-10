library("log4r")

# ---- creates/loads log file ----
startup <- function(my_logfile = "/data_change_logfile.txt"){
  
  root       <- rprojroot::find_rstudio_root_file() 
  
  if (!is.null(match.call()$my_logfile)) {
    my_logfile <- paste0(root,"/", my_logfile, ".txt") 
  } else {
    my_logfile <- paste0(root, my_logfile)
  }
  
  # ---- internal function for custom layout ----
  custom_layout <- function(time_format = "%Y-%m-%d %H:%M:%S") {
  stopifnot(is.character(time_format))
  log4r:::verify_time_format(time_format)
  function(level, ...) {
    msg <- paste0(..., collapse = "")
    sprintf("[%s]\t %-5s\t%s\n", log4r:::fmt_current_time(time_format), level, 
            msg)
    }
  }
  
  # ---- creates log file if doesn't exist in root ----
  if (!file.exists(my_logfile)) {
    cat(sprintf("\nCreating Log File: %s\n\n", basename(my_logfile)))
    cat(sprintf('%-22s\t%-5s\t%14s\t%14s\t%14s\t%14s\t%s', 
             "Time", "Type", "File_Name", "Sheet", "Cells", "User", "Message"),
        file = my_logfile, sep = "\n")
   
  } else {
    cat(sprintf("\nUsing Log File: %s\n\n", basename(my_logfile)))
  }
  
  # ---- output style to console ----
  # my_console_appender = console_appender(layout = default_log_layout())
  my_console_appender <-  console_appender(layout = custom_layout())
  
  # ---- output style to file ----
  my_file_appender <-  file_appender(my_logfile, 
                                   append = T, 
                                   layout = custom_layout())
  
  # ---- options for logger, used when writing to file ----
  log_options <<- invisible(
    log4r::logger(
      threshold = "INFO",
      appenders = list(my_console_appender, my_file_appender)
      )
  )
}

# ---- change between logs ----
chg_log <- function(log_name) {
  root     <- rprojroot::find_rstudio_root_file() 
  log_file <- paste0(root,"/", log_name, ".txt") 
  
  # checks if log files exists
  stopifnot("This log may not exist yet!" = file.exists(log_file))
  
  # changes log
  startup(log_name)
}

# ---- shows current log ----
current_log <- function() {
  
  stopifnot("Need to run `startup()` before checking log name" = 
              exists("log_options"))
  
  log_name <- environment(log_options[["appenders"]][[2]])[["file"]]
  cat(sprintf("\nEditing Log File: %s\n\n", basename(log_name)))
  }

# ---- write to log ----
log4r_info <- function(verbose = TRUE) {
  # on.exit(message("Exiting log"))
  if (verbose) current_log()
  
  tryCatch({
    file   <- readline("What file did you change? ")
    sheet  <- readline("What sheet did you change (number of name)? ")
    loc    <- readline("Which cells? (i.e. A12, or A13, A31, A1:B31) ")
    name   <- readline("Who made this change? ")
    change <- readline("What did you change? ")
    info   <- sprintf('%14s\t%14s\t%14s\t%14s\t%s', file, sheet, loc, name, change)
    log4r::info(log_options, info)
  },
  interrupt = function(e){
    message("Action Interrupted! If this was a mistake, run again")
  })
  
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