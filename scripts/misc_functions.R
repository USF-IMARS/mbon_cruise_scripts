# ============================================================================ #
#
#
# ---- Misc Functions ----
#
#
# ============================================================================ #  



##%######################################################%##
#                                                          #
####               Get Sheet Information                ####
#                                                          #
##%######################################################%##
#' Get Sheet Information
#'
#' FUNCTION_DESCRIPTION
#'
#' @param files DESCRIPTION.
#'
#' @return RETURN_DESCRIPTION
#' @examples
#' # ADD_EXAMPLES_HERE
#' 
get_sheet_info <- function(files,
                           sheet_type = c("filter", "edna")
                           ) {
  
  
  sheet_type <- match.arg(sheet_type)
  
  recal <- tibble()
  
  for (i in seq(nrow(files))) {
    # select sheet name with `field_logsheet`
    cli::cli_alert_info("Getting sheet info for file: {.file {files$base[i]}}")
    temp_sht <- openxlsx::getSheetNames(files$file_path[i]) 
    
    sht_num  <- 
      switch(sheet_type,
        filter = which(str_detect(temp_sht, "field_")),
        edna   = which(str_detect(temp_sht, "(?i)edna") & 
                       !str_detect(temp_sht, "(?i)print"))
      )
    
    if (is_empty(sht_num)) {
      sht_num <- which(str_detect(temp_sht, "Sheet1")) 
    } 
    
    if (is_empty(sht_num)) {
      sht_num <- menu(temp_sht, 
                      title = glue(
                        "\n-----\n\n",
                        "Which sheet contains metadata?", 
                        "\n(0 for none)"))
    }
    
    # skip if no sheet name
    if (str_detect(sheet_type, "edna") 
        && sht_num == 0) {
      cli::cli_alert_danger("Files doesn't have an eDNA sheet. Skipping file!")
      recal <-
        bind_rows(
          recal,
          tibble(file_path = files$file_path[i]))
      next
    } else if (sht_num == 0) {
      recal <- 
        bind_rows(
          recal,
          tibble(file_path = files$file_path[i]))
      next
    }
      

    # read sheet 
    temp <-
      openxlsx::read.xlsx(
        xlsxFile      = files$file_path[i],
        sheet         = sht_num,
        rows          = 1:5,
        colNames      = FALSE,
        skipEmptyRows = FALSE,
        skipEmptyCols = FALSE
      ) %>%
      as_tibble()
    
    # if (str_detect(files$file_path[i], "OCT2018")) browser()
    
    # row number that contains headers
    row <- which(apply(temp, 1, function(x) any(grepl("(?i)sample", x))))

    if (is_empty(row) || is.na(row) || length(row) > 1) {
      View(temp)
      row <- readline("Which line is the header? ") %>%
        as.numeric()
    }
    
    # skip if no row info
    if (is_empty(row) || is.na(row)) {
      recal <-
        bind_rows(
          recal, 
          tibble(
            file_path = files$file_path[i], 
            sht_num   = sht_num))
      next
    }
    
    # get last column to read
    # either `notes` or `collector`
    last_c <- 
      grepl("(?i)notes|(?i)collector", temp[row,]) %>%
      which() %>%
      max()
    
    recal <- 
      bind_rows(
        recal,
        tibble(
          file_path = files$file_path[i],
          sht_num = sht_num,
          row     = row,
          last_c  = last_c
        )
      )
  }
  
  # joins recal and files
  files <- recal %>%
    drop_na(everything()) %>%
    left_join(files, by = "file_path")
  
  return(files)
  
  # ---- end of function
}


##%######################################################%##
#                                                          #
####         Read Cruise Logsheets and Convert          ####
#                                                          #
##%######################################################%##
#' Read Cruise Logsheets and Convert
#'
#' FUNCTION_DESCRIPTION
#'
#' @param .x File path
#' @param .y Sheet name
#' @param .z Rows to skip
#' @param .l Values that should be treated as `NA`
#' @param ship_acr The 2 letter ship acronym to match
#'
#' @return RETURN_DESCRIPTION
#'
#' @author Sebastian Di Geronimo (June 09, 2023)
#'
#' @examples
#' # ADD_EXAMPLES_HERE
#' 
read_logsheets <- function(
    .x, .y, .z, .l,
    sheet_type = c("filter", "edna"),
    na_skip = c("#N/A"),
    ship_acr = "FK|WS|SV|WB|H|CORE") {

  sheet_type <- match.arg(sheet_type)

  na_skip <- unlist(na_skip)

  # ---- load data
  # select sheet, skip number of lines to sample 1, stop columns after
  # `notes` or `collector`, remove NA values, clean names
  # temp <-
  #   openxlsx::read.xlsx(
  #     xlsxFile    = .x,
  #     sheet       = .y,
  #     cols        = 1:.l,
  #     startRow    = .z,
  #     na.strings  = na_skip,
  #     detectDates = TRUE
  #   ) %>%
  #     janitor::clean_names() #%>%
  temp <-
    openxlsx2::read_xlsx(
      file        = .x,
      sheet       = .y,
      cols        = 1:.l,
      startRow    = .z,
      na.strings  = na_skip,
      detectDates = FALSE
    ) %>%
    janitor::clean_names() %>%
    # ---- fix time
    # time_gmt and time_sampled_24_00 to sample_collection_time_gmt
    # fix sample_id to identifier
    rename(
      any_of(
        c(
          sample_collection_time_gmt = "time_gmt",
          sample_collection_time_gmt = "sample_collection_time_hh_mm",
          sample_collection_time_gmt = "sample_collection_time",
          # TODO: need to correct to gmt ----
          sample_collection_time_gmt = "sample_collection_time_edt",
          sample_collection_time_gmt = "time_sampled_24_00",
          date_mm_dd_yy = "mm_dd_yy",
          identifier = "sample_id",
          cruises    = "cruise_id"
        )
      )
    ) %>%
    mutate(
      date_mm_dd_yy = if_else(date_mm_dd_yy == 0, NA, date_mm_dd_yy),
      sample_collection_time_gmt = if_else(
        sample_collection_time_gmt == 0, 
        NA, 
        sample_collection_time_gmt)
    )

  # make sure the identifiers contain: FK, WS, SV or WB
  if (!str_detect(.x, "OCT2018")) {
    temp <-
      temp %>%
      filter(
        if_any(
          matches(
            c("identifier", "sample")
          ),
          ~ str_detect(.x, ship_acr)
        )
      )
  } else if (str_detect(.x, "OCT2018")) {
    temp <- temp %>%
      filter(!is.na(vol_ml))
  }
  
  # filter({if("identifier" %in% names(.)) {
  #   str_detect(identifier, ship_acr)
  #   } else { . }})
  # filter(str_detect(identifier, ship_acr)) #%>%

  # if vol_mol contains - or ~, remove
  temp <-
    temp %>%
    mutate(
      vol_ml    = replace(vol_ml, str_detect(vol_ml, "-"), NA),
      vol_ml    = str_replace_all(vol_ml, "~", "")
    ) %>%
    # try to convert to `numeric`
    suppressMessages(type_convert(.)) %>%
    # make sure notes are strings
    mutate(
      notes = as.character(notes)
    )

  # ---- fix issues related to few sheets
  # fix vol_ml if contains `&` to add two values
  if (typeof(temp$vol_ml) == "character") {
    temp <- temp %>%
      mutate(
        vol_ml = (str_split(vol_ml, " & ")),
        vol_ml = map(vol_ml, ~ sum(as.numeric(.x)))
      ) %>%
      unnest(vol_ml) %>%
      suppressMessages(type_convert(.))
  }

  # fix depth_m to remove any `*` or `..`, then convert to num
  if (typeof(temp$depth_m) == "character") {
    temp <- temp %>%
      mutate(
        depth_m = str_remove(depth_m, "(\\*)+"),
        depth_m = str_replace(depth_m, "\\.\\.", "\\.")
      ) %>%
      suppressMessages(type_convert(.))
  }

  # fix max_depth to replace `flow through` to 0
  if ("max_depth" %in% names(temp) &&
    typeof(temp$max_depth) == "character") {
    temp <- mutate(temp,
      max_depth = str_replace(max_depth, "flo.*", "0")
    ) %>%
      suppressMessages(type_convert(.))
  }

  # fix date_mm_dd_yy when excel date is non-numeric
  # i.e. error 08//01/2015 - encoded as string, all other values are
  # read as strings (i.e. `"48025"` as string instead of `48025` as num)
  if (
    "date_mm_dd_yy" %in% names(temp) 
    && (typeof(temp$date_mm_dd_yy) == "character" 
        | is.numeric(temp$date_mm_dd_yy)
        )
    ) {
    temp <- temp %>%
      mutate(
        date_mm_dd_yy = str_replace(date_mm_dd_yy, "//", "/"),
        # date_mm_dd_yy = if_else(date_mm_dd_yy == 0, NA, date_mm_dd_yy),
        date_mm_dd_yy = map_chr(
          date_mm_dd_yy,
          function(.x) {
            if (!is.na(as.numeric(.x))) {
              times <- as.numeric(.x) %>%
                janitor::excel_numeric_to_date()
            } else {
              # convert strings "8/1/2015" to date
              times <- anytime::anydate(.x)
            }
            return(as.character(times))
          }
        ),

        # converts to correct format
        date_mm_dd_yy = suppressWarnings(as.POSIXct(date_mm_dd_yy))
      )
  }

  if (any(str_detect(names(temp), "sample_number"))) {
    temp <-
      mutate(temp, sample_number = as.character(sample_number))
  }
  
  # add date_time,
  # first, converting excel date_time to HMS
  # i.e. 1899-01-01 12:15:34 UTC to 12:15:13
  # then, add date and time
  if (str_detect(sheet_type, "filter")) {
    temp <-
      temp %>%
      # fix date when entered as mm:dd:yy instead of mm/dd/yyyy
      {
        if (nrow(filter(., date_mm_dd_yy > as_date("2015-01-01"))) < 1) {
          mutate(.,
            date_mm_dd_yy = as.character(date_mm_dd_yy),
            date_mm_dd_yy = str_replace(date_mm_dd_yy, ".* ", ""),
            date_mm_dd_yy = str_replace_all(date_mm_dd_yy, ":", "/"),
            date_mm_dd_yy = str_replace_all(date_mm_dd_yy, "(.*/.*/)", "\\120"),
            # date_mm_dd_yy = anytime::anydate(date_mm_dd_yy),
            .before = 1
          )
        } else {
          .
        }
      } %>% 
      {
        if (all(class(temp$sample_collection_time_gmt) != "POSIXct")) {
          mutate(.,
            sample_collection_time_gmt = sample_collection_time_gmt * 86400,
            sample_collection_time_gmt = round(sample_collection_time_gmt),
            sample_collection_time_gmt = hms::as_hms(sample_collection_time_gmt),
            
            date_time = ymd(date_mm_dd_yy) + lubridate::hms(sample_collection_time_gmt),
            .after = sample_collection_time_gmt
          )
        } else {
          mutate(.,
            sample_collection_time_gmt = hms::as_hms(sample_collection_time_gmt),
            date_time = ymd(date_mm_dd_yy) + lubridate::hms(sample_collection_time_gmt),
            .after = sample_collection_time_gmt
          )
        }
      } %>%
      # remover extract column if exists
      select(-any_of("time_filtered_24_00")) %>%
      mutate(
        station = str_remove(station, "~"),
        station = str_replace_all(station, " ", ""),
        station = str_to_upper(station),
        station = case_when(
          str_detect(station, "9.69999") ~ "9.7",
          str_detect(station, "9.80000") ~ "9.8",
          .default = station
        )
      )
  } else {
    
    
    temp <-  mutate(temp,
                    sample_collection_time_gmt = sample_collection_time_gmt * 86400,
                    sample_collection_time_gmt = round(sample_collection_time_gmt),
                    sample_collection_time_gmt = hms::as_hms(sample_collection_time_gmt),
                    
                    date_time = ymd(date_mm_dd_yy) + lubridate::hms(sample_collection_time_gmt),
                    .after = sample_collection_time_gmt
    )  %>%
    rename("time_gmt" = sample_collection_time_gmt,
           "mm_dd_yy" = date_mm_dd_yy)
  }


  # if (!nrow(temp) > 1) temp <- NULL
  return(temp)

  # ---- end of function
}


##%######################################################%##
#                                                          #
####      Search Metadata Directories for Folders       ####
#                                                          #
##%######################################################%##
#' Search Metadata Directories for Folders
#'
#' FUNCTION_DESCRIPTION
#'
#' @param .dir_path Starting directory path.
#' @param .folder_search Subfolders exists within every level
#'                    Can be:
#'                    - a string (ex `"dir1"`),
#'                    - a vector (ex `c("dir1", "dir2")`), or 
#'                    - a list (ex `list("dir1", "dir2")`)
#' @param return_type Return type, either `vector` or `tibble`
#' @param recurse_level How many levels down a folder subdirectory should be 
#'                      examined. (NOTE: limit to lowest number as possible as 
#'                      more levels could create a runtime problem)
#' @param .type Type of search 
#'        - default: "directory"
#'        - others from `fs::dir_ls()`
#'
#' @author Sebastian Di Geronimo (May 2023)
#'
#' @return RETURN_DESCRIPTION
#' @examples
#' # ADD_EXAMPLES_HERE
#' 
search_meta_folders <- function(
    .dir_path,
    .folder_search = NULL,
    return_type    = c("vector", "tibble"),
    recurse_level  = FALSE,
    .type = "directory") {
  
  return_type <- match.arg(return_type)
  
  # reformat to "*<subfolder_1>|*<subfolder_2>
  if (all(!is.na(.folder_search)) && !is.null(.folder_search)) {
    
    message(
      paste("Searching for sub-folder(s):",
            paste(c("\b", .folder_search), 
                  collapse = "\n"), "\n"))
    
    .folder_search <- 
      as.character(.folder_search) %>%
      paste0("*", ., collapse = "|")
  } else if (is.null(.folder_search)) {
      message("Not searching for subdirectory.")
  } else {
    stop("`.folder_search` is not of type list, or character, or is NULL",
         call. = FALSE)
    }
  
  file_loc <-
    here::here(.dir_path) %>%
    fs::dir_ls(
      type = .type,
      glob = .folder_search,
      fail = FALSE,
      recurse = recurse_level
      ) 
  
  if (purrr::is_empty(file_loc)) {
    warning("No directories were found.")
  }
  
  switch(return_type,
    vector = file_loc,
    tibble = tibble::tibble(folder = file_loc) 
  )
  
  # ---- end of function
}

##%######################################################%##
#                                                          #
####             Most Recently Created File             ####
#                                                          #
##%######################################################%##
#' Most Recently Created File
#'
#' This function should be used after an `fs::dir_ls` search with a specific 
#' file that may have multiple versions. When there are multiple matches to 
#' a search, this will take the most recent version of it.
#'
#' @param fpath The fs_path object created from `fs::dir_ls`
#' @param check Optionally check the most recent file. This can be set to 
#' either `TRUE` or `FALSE`.
#'
#' @return A vector of the most recent created file as `fs_path` object
#' @examples
#' # NA
#' 
last_mod <-  function(fpath, check = TRUE) {
  
  if (!check) return(fpath)
  
  ftime <- file.mtime(fpath) 
  
  return(fpath[which.max(ftime)]) 
  
  # ---- end of function
}

##%######################################################%##
#                                                          #
####      Base File Name and File Name Expression       ####
#                                                          #
##%######################################################%##
#' Base File Name and File Name Expression
#'
#' This function take a location to save file and a base name to 
#' search for in either locally or the cloud
#'
#' @param loc Location to save file
#' @param file_base Base name of file without a suffix or extension
#' @param exts Extension to save file.
#' @param time_stamp_fmt Time stamp format as the suffix to the base file name. 
#'                       - default = YYYYMMDD_HHMMSS (i.e "%Y%m%d_%H%M%S")
#'                       - if no suffix, `NULL`
#'                       - if want custom, `<custom_message>`
#'                       - if help, will give a table of formats with examples
#'
#' @return Returns a list of three:
#'          - file location  = file_loc
#'          - base file name = file_base,
#'          - file expression to be evaluated = file_expr
#'          
#' @details
#' The time stamp can be formated based on 
#' Code	Meaning	Code	Meaning
#'      %a - Abbreviated weekday	
#'      %A - Full weekday
#'      %b - Abbreviated month	
#'      %B - Full month
#'      %c - Locale-specific date and time	
#'      %d - Decimal date
#'      %H - Decimal hours (24 hour)	
#'      %I - Decimal hours (12 hour)
#'      %j - Decimal day of the year	
#'      %m - Decimal month
#'      %M - Decimal minute	
#'      %p - Locale-specific AM/PM
#'      %S - Decimal second	
#'      %U - Decimal week of the year (starting on Sunday)
#'      %w - Decimal Weekday (0=Sunday)	
#'      %W - Decimal week of the year (starting on Monday)
#'      %x - Locale-specific Date	
#'      %X - Locale-specific Time
#'      %y - 2-digit year	
#'      %Y - 4-digit year
#'      %z - Offset from GMT	
#'      %Z - Time zone (character)
#'
#' @author Sebastian Di Geronimo (June, 2023)
#'          
#' @examples
#' # ADD_EXAMPLES_HERE
#' 
file_expr <- function(loc            = NULL,
                      file_base      = NULL,
                      exts           = "csv",
                      time_stamp_fmt = "%Y%m%d_%H%M%S") {
  
  # ---- help for deciding time stamp format
  if (!is.null(time_stamp_fmt) && str_detect(time_stamp_fmt, "help")) {
    return(tribble(
      ~code, ~meaning,
      "%a",  "Abbreviated weekday", 
      "%A",  "Full weekday",
      "%b",  "Abbreviated month", 
      "%B",  "Full month", 
      "%c",  "Locale-specific date and time", 
      "%d",  "Decimal date", 
      "%H",  "Decimal hours (24 hour)", 
      "%I",  "Decimal hours (12 hour)", 
      "%j",  "Decimal day of the year",	
      "%m",  "Decimal month", 
      "%M",  "Decimal minute",	
      "%p",  "Locale-specific AM/PM", 
      "%S",  "Decimal second", 
      "%U",  "Decimal week of the year (starting on Sunday)", 
      "%w",  "Decimal Weekday (0=Sunday)", 
      "%W",  "Decimal week of the year (starting on Monday)", 
      "%x",  "Locale-specific Date", 
      "%X",  "Locale-specific Time", 
      "%y",  "2-digit year", 
      "%Y",  "4-digit year", 
      "%z",  "Offset from GMT",	
      "%Z",  "Time zone (character)", 
    ) %>%
      mutate(
        example = format(ymd_hms("2000-01-01 02:11:51"), code),
        example = sprintf("%s == `%s`", "2000-01-01 02:11:51", example)
      ))
  }
  
  # catch time stamp format if NULL  
  time_stamp_fmt <-
    tryCatch(
      {
        glue("_", format(Sys.time(), time_stamp_fmt))
        expr(glue("_", format(Sys.time(), !!time_stamp_fmt)))
      },
      error = function(e) {
        NULL
      }
    )
  
  # add period to extension
  exts <- glue(".{exts}")
  
  file_expr <-
    # create expression for the file name
    expr(
      here::here(
        !!loc,
        glue(
          !!file_base,
          !!time_stamp_fmt,
          !!exts,
          .null = ""
        )
      )
    )
  
  list(
    file_base = file_base,
    file_expr = file_expr
  )
  
  # ---- end of function
}

##%######################################################%##
#                                                          #
####                   Save .csv File                   ####
#                                                          #
##%######################################################%##
#' Save .csv File
#'
#' FUNCTION_DESCRIPTION
#'
#' @param .data The data.frame or tibble to be saved.
#' @param save_location Folder to save file. Will create folder if doesn't exist.
#' @param save_name Prefix name of file. Will be saved with `_<timestamp>.csv`
#' @param overwrite `TRUE` or `FALSE` to re-save file if exists, or keep current. 
#' @param verbose `TRUE` or `FALSE` to print location in script.
#' @param time_stamp_fmt Time stamp format as the suffix to the base file name.
#'                       - default = YYYYMMDD_HHMMSS (i.e "%Y%m%d_%H%M%S")
#'                       - if no suffix, `NULL`
#'                       - if want custom, `<custom_message>`
#'
#' @return NULL, save file
#'
#' @author Sebastian Di Geronimo (June 02, 2023)
#'
#' @examples
#' # ADD_EXAMPLES_HERE
#' 
save_csv <- function(
    .data         = NULL,        
    save_location = NULL,
    save_name     = NULL,
    overwrite     = FALSE,
    verbose       = TRUE,
    time_stamp_fmt = "%Y%m%d_%H%M%S") {
  
  library("cli")
  
  # ---- checking input parameters
  if (is.null(.data)) {
    cli::cli_abort(
      c("Check input, {.var {col_yellow(\".data\")}} is `NULL`.", 
        "{col_red(\"Stopping\")}"))
  }
  if (is.null(save_location)) {
    cli::cli_abort(
      c("Check input, {.var {col_yellow(\"save_location\")}} is `NULL`.",
        "{col_red(\"Stopping\")}"))
  }
  if (is.null(save_name)) {
    cli::cli_abort(
      c("Check input, {.var {col_yellow(\"save_name\")}} is `NULL`.",
        "{col_red(\"Stopping\")}"))
  }
  if (is.null(overwrite)) {
    cli::cli_abort(
      c("Check input, {.var {col_yellow(\"overwrite\")}} is `NULL`.",
        "{col_red(\"Stopping\")}"))
  }
  
  # ---- file name
  data_f <- 
    file_expr(
      save_location,
      save_name,
      exts = "csv",
      time_stamp_fmt
    )
  
  data_f <- eval(data_f$file_expr)
  
  if (verbose) {
    cli::cli_h1("{save_name}")
    cli::cli_alert_info(
      c("File Information:\n",
        "Rows:      {nrow(.data)}\n",
        "Columns:   {ncol(.data)}\n",
        "Location:  {.file {save_location}}\n",
        "File Name: {.file {basename(data_f)}}")
    )
  }
  # ---- check if folder exists and create otherwise
  if (!dir_exists(save_location)) {
    
    if (verbose) 
      cli::cli_alert_info(
        "{cli::col_green(\"Creating\")} folder location!")
    
    fs::dir_create(save_location)
  }
  
  # ---- check if need to create file
  file_loc <- 
    fs::dir_ls(
      save_location,
      regexp = save_name
    ) 
  
  create_f <- rlang::is_empty(file_loc)
  
  if (!create_f && !overwrite) {
    # return early if no need to create
    if (verbose)
      cli::cli_alert_info(
        "File exist and {.emph is not} being {col_green(\"overwritten\")}!\n"
      )
    return(invisible())
  }
  
  if (!create_f && overwrite && verbose) {
    cli::cli_alert_info(
      "File exist and {.emph is} being {col_red(\"overwritten\")}!")
  } else if (create_f && verbose) {
    cli::cli_alert_info("File does not exists, {col_green(\"creating\")}!")
  }
  
  
  # ---- saving file
  if (verbose) cli::cli_alert_info("Saving file!")
  
  readr::write_csv(
    x    = .data,
    file = data_f,
    na   = ""
  )
  
  if (verbose) cli::cli_alert_success("Saved!\n\n")
  
  # ---- end of function
}


##%######################################################%##
#                                                          #
####   Plot Cruise Dates with Expected Range of Dates   ####
#                                                          #
##%######################################################%##
#' Plot Cruise Dates with Expected Range of Dates
#'
#' FUNCTION_DESCRIPTION
#'
#' @param cruise_title Cruise ID
#' @param data Date to search
#' @param .cruise_event data.frame or tibble with 3 columns:
#'                      - `cruise` containing cruise IDs
#'                      - `start_date` containing the start date of cruise
#'                      - `end_date` containing the end date of cruise
#'
#' @return RETURN_DESCRIPTION
#' @examples
#' # ADD_EXAMPLES_HERE
plot_cruise_dates <- function(cruise_title, data, .cruise_event) {
  
  
  dates <- filter(.cruise_event, cruise %in% cruise_title)
  
  limit <- list(
    min = min(c(dates$start_date, data$date), na.rm = TRUE) - days(1),
    max = max(c(dates$end_date, data$date), na.rm = TRUE) + days(1)
  )
  
  if (any(dates$start_date > data$date)) {
    station <- 
      data$station[which(data$date < dates$start_date)] %>%
      str_c(collapse = ", ")
    warning(paste(
      "-------\n", "Check this cruise ID:", cruise_title,
      "\nFor stations:", station, "\n-------\n\n"
    ))
    cruise_title <- glue("WARNING FOR\n{cruise_title}:\n",
                         "Stations: {station}")
  }
  
  ggplot(
    data = data,
    aes(x = date, y = station, color = station)
  ) +
    geom_rect(
      data = dates,
      aes(
        xmin = start_date,
        xmax = end_date,
        ymin = Inf,
        ymax = -Inf
      ),
      fill = "grey70",
      color = "black",
      alpha = 0.5,
      show.legend = FALSE,
      inherit.aes = FALSE
    ) +
    geom_point(
      size = 2,
      show.legend = FALSE
    ) +
    labs(
      title = cruise_title,
      x = NULL,
      y = "Station"
    ) +
    scale_x_datetime(
      limits = c(
        limit$min,
        limit$max
      ),
      date_breaks = "1 days",
      date_labels = "%d %b %Y"
    ) +
    ggthemes::theme_calc() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  
    # ---- end of function
}



##%######################################################%##
#                                                          #
####      Setup Cruise Directory and Blank Files        ####
#                                                          #
##%######################################################%##
#' Setup Cruise Directory and Blank Files
#'
#' @param cruise_location Location to create cruise directory and
#'                        sub-directories
#' @param cruise_id Cruise ID
#' @param cruise_date Start date of cruise as `date`
#' @param blank_location Location of blank files
#' @param sub_dir Sub-directories to create:
#'                - "metadata" for logsheets 
#'                - "forms" for paperwork
#'                - "CDOM" for CDOM data
#'                - "apad" for filter pad data
#'                - "bb3" for BB3 data
#'                
#' @param volunteer `logical` if need volunteer paperwork
#'
#' @return RETURN_DESCRIPTION
#' @examples
#' # ADD_EXAMPLES_HERE
#' 
cruise_setup <- function(
    cruise_location,
    cruise_id,
    cruise_date,
    blank_location,
    sub_dir = c("metadata", "forms", "CDOM", "apad", "bb3"),
    volunteer = FALSE,
    map_file = NULL,
    return_path = FALSE) {
  
  # ---- Find blank files
  if (!dir_exists(blank_location)) {
    cli::cli_abort(
      c("{.var blank_location} {.emph doesn't exists}. Fix path and try again!",
        "{.var blank_location} = {.path {blank_location}}"))
  }
  
  blank_files <- 
    blank_location %>%
    dir_ls(type = "file") %>%
    str_subset("~|ignore", negate = TRUE)
  
  # ---- Create cruise folders
  cruise_dir <- 
    here(
      cruise_location,
      year(cruise_date),
      cruise_id)
  
  cli::cli_h1("Cruise ID: {cruise_id}")
  
  if (any(!dir_exists(here(cruise_dir, sub_dir)))) {
    cli::cli_alert_info("Creating Sub-directories: {.var {sub_dir}}")
    dir_create(here(cruise_dir, sub_dir))
    
    fs::dir_tree(cruise_dir)
  } else {
   cli::cli_alert_info("No new directories created!") 
   fs::dir_tree(cruise_dir)
  }
  
  
  # ---- Copying Blank Logsheet ---- #
  
  # blank name
  blk_logsheet_file <- str_subset(blank_files, "sample_logsheet")
  
  # new name
  logsheet_file <- 
    here(
      cruise_dir, "metadata",
      glue(
        "fknms_sample_logsheet_",
        "{sprintf(\"%.2d\", month(cruise_date))}_",
        "{year(cruise_date)}_{cruise_id}.xlsx"
        ) 
    )
  
  if (!file_exists(logsheet_file)) {
    cli::cli_alert_info(
      c("Copying blank logsheet file:\n",
        "Location:  {.path {dirname(logsheet_file)}}\n",
        "File Name: {.path {basename(logsheet_file)}}\n\n")
    )
    
    file_copy(
      blk_logsheet_file,
      logsheet_file
    )
    
  } else {
    cli::cli_alert_info(
      c("Logsheet exists:\n",
        "Location:  {.path {dirname(logsheet_file)}}\n",
        "File Name: {.path {basename(logsheet_file)}}\n\n")
    )
  }
  
  # ---- Copying Blank USF Self-insurance ---- #
  # blank name
  blk_slf_insur_file <- str_subset(blank_files, "USF_letter_of_self_insurance")
  
  slf_insur_file <- 
    here(
      cruise_dir, "forms",
      glue("USF_letter_of_self_insurance_WS_",
           "{year(cruise_date)}_",
           "{month(cruise_date, label = TRUE, abbr = FALSE)}_",
           "LAST_NAME_PARTICIPANTS.pdf")
    )
  
  slf_insur_exists <- dir_ls(here(cruise_dir, "forms"), 
                             regexp = "USF_letter_of_self_insurance_WS")
  
  if (is_empty(slf_insur_exists)) {
    cli::cli_alert_info(
      c("Copying blank self-insurance file:\n",
        "Location:  {.path {dirname(slf_insur_file)}}\n",
        "File Name: {.path {basename(slf_insur_file)}}\n", 
        "{.strong Remember to edit {.var LAST_NAME_PARTICIPANTS}}\n\n")
    )
    
    file_copy(
      blk_slf_insur_file,
      slf_insur_file
    )
    
  } else {
    cli::cli_alert_info(
      c("These self-insurance file(s) exist:\n",
        "Location:  {.path {dirname(slf_insur_exists)}}\n",
        "File Name(s): {.path {basename(slf_insur_exists)}}\n\n")
    )
    }
  
  # ---- Volunteer Paperwork ---- #
  if (volunteer) {
    cli::cli_alert_warning("Volunteer paperwork is requested!")
    
    blk_volunteer <- str_subset(blank_files, "(?i)volunteer_")
    
    volunteer_file <- 
      basename(blk_volunteer) %>%
      str_replace( 
       "mm\\.dd\\.(yyyy|year)_name",
       format(cruise_date, "%b_%d_%Y_LAST_NAME")
       ) %>%
      here(cruise_dir, "forms", .)
    
    vol_paper <- 
      dir_ls(here(cruise_dir, "forms"), 
             regexp = "(?i)volunteer")
    
    if (is_empty(vol_paper)) {
      cli::cli_alert_info(
        c("Copying Volunteer paperwork:\n",
          "Location:  {.path {dirname(volunteer_file)[1]}}\n",
          "File Name: {.path {basename(volunteer_file)}}\n", 
          "{.strong Remember to edit {.var LAST_NAME}}\n\n")
      )
      
      file_copy(
        blk_volunteer,
        volunteer_file
      )
      
    } else {
      cli::cli_alert_info(
        c("These volunteer paperwork file(s) exist:\n",
          "Location:  {.path {dirname(vol_paper)[1]}}\n",
          "File Name(s): {.path {basename(vol_paper)}}\n\n")
      )
    }
  }
  
  
  # ---- Copy Map to Directory ---- #
  if (is.null(map_file)) {
    cli::cli_alert_info("No map file given.\n\n")
  } else {
    if (file_exists(here(cruise_dir, "metadata", basename(map_file)))) {
      cli::cli_alert_info(
        c("Map exists in:\n",
          "Location:  {.path {here(cruise_dir, \"metadata\")}}\n",
          "File Name: {.path {basename(map_file)}}\n\n")
      )
    } else {
      cli::cli_alert_info(
        c("Copying Sampling Map:\n",
          "Location:  {.path {dirname(map_file)}}\n",
          "File Name: {.path {basename(map_file)}}\n\n")
      )
      
      file_copy(
        map_file,
        here(cruise_dir, "metadata")
      )
    }
  }
  
  # ---- Copying sample_id_creation_blank ---- #

  # blank name
  blk_logsheet_file <- 
  blk_sample_id <- 
    here(blank_location, "..") %>%
    dir_ls(type = "file") %>%
    str_subset("sample_id_creation_blank")
  
  # new name
  new_sample_id <- 
    basename(blk_sample_id) %>%
    str_replace("blank", cruise_id) %>%
    here(cruise_dir, "metadata", .)
  
  
  if (!file_exists(new_sample_id)) {
    cli::cli_alert_info(
      c("Copying sample ID file:\n",
        "Location:  {.path {dirname(blk_sample_id)}}\n",
        "File Name: {.path {basename(blk_sample_id)}}\n\n")
    )
    
    file_copy(
      blk_sample_id,
      new_sample_id
    )
    
  } else {
    cli::cli_alert_info(
      c("Sample ID file exists:\n",
        "Location:  {.path {dirname(blk_sample_id)}}\n",
        "File Name: {.path {basename(blk_sample_id)}}\n\n")
    )
  }
  
  
  if (return_path) return(cruise_dir)
  
  # ---- end of function ---- #
}
