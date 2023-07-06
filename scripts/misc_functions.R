# ============================================================================ #
# ---- Misc Functions ----
# ============================================================================ #  


get_sheet_info <- function(files) {
  recal <- tibble()
  
  for (i in seq(nrow(files))) {
    # select sheet name with `field_logsheet`
    cli::cli_alert_info("Getting sheet info for file: {.file {files$base[i]}}")
    temp_sht <- openxlsx::getSheetNames(files$file_path[i]) 
    sht_num  <- which(str_detect(temp_sht, "field_"))
    
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
    if (sht_num == 0) {
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
    
    # row number that contains headers
    row <- which(apply(temp, 1, function(x) any(grepl("(?i)sample", x))))
    
    if (is_empty(row) || is.na(row)) {
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
    last_c <- which(grepl("(?i)notes|(?i)collector", temp[row,])) %>%
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
read_logsheets <-  function(.x, .y, .z, .l,
                            ship_acr = "FK|WS|SV|WB|H") {

  # ---- load data
  # select sheet, skip number of lines to sample 1, stop columns after
  # `notes` or `collector`, remove NA values, clean names
  temp <- 
    openxlsx::read.xlsx(
      xlsxFile    = .x,
      sheet       = .y,
      cols        = 1:.l,
      startRow    = .z,
      na.strings  = na_skip,
      detectDates = TRUE
    ) %>%
      janitor::clean_names() %>%
    
    # ---- fix time
    # time_gmt and time_sampled_24_00 to sample_collection_time_gmt
    # fix sample_id to identifier
    rename(any_of(
      c(sample_collection_time_gmt = "time_gmt",
        sample_collection_time_gmt = "sample_collection_time_hh_mm",
        # TODO: need to correct to gmt ----
        sample_collection_time_gmt = "sample_collection_time_edt", 
        sample_collection_time_gmt = "time_sampled_24_00",
        identifier = "sample_id")))  %>%
    
    # make sure the identifiers contain: FK, WS, SV or WB
    filter(str_detect(identifier, ship_acr)) %>%
    
    # if vol_mol contains - or ~, remove 
    mutate(
      vol_ml    = replace(vol_ml,  str_detect(vol_ml, "-"), NA),
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
        vol_ml = map(vol_ml, ~ sum(as.numeric(.x)))) %>%
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
  if (typeof(temp$max_depth) == "character") {
    temp <- mutate(temp,
                   max_depth = str_replace(max_depth, "flo.*", "0")) %>%
      suppressMessages(type_convert(.))
  }
  
  # fix date_mm_dd_yy when excel date is non-numeric
  # i.e. error 08//01/2015 - encoded as string, all other values are 
  # read as strings (i.e. `"48025"` as string instead of `48025` as num)
  if (typeof(temp$date_mm_dd_yy) == "character") {
    temp <- temp %>%
      mutate(
        date_mm_dd_yy = str_replace(date_mm_dd_yy, "//", "/"),
        date_mm_dd_yy = map_chr(date_mm_dd_yy,
                                function(.x) {
                                  if (!is.na(as.numeric(.x))) {
                                    times <- as.numeric(.x) %>%
                                      janitor::excel_numeric_to_date()
                                  } else {
                                    # convert strings "8/1/2015" to date 
                                    times <- anytime::anydate(.x)
                                  }
                                  return(as.character(times))
                                }),
        
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
  temp <- 
    temp %>%
    # fix date when entered as mm:dd:yy instead of mm/dd/yyyy
    {if (nrow(filter(., date_mm_dd_yy > as_date("2015-01-01"))) < 1) {
      mutate(., 
             date_mm_dd_yy = as.character(date_mm_dd_yy), 
             date_mm_dd_yy = str_replace(date_mm_dd_yy, ".* ", ""),
             date_mm_dd_yy = str_replace_all(date_mm_dd_yy, ":", "/"),
             date_mm_dd_yy = str_replace_all(date_mm_dd_yy, "(.*/.*/)", "\\120"),
             date_mm_dd_yy = anytime::anydate(date_mm_dd_yy),
             .before = 1)
    } else {.}}  %>%
    
    mutate(
    sample_collection_time_gmt = sample_collection_time_gmt*86400,
    sample_collection_time_gmt = hms::as_hms(sample_collection_time_gmt),
    date_time = ymd(date_mm_dd_yy) + hms(sample_collection_time_gmt),
    .after = sample_collection_time_gmt
    ) %>%
    
    # remover extract column if exists
    select(-any_of("time_filtered_24_00"))  %>% 
    mutate(station = str_remove(station, "~"),
           station = str_replace_all(station, " ", ""),
           station = str_to_upper(station),
           station = case_when(
             str_detect(station, "9.69999") ~ "9.7",
             str_detect(station, "9.80000") ~ "9.8",
             .default =  station))
  
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
#'
#' @author Sebastian Di Geronimo (May 2023)
#'
#' @return RETURN_DESCRIPTION
#' @examples
#' # ADD_EXAMPLES_HERE
search_meta_folders <- function(
    .dir_path,
    .folder_search = NULL,
    return_type    = c("vector", "tibble"),
    recurse_level  = FALSE) {
  
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
      type = "directory",
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
