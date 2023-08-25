# ============================================================================ #
# 
# 
# ---- Creation of Process Log Excel File ----
# 
# 
# ============================================================================ #  
# These functions are used in the creation and updating of the process log
# excel files. These are used for inputing metadata, file names from the lab  
# and fluoremetry data from the Turner Triology Fluormeter. 


##%######################################################%##
#                                                          #
####              Create Process Log Sheet              ####
#                                                          #
##%######################################################%##
#'@title Create Process Log Sheet
#'
#' @description 
#' This function is used to create process log sheets when running filter pad
#' samples or CDOM samples in the lab. This is cruise specific
#'
#' @param .meta The metadata for the specific cruise of interest.
#' 
#' @param cal Calibration information for the Turner Triology
#' 
#' @param cruise Cruise ID number (i.e WS19266)
#' @param sht_nm The name of two sheets to be created. 
#' @param inst_nm The name of the instrument used (default: "PElam850+")
#' @param creator Who create these Excel Log Sheets
#' 
#' @return An excel spreadsheet with the cruise name.
#' 
#' @note 
#' A few equations used to calculate [Chlorophyll-a]: \cr
#' 'No Acid' ratio: \cr\cr
#' \eqn{
#' \begin{aligned}
#' & \text{Excel Formula} =IF(R>0,((Q/R)+(S/T))/2,-999) \\
#' & na_{cl} = \text{No Acid Low Calibration (Q)} \\ 
#' & na_l = \text{No Acid Low (R)} \\
#' & na_{ch} = \text{No Acid High Calibration (S)} \\ 
#' & na_h = \text{No Acid High (T)} \\
#' \end{aligned}
#' } \cr
#' 
#' \deqn{\text{'No Acid' ratio} = 
#' \frac {(na_{cl} /na_l) + (na_{ch} / na_h)} {2}
#' } \cr
#' 
#' [Chl a] (mg m-3): \cr\cr
#' \eqn{
#' \begin{aligned}
#' & \text{Excel Formula} =+IF(L4>0,(O4*((L4-N4)*U4)+P4)*K4/J4,-999) \\
#' & na_s = \text{'No Acid' Slope (O)} \\
#' & na = \text{'No Acid' Rb (L)} \\
#' & na_b = \text{'No Acid' blank (N)} \\
#' & na_{lh} = \text{'No Acid' ratio (U)} \\
#' & na_{int} = \text{'No Acid'  y-int (P)} \\
#' & vol_{me} = \text{Volume methanol (ml) (K)} \\
#' & vol_{sw} = \text{Volume seawater (ml) (J)} \\
#' \end{aligned}
#' }
#' 
#' \deqn{\text{[Chl-a]} = (na_s * ((na - na_b) * na_{lh})) + na_{int}) * 
#' (vol_{me} / vol_{sw})
#' } \cr
#' 
#' If exist duplicates are averaged together and their standard deviations.
#' 
#' @examples
#' NA
#' 
process_log_sheet <- function(
  .meta, 
  cal, 
  cruise,
  sht_nm  = c("metadata", "Chl-a"), 
  inst_nm = "PElam850+ (2)",
  creator = "Sebastian Di Geronimo") {

  # ========================================================================== #
  # ---- Setup ----
  # ========================================================================== #  
  cli_text("\n\n")
  cli_alert_info("Starting cruise: {cruise}")
  cli_text("\n-----\n")
  cli_alert_info("Setup")
  
  librarian::shelf(
    librarian, tibble, tidyr, readr, purrr, dplyr, stringr,
    forcats, lubridate, glue, fs, magrittr, here,
    
    # additional
    openxlsx, hms, janitor, cli
  )

  # ---- Styles for formatting
  sstyles <- style_formatting()
  
  # ---- Filter Most Recent Cal Date
  cal <- cal[which(cal[,1][[1]] == max(cal[,1][[1]])),]
  
  .meta <- 
    .meta %>%
    select(-1, -sample_number) %>%
    mutate(sample_collection_time_gmt = as_hms(sample_collection_time_gmt)) 
  
  # ========================================================================== #
  # ---- Create Metadata Sheet ----
  # ========================================================================== #  
  # ---- Format Metadata Sheet Info
  meta <-
    .meta %>%
    select(
      date_mm_dd_yy,                                
      sample_collection_time_gmt,
      lat,
      lon,
      max_depth,
      depth_m,
      station,
      identifier,
      sample_type) %>%
    
    pivot_wider(
      names_from   = sample_type, 
      values_from  = identifier,
      values_fn    = list,
      names_repair = make_clean_names
    ) %>%
    unnest(c(chl_a, cdom, hplc)) %>%
    mutate(
      .by   = c(station, depth_m),
      cdom  = replace(cdom, duplicated(cdom), NA),
      hplc  = replace(hplc, duplicated(hplc), NA),
      ranks = str_remove(chl_a, ".*-"),
      ranks = str_remove_all(ranks, "[:alpha:]"),
      ranks = as.numeric(ranks)
    ) %>%
    arrange(ranks) %>%
    mutate(
      .keep                = "none",
      "Date (UTC)"         = date_mm_dd_yy,                                
      "Time (UTC)"         = sample_collection_time_gmt,
      "Latitude (deg. N)"  = lat,
      "Longitude (deg. W)" = lon,
      "Bottom Depth (m)"   = max_depth,
      "Sample Depth (m)"   = depth_m,
      "Station ID"         = station,
      "Instrument"         = inst_nm,
      "Sample ID"          = chl_a,
      "CDOM Sample ID"     = cdom,
      "HPLC Sample ID"     = hplc,
    ) %>%
    mutate(
      "ap,d(λ) process date"                       = NA_Date_,
      "ap(λ) filename (*.Sample.Raw*)"             = NA_character_,
      "ad(λ) filename (*.Sample.Raw*)"             = NA_character_,
      "CDOM process date"                          = NA_Date_,
      "Instrument "                                = inst_nm,
      "aCDOM(λ) filename, Rep. #1 (*.Sample.Raw*)" = NA_character_,
      "aCDOM(λ) filename, Rep. #2 (*.Sample.Raw*)" = NA_character_,
      "aCDOM(λ) filename, Rep. #3 (*.Sample.Raw*)" = NA_character_,
      "Comments"                                   = NA_character_
    ) %>%
    relocate("ap,d(λ) process date", 
             .before = Instrument) %>%
    relocate("CDOM Sample ID", 
             .after = "Instrument ")  %>%
    relocate( "HPLC Sample ID", 
              .before = "CDOM process date")
  
  
  # ---- Create Worksheet
  cli_alert_info("Creating workbook")
  
  wb <- createWorkbook(
    creator = creator, 
    title   = glue("{cruise} Process Log"))
  
  # ---- Add Info
  cli_alert_info("Adding worksheet: {.var {sht_nm[1]}}")
  addWorksheet(wb, sheetName = sht_nm[1])
  
  activeSheet(wb) <- which(names(wb) == sht_nm)
  
  # add black line to bottom of headers
  writeData(
    wb, 
    x        = meta, 
    sheet    = sht_nm[1], 
    startCol = 1, 
    startRow = 1,
    headerStyle = sstyles$styl_brd_head
  )
  
  
  # column numbers for filter pads, HPLC and CDOM
  cols <-
    list(
      # date columns
      date = which(str_detect(names(meta), "(?i)date")),
        
      # filter pad columns
      pads = seq(
        which(str_detect(names(meta), "ap.*process date")),
        max(which(str_detect(names(meta), "(?i)acdom.*#.*Sample.Raw")))
      ),

      # HPLC columns
      hplc = which(str_detect(names(meta), "(?i)hplc sample id")),

      # CDOM columns
      cdom = seq(
        which(str_detect(names(meta), "(?i)cdom.*process date")),
        max(which(str_detect(names(meta), "(?i)acdom.*#.*Sample.Raw")))
      )
    )
  
  # ---- Set Width and Height of Rows and Cols
  format_styles(
    wb, 
    sht_nm, 
    cols,
    names(meta), 
    type = "meta_col_wh"
  )
  
  # ---- Add Styles
  format_styles(
    wb,
    sht_nm     = sht_nm,
    names_meta = names(meta),
    dim_meta   = dim(meta),
    cols       = cols,
    sstyles    = sstyles,
    type       = "meta"
  )

  # ========================================================================== #
  # ---- Create Chl-a Sheet ----
  # ========================================================================== # 
  cli_alert_info("Adding worksheet: {.var {sht_nm[2]}}")
  
  # ---- Format Chla sheet Info
  chl_a <-
    .meta %>%
    filter(str_detect(sample_type, "Chl-a")) %>%
    select(
      date_mm_dd_yy, sample_collection_time_gmt, 
      lat, lon, max_depth, depth_m, 
      station, identifier, vol_ml)  %>%
    mutate(
      .keep = "none",
      "Date (UTC)"           = date_mm_dd_yy,                                
      "Time (UTC)"           = sample_collection_time_gmt,
      "Latitude (deg. N)"    = lat,
      "Longitude (deg. W)"   = lon,
      "Bottom Depth (m)"     = max_depth,
      "Sample Depth (m)"     = depth_m,
      "Station ID"           = station,
      "Sample ID"            = identifier,
      "Volume seawater (ml)" = vol_ml
    ) %>%
    mutate(
      "Process date"                 = NA_Date_,
      "Volume methanol (ml)"         = NA_real_,
      "'No Acid' Rb"                 = NA_real_,
      "Fluorometer Calibration date" = cal$cal_date,
      "'No Acid' blank"               = NA_real_,
      "'No Acid' Slope"              = cal$no_acid_slop,
      "'No Acid' y-int"              = cal$no_acid_y_int,
      "Cal. 'No Acid' Low"           = cal$cal_no_acid_low, 
      "'No Acid' Low"                = NA_real_,               
      "Cal. 'No Acid' High"          = cal$cal_no_acid_high,
      "'No Acid' High"               = NA_real_,
      "'No Acid' ratio"              = NA_real_,
      "[Chl a] (mg m^-3)"            = NA_real_, 
      "AVG [Chl a] (mg m^-3)"        = NA_real_, 
      "STDEV [Chl a] (mg m^-3)"      = NA_real_,
      "Comments"                     = NA_character_
    ) %>%
    relocate("Process date", .after = "Sample ID")
  
  # ---- Add sheet
  addWorksheet(wb, sheetName = sht_nm[2])
  
  # ---- Add Large Headers
  add_info(wb, sht_nm, sstyles, type = "header")
  
  # ---- Add Info
  writeData(
    wb, 
    x           = chl_a,
    sheet       = sht_nm[2], 
    startCol    = 1, 
    startRow    = 3, 
    headerStyle = sstyles$styl_brd_head)
  
  # ---- Add Styles
  format_styles(
    wb,
    sht_nm,
    dim_chl = dim(chl_a),
    dim_meta = dim(meta),
    sstyles,
    type = "chl_a"
    )
  
  # ---- Specify Width and Heights
  format_styles(
    wb, 
    sht_nm, 
    col_names = names(chl_a),
    type      = "chl_col_wh"
    )
  
  # ---- Add formulas to Cells
  add_info(
    wb, 
    sht_nm, 
    chl_names = names(chl_a), 
    nr_chl    = nrow(chl_a), 
    type      = "formula"
    )
  
  # ---- Write Comments
  add_info(
    wb, 
    sht_nm, 
    chl_names = names(chl_a), 
    type      = "comment"
    )
  
  
  # ---- Reorder Sheets
  worksheetOrder(wb) <- c(2, 1)
  
  cli_alert_success("Finishing!\n----\n\n")
  
  return(wb)
  
  # ---- end of function process_log_sheet
  }

# ============================================================================ #
# ---- !!! Stylizing Excel Sheets !!! ----
# ============================================================================ #  

##%######################################################%##
#                                                          #
####   Styles for Excel When Formatting Process Log     ####
#                                                          #
##%######################################################%##
#' Styles for Excel When Formatting Process Log 
#'
#' Standard styling for process logs.
#'
#' @param  NA
#'
#' @return RETURN_DESCRIPTION
#'
#' @author Sebastian Di Geronimo (August 23, 2023)
#'
#' @examples
#' # ADD_EXAMPLES_HERE
#' 
style_formatting <- function() {
  list(
    styl_date     = createStyle(numFmt = "DATE"),
    styl_time     = createStyle(numFmt = "hh:mm"),
    styl_num      = createStyle(numFmt = "0.00"),
    styl_col1     = createStyle(fgFill = "#FDE9D9"),
    styl_col2     = createStyle(fgFill = "#DCE6F1"),
    styl_col3     = createStyle(fontName       = "Arial",
                                fontSize       = 22,
                                fontColour     = "white",
                                textDecoration = "bold",
                                border         = "TopBottomLeftRight",
                                fgFill         = "#948A54",
                                halign         = "center"),
    styl_col4     = createStyle(fgFill = "#DDD9C4",
                                halign = "center"),
    styl_col5     = createStyle(fgFill = "#F2DCDB"),
    styl_border   = createStyle(border      = "Right", 
                                borderStyle = "thin"),
    styl_border_l = createStyle(border      = "Left", 
                                borderStyle = "thin"),
    styl_brd_bot  = createStyle(border      = "bottom", 
                                borderStyle = "thin"),
    styl_brd_head = createStyle(border         = "bottom", 
                                borderStyle    = "thin",
                                textDecoration = "bold",
                                halign         = "center",
                                wrapText       = TRUE),
    styl_align    = createStyle(halign = "center"),
    styl_brd_dash = createStyle(border      = "right", 
                                borderStyle = "dotted")
  ) 
  
    # ---- end of function style_formatting
}

##%######################################################%##
#                                                          #
####                Add Styles to Sheets                ####
#                                                          #
##%######################################################%##
#' Add Styles to Sheets
#'
#' FUNCTION_DESCRIPTION
#'
#' @param wb DESCRIPTION.
#' @param type DESCRIPTION.
#' @param ... DESCRIPTION.
#'
#' @author Sebastian Di Geronimo (August 23, 2023)
#'
#' @return RETURN_DESCRIPTION
#' 
#' @rdname style_format
#' 
#' @examples
#' # ADD_EXAMPLES_HERE
#' 
format_styles <- function(
    wb, type = c("meta", "chl_a", "meta_col_wh", "chl_col_wh"), ...) {
  
  type      <- match.arg(type)
  class(wb) <- type
  
  UseMethod("format_styles", wb)
  # ---- end of function format_styles
  }


##%######################################################%##
#                                                          #
####   Set Column Height and Width for Metadata Sheet   ####
#                                                          #
##%######################################################%##
#' Set Column Height and Width for Metadata Sheet
#'
#' This sets the column height and width for the metadata sheet
#'
#' @param wb Workbook to modify
#' @param sht_nm Sheet name to modify
#' @param cols A list containing column number for HPLC and cdom
#' @param meta_names Column names for metadata
#'
#' @author Sebastian Di Geronimo (August 23, 2023)
#'
#' @return RETURN_DESCRIPTION
#' 
#' @rdname style_format
#' 
#' @examples
#' # ADD_EXAMPLES_HERE
format_styles.meta_col_wh <- function(
    wb, 
    sht_nm, 
    cols,
    meta_names,
    ...) {
  
  # ----  Width and Heights
  setRowHeights(
    wb, 
    sheet   = sht_nm[1], 
    rows    = 1, 
    heights = 73.2)
  
  setColWidths(
    wb, 
    sheet  = sht_nm[1], 
    cols   = cols$cdom[length(cols$cdom)] + 1, 
    widths = 39.56)
  
  setColWidths(
    wb, 
    sheet  = sht_nm[1], 
    cols   = cols$hplc, 
    widths = 15.67)
  
  setColWidths(
    wb, 
    sheet  = sht_nm[1], 
    cols   = 
      c(which(str_detect(meta_names, "(?i)^sample id")),
        which(str_detect(meta_names, "(?i)^cdom sample id"))
      ), 
    widths = 12.89)

  setColWidths(
    wb, 
    sheet  = sht_nm[1], 
    cols   = 
      c(which(str_detect(meta_names, "(?i)instrument")),
        which(str_detect(meta_names, "(?i)^instrument "))
      ), 
    widths = 12.22)
  
  # ---- end of function format_styles.meta_col_wh
}

##%######################################################%##
#                                                          #
####    Set Column Height and Width for Chl-a Sheet     ####
#                                                          #
##%######################################################%##
#' Set Column Height and Width for Chl-a Sheet
#'
#' This sets the column height and width for the metadata sheet
#'
#' @param wb Workbook to modify
#' @param sht_nm Sheet name to modify
#' @param cols A list containing column number for HPLC and cdom
#' @param meta_names Column names for metadata
#'
#' @author Sebastian Di Geronimo (August 23, 2023)
#'
#' @return RETURN_DESCRIPTION
#' 
#' @rdname style_format
#' 
#' @examples
#' # ADD_EXAMPLES_HERE
format_styles.chl_col_wh <- function(
    wb, 
    sht_nm, 
    col_names,
    ...) {
  
  comm_col   <- which(str_detect(col_names, "(?i)comments"))
  sample_col <- which(str_detect(col_names, "(?i)sample id"))
  
  # ----  Width and Heights
  setRowHeights(wb, sheet = sht_nm[2], rows = 1, heights = 29.3)
  setRowHeights(wb, sheet = sht_nm[2], rows = 2, heights = 14.4)
  setRowHeights(wb, sheet = sht_nm[2], rows = 3, heights = 57)
  
  setColWidths(wb, sheet = sht_nm[2], cols = comm_col, widths = 40.56)
  setColWidths(wb, sheet = sht_nm[2], cols = sample_col, widths = 12)
  
  # ---- end of function format_styles.chl_col_wh
}


##%######################################################%##
#                                                          #
####            Add Styles to Metadata Sheet            ####
#                                                          #
##%######################################################%##
#' Add Styles to Metadata Sheet
#'
#' FUNCTION_DESCRIPTION
#'
#' @param wb Worksheet
#' @param sht_nm Sheet name
#' @param names_meta Column names from metadta
#' @param dim_meta Dimenstion of metadata (vector of 2 values)
#' @param cols A list containing column number for filter pads, HPLC and cdom
#' @param sstyles Format styles as a `list()`
#'
#' @author Sebastian Di Geronimo (August 23, 2023)
#'
#' @return RETURN_DESCRIPTION
#' 
#' @rdname style_format
#' 
#' @examples
#' # ADD_EXAMPLES_HERE
#' 
format_styles.meta <- function(
    wb, 
    sht_nm, 
    names_meta, 
    dim_meta,
    cols,
    sstyles,
    ...) {
  
  options(openxlsx.dateFormat = "mm/dd/yy")
  
  # ---- Add Styles
  # date
  addStyle(
    wb, 
    sht_nm[1],
    style      = sstyles$styl_date,
    rows       = 1:(dim_meta[1] + 1),
    # cols       = 1,
    cols       = cols$date,
    gridExpand = TRUE,
    stack      = TRUE)
  
  # time
  addStyle(
    wb, 
    sht_nm[1],
    style      = sstyles$styl_time,
    rows       = 1:dim_meta[1] + 1,
    cols       = 2,
    gridExpand = TRUE,
    stack      = TRUE)
  
  # depth
  addStyle(
    wb, 
    sht_nm[1],
    style      = sstyles$styl_num,
    rows       = 1:dim_meta[1] + 1,
    cols       = 5:6,
    gridExpand = TRUE,
    stack      = TRUE)
  
  # color filter pads
  addStyle(
    wb, 
    sheet      = sht_nm[1], 
    style      = sstyles$styl_col1,
    cols       = cols$pads,
    rows       = 1:(dim_meta[1] + 1),
    gridExpand = TRUE,
    stack      = TRUE)
  
  # color hplc
  addStyle(
    wb, 
    sheet      = sht_nm[1], 
    style      = sstyles$styl_col5,
    cols       = cols$hplc,
    rows       = 1:(dim_meta[1] + 1),
    gridExpand = TRUE,
    stack      = TRUE)
  
  # color cdom 
  addStyle(
    wb, 
    sheet      = sht_nm[1], 
    style      = sstyles$styl_col2,
    cols       = cols$cdom,
    rows       = 1:(dim_meta[1] + 1),
    gridExpand = TRUE,
    stack      = TRUE)
  
  # add thin borders between sections
  addStyle(
    wb, 
    sheet      = sht_nm[1], 
    style      = sstyles$styl_border_l,
    cols       = c(
      cols$pads[1], cols$hplc, 
      cols$cdom[1],
      dim_meta[2], dim_meta[2] + 1
      ),
    rows       = 1:(dim_meta[1] + 1),
    gridExpand = TRUE, 
    stack      = TRUE)
  
  # add black line to end
  addStyle(
    wb, 
    sheet = sht_nm[1],
    cols  = dim_meta[2],
    rows  = 1,
    style = sstyles$styl_brd_head,
    stack = TRUE)
  
  addStyle(
    wb, 
    sheet      = sht_nm[1], 
    style      = sstyles$styl_brd_bot,
    cols       = seq(dim_meta[2]),
    rows       = dim_meta[1] + 1,
    gridExpand = TRUE, 
    stack      = TRUE)
  
  addStyle(
    wb, 
    sheet      = sht_nm[1], 
    style      = sstyles$styl_align,
    cols       = 1:19,
    rows       = 1:dim_meta[1] + 1,
    gridExpand = TRUE, 
    stack      = TRUE)
  
  addStyle(
    wb, 
    sht_nm[1], 
    style      = sstyles$styl_brd_dash, 
    rows       = 1:(dim_meta[1] + 1), 
    cols       = c(which(str_detect(names_meta, "(?i)^sample id")),
                   which(str_detect(names_meta, "(?i)^cdom sample id"))), 
    stack      = TRUE,
    gridExpand = TRUE)
  
  
  # ---- Freeze Pane
  freezePane(wb, sht_nm[1], firstRow = TRUE)
  
  # ---- end of function format_styles.meta
}

##%######################################################%##
#                                                          #
####             Add Styles to Chl-a Sheet              ####
#                                                          #
##%######################################################%##
#' Add Styles to Chl-a Sheet
#'
#' FUNCTION_DESCRIPTION
#'
#' @param wb DESCRIPTION.
#' @param sht_nm DESCRIPTION.
#' @param dim_chl DESCRIPTION.
#' @param dim_meta DESCRIPTION.
#' @param sstyles DESCRIPTION.
#' @param ... DESCRIPTION.
#'
#' @author Sebastian Di Geronimo (August 23, 2023)
#'
#' @return RETURN_DESCRIPTION
#' 
#' @rdname style_format
#' 
#' @examples
#' # ADD_EXAMPLES_HERE
#' 
format_styles.chl_a <- function(
    wb,
    sht_nm,
    dim_chl,
    dim_meta,
    sstyles,
    ...
) {
  # ---- Add Styles
  options(openxlsx.dateFormat = "mm/dd/yy")
  
  # ---- styles 
  addStyle(
    wb, 
    sht_nm[2],
    style      = sstyles$styl_date,
    rows       = 3:(dim_meta[1] + 3),
    cols       = 1,
    gridExpand = TRUE,
    stack      = TRUE)
  
  addStyle(
    wb, 
    sht_nm[2],
    style      = sstyles$styl_date,
    rows       = 3:(dim_meta[1] + 3),
    cols       = "M",
    gridExpand = TRUE,
    stack      = TRUE)
  
  addStyle(
    wb, 
    sht_nm[2],
    style      = sstyles$styl_time,
    rows       = 3:(dim_meta[1] + 3),
    cols       = 2,
    gridExpand = TRUE,
    stack      = TRUE)
  
  
  addStyle(
    wb, 
    sht_nm[2],
    rows       = 2:(dim_chl[1] + 3),
    cols       = 9:24,
    stack      = TRUE,
    style      = sstyles$styl_col4,
    gridExpand = TRUE
  )
  
  addStyle(
    wb, 
    sht_nm[2], 
    style      = sstyles$styl_border, 
    rows       = 2:(dim_chl[1] + 3), 
    cols       = c("H", "L", "P", "U", "X", "Y"), 
    stack      = TRUE,
    gridExpand = TRUE)
  
  addStyle(
    wb, 
    sht_nm[2], 
    style      = sstyles$styl_brd_dash, 
    rows       = 3:(dim_chl[1] + 3), 
    cols       = "T", 
    stack      = TRUE,
    gridExpand = TRUE)
  
  addStyle(
    wb, 
    sht_nm[2], 
    style      = sstyles$styl_brd_bot,
    rows       = dim_chl[1] + 3,
    cols       = 1:dim_chl[2], 
    stack      = TRUE)
  
  addStyle(
    wb, 
    sht_nm[2],
    style      = sstyles$styl_num,
    rows       = 1:dim_meta[1] + 1,
    cols       = 5:6,
    gridExpand = TRUE,
    stack      = TRUE)
  
  # ---- Freeze Pane
  freezePane(wb, sht_nm[2], firstActiveRow = 4)
  
  # ---- end of function format_syles.chl_a
}


# ============================================================================ #
# ---- !!! Adding Information !!! ----
# ============================================================================ # 
##%######################################################%##
#                                                          #
####             Add Information to Sheets              ####
#                                                          #
##%######################################################%##
#' Add Information to Sheets
#'
#' FUNCTION_DESCRIPTION
#'
#' @param wb DESCRIPTION.
#' @param type DESCRIPTION.
#' @param ... DESCRIPTION.
#'
#' @author Sebastian Di Geronimo (August 23, 2023)
#'
#' @return RETURN_DESCRIPTION
#' 
#' @rdname add_info
#' 
#' @examples
#' # ADD_EXAMPLES_HERE
#' 
add_info <- function(wb, type = c("header", "formula", "comment"), ...) {
  
  type     <- match.arg(type)
  
  class(wb) <- type
  
  UseMethod("add_info", wb)
  
  # ---- end of function add_info
} 


##%######################################################%##
#                                                          #
####            Add Large Header Information            ####
#                                                          #
##%######################################################%##
#' Add Large Header Information
#'
#' FUNCTION_DESCRIPTION
#'
#' @param wb DESCRIPTION.
#' @param sht_nm DESCRIPTION.
#' @param ... DESCRIPTION.
#'
#' @author Sebastian Di Geronimo (August 23, 2023)
#'
#' @return RETURN_DESCRIPTION
#' 
#' @rdname add_info
#' 
#' @examples
#' # ADD_EXAMPLES_HERE
add_info.header <- function(
    wb,
    sht_nm,
    sstyles,
    ...) {
  
  # ========================================================================== #
  # ---- Load Libraries ----
  # ========================================================================== #  
  library("librarian")
  shelf(
    librarian, tibble, tidyr, readr, purrr, dplyr, stringr,
    forcats, lubridate, glue, fs, magrittr, here,
    
    # additional
    openxlsx, hms, janitor, cli
  )
  
  mergeCells(wb, sht_nm[2], cols = 9:24, rows = 1)
  
  writeData(
    wb, 
    sht_nm[2], 
    "Trilogy Fluorometer", 
    startCol = 9, 
    startRow = 1, 
    colNames = FALSE)
  
  addStyle(
    wb, 
    sht_nm[2], 
    rows  = 1, 
    cols  = 9:24, 
    stack = TRUE, 
    style = sstyles$styl_col3)
  
  # ---- Add Smaller Headers
  for (i in list(9:12, 13:16, 17:21, 22:24)) {
    mergeCells(wb, sht_nm[2], cols = i, rows = 2) 
  }
  
  writeData(
    wb, 
    sht_nm[2], 
    "Secondary Solid Standard", 
    startCol = 17, 
    startRow = 2, 
    colNames = FALSE)
  
  writeData(
    wb, 
    sht_nm[2], 
    "Welschmeyer (no acid)",
    startCol = 22, 
    startRow = 2, 
    colNames = FALSE)
  
    # ---- end of function add_info.headers
}



##%######################################################%##
#                                                          #
####             Add Formulas to Chl Sheet              ####
#                                                          #
##%######################################################%##
#' Add Formulas to Chl Sheet
#'
#' FUNCTION_DESCRIPTION
#'
#' @param wb Workbook
#' @param sht_nm Sheet Name
#' @param chl_names Column names for chl sheet
#' @param formulas Formulas to add into the input
#' @param ... Additional inputs to function
#' 
#' @author Sebastian Di Geronimo (August 23, 2023)
#'
#' @return RETURN_DESCRIPTION
#' 
#' @rdname add_info
#' 
#' @examples
#' # ADD_EXAMPLES_HERE
add_info.formula <- function(
  wb, 
  sht_nm, 
  chl_names, 
  nr_chl,
  ...) {
  
  # ---- Formulas for Chlor-a Sheet
  # =IF(R4>0,((Q4/R4)+(S4/T4))/2,-999)
  no_acid_ratio <- paste0(
    "IF(", paste0("R", 1:nr_chl + 3), ">0,((", paste0("Q", 1:nr_chl + 3), 
    "/", paste0("R", 1:nr_chl + 3), ")+(", paste0("S", 1:nr_chl + 3), 
    "/", paste0("T", 1:nr_chl + 3), "))/2,-999)")
  
  # V =+IF(L4>0,(O4*((L4-N4)*U4)+P4)*K4/J4,-999)
  chla_conc <- 
    paste0("+IF(", paste0("L", 1:nr_chl + 3),">0,(", 
           paste0("O", 1:nr_chl + 3),"*((",paste0("L", 1:nr_chl + 3),
           "-",paste0("N", 1:nr_chl + 3),")*",paste0("U", 1:nr_chl + 3),
           ")+",paste0("P", 1:nr_chl + 3),")*",paste0("K", 1:nr_chl + 3),
           "/",paste0("J", 1:nr_chl + 3),",-999)")
  
  formulas <- 
    tibble(
      # W =AVERAGE(V4:V5)
      avg = paste0(
        "IF(AND(", paste0("V", seq(1, nr_chl, 2) + 3), ">=0, ", 
        paste0("V", seq(1, nr_chl, 2) + 4), ">=0), ",
        paste0("AVERAGE(",paste0("V", seq(1, nr_chl, 2) + 3), ":",
               paste0("V", seq(1, nr_chl, 2) + 4),")"), ", -999)"
      ),
      # X =STDEV(V4:V5)
      stdev = paste0(
        "IF(AND(", paste0("V", seq(1, nr_chl, 2) + 3), ">=0, ", 
        paste0("V", seq(1, nr_chl, 2) + 4), ">=0), ",
        paste0("STDEV(",paste0("V", seq(1, nr_chl, 2) + 3), ":",
               # paste0("V", seq(1, nr_chl, 2) + 4),")"), ", \"\")"
               paste0("V", seq(1, nr_chl, 2) + 4),")"), ", -999)"
      ),
    )  %>%
    bind_rows(tibble(avg = rep(NA, nr_chl / 2))) %>%
    mutate(rows = rep(1:(nr_chl / 2), 2)) %>%
    arrange(rows, !is.na(avg), avg) %>%
    select(-rows) %>%
    mutate( 
      # col U 
      no_acid_ratio = no_acid_ratio,
      # col V
      chla_conc = chla_conc) 
  
  # ---- column values for comments
  na_ratio  <- which(str_detect(chl_names, "(?i)^'No Acid' ratio"))
  chl_conc  <- which(str_detect(chl_names, "(?i)^\\[Chl a\\]"))
  avg_chl   <- which(str_detect(chl_names, "(?i)^AVG"))
  std_chl   <- which(str_detect(chl_names, "(?i)^STDEV"))
  
  # ---- add formulas
  writeFormula(
    wb, 
    sht_nm[2],
    formulas$no_acid_ratio, 
    startCol = na_ratio,
    startRow = 4)
  
  writeFormula(
    wb, 
    sht_nm[2],
    formulas$chla_conc, 
    startCol = chl_conc,
    startRow = 4)
  
  writeFormula(
    wb, 
    sht_nm[2],
    formulas$avg, 
    startCol = avg_chl,
    startRow = 4)
  
  writeFormula(
    wb, 
    sht_nm[2],
    formulas$stdev, 
    startCol = std_chl,
    startRow = 4)
  
  # ---- end of function add_info.formula
}


##%######################################################%##
#                                                          #
####               Add Comments to Cells                ####
#                                                          #
##%######################################################%##
#' Add Comments to Cells
#'
#' FUNCTION_DESCRIPTION
#'
#' @param wb Workbook
#' @param sht_nm Sheet Name
#' @param chl_names Column names for chl sheet
#' @param comm Comments
#' @param ... Additional inputs to function
#'
#' @author Sebastian Di Geronimo (August 23, 2023)
#'
#' @return RETURN_DESCRIPTION
#' 
#' @rdname add_info
#' @examples
#' # ADD_EXAMPLES_HERE
add_info.comment <- function(
    wb, sht_nm, chl_names, ...) {
  
  # ---- Comments for Chlor-a Sheet
  comm <- 
    list(
      fluor     = createComment(
        "*Jen*: Fluoresence Reading of the Sample (RFU)",
        author  = "Jen", visible = FALSE),
      blank     = createComment(
        "*Jen*: Fluorescence reading for 100% methanol 'blank'",
        author  = "Jen", visible = FALSE),
      low_read  = createComment(
        "*Jen*: Report average of all 'low' readings collected that day.",
        author  = "Jen", visible = FALSE),
      high_read = createComment(
        "*Jen*: Report average of all 'high' readings collected that day.", 
        author  = "Jen", visible = FALSE)
    )
  
  # ---- column values for comments
  na_rb  <- which(str_detect(chl_names, "(?i)^'No Acid' Rb"))
  na_blk <- which(str_detect(chl_names, "(?i)^'No Acid' blank"))
  na_low <- which(str_detect(chl_names, "(?i)^'No Acid' low"))
  na_hi  <- which(str_detect(chl_names, "(?i)^'No Acid' high"))
  
  # ---- add comments
  writeComment(wb, sht_nm[2], col = na_rb,  row = 3, comment = comm$fluor)
  writeComment(wb, sht_nm[2], col = na_blk, row = 3, comment = comm$blank)
  writeComment(wb, sht_nm[2], col = na_low, row = 3, comment = comm$low_read)
  writeComment(wb, sht_nm[2], col = na_hi,  row = 3, comment = comm$high_read)
  
  # ---- end of function add_info.comment
}



# ============================================================================ #
# ---- !!! Updating Excel Sheets !!! ----
# ============================================================================ # 
##%######################################################%##
#                                                          #
####           Update Process Log Information           ####
#                                                          #
##%######################################################%##
#' Update Process Log Information
#'
#' FUNCTION_DESCRIPTION
#'
#' @param wb Workbook
#' @param type Updating metadta or calibration information
#' @param ... Inputs for `methods`
#'
#' @return RETURN_DESCRIPTION
#' @examples
#' # ADD_EXAMPLES_HERE
#' 
update_proc_log <- function(x, type = c("meta", "cal", "meta2"), ...) {
  
  # ========================================================================== #
  # ---- Load Libraries ----
  # ========================================================================== #  
  library("librarian")
  shelf(
    librarian, tibble, tidyr, readr, purrr, dplyr, stringr,
    forcats, lubridate, glue, fs, magrittr, here,
    
    # additional
    openxlsx, hms, janitor, cli
  )
  
  type     <- match.arg(type)
  class(x) <- type
  
  UseMethod("update_proc_log", x)
  # ---- end of function update_proc_log
}

##%######################################################%##
#                                                          #
####     Update Process Log Calibration Information     ####
#                                                          #
##%######################################################%##
update_proc_log.cal <- function(x, cal, ...) {
  
  names(cal) <-  c(
    "Fluorometer Calibration date",
    "'No Acid' Slope",
    "'No Acid' y-int",
    "Cal. 'No Acid' Low",
    "Cal. 'No Acid' High"
  )
  
  update <- 
    x %>% 
    readWorkbook(
      sheet       = "Chl-a", 
      startRow    = 3, 
      detectDates = TRUE,
      sep.names   = " ") %>%
    mutate(
      .keep = "none",
      `Process date`,
      data = pmap(
        list(`Process date`, `Fluorometer Calibration date`), cal,
        .f = function(x, y, cal) {
          
          if (is.na(x)) {
            cal[which(cal[,1][[1]] == max(cal[,1][[1]])),]
            
          } else {
            val <- max(which(!x < cal[,1][[1]]))
            cal[val,]
          }
          # ---- may not need, the difference is the prev cal will be kept if it 
          # is also less than sample process date. 
          # the issue is that it may not be the closest process date and 
          # therefore would be wrong
          # 
          # if (is.na(x)) {
          #   cal[which(cal[,1][[1]] == max(cal[,1][[1]])),]
          #   
          # } else if (x < y) {
          #   val <- max(which(!x < cal[,1][[1]]))
          #   cal[val,]
          # } else {
          #   val <- which(y == cal[,1][[1]])
          #   cal[val,] %T>% print()
          # }
        }
      )
    ) %>%
    unnest(data)
  
  wb <- loadWorkbook(x)
  
  # `Fluorometer Calibration date`
  writeData(
    wb,
    sheet    = "Chl-a",
    startRow = 4,
    startCol = 13,
    x        = update[[names(cal)[1]]]
  )
  
  # `'No Acid' Slope`
  writeData(
    wb,
    sheet    = "Chl-a",
    startRow = 4,
    startCol = 15,
    x        = update[[names(cal)[2]]]
  )
  
  # `'No Acid' y-int`
  writeData(
    wb,
    sheet    = "Chl-a",
    startRow = 4,
    startCol = 16,
    x        = update[[names(cal)[3]]]
  )
  
  # `Cal. 'No Acid' Low`
  writeData(
    wb,
    sheet    = "Chl-a",
    startRow = 4,
    startCol = 17,
    x        = update[[names(cal)[4]]]
  )
  
  # `Cal. 'No Acid' High`
  writeData(
    wb,
    sheet    = "Chl-a",
    startRow = 4,
    startCol = 19,
    x        = update[[names(cal)[5]]]
  )
  
  
  return(wb)
}


##%######################################################%##
#                                                          #
####            Update Process Log Metadata             ####
#                                                          #
##%######################################################%##
#' Update Process Log Metadata
#'
#' This function should be used to update the process log excel files if the 
#' information in the inventory sheet was updated and affects the files in the
#' process log.
#'
#' @param x Metadata 
#' @param cruise Cruise ID
#' @param prev_log Previous log file path
#' @param prev_sht New sheet name (if want to change)
#' @param old_sht_rn Old sheet name
#' @param .keep_wksht optional: `TRUE/FALSE`, keep old worksheet
#'
#' @return RETURN_DESCRIPTION
#' @examples
#' # ADD_EXAMPLES_HERE
#' 
update_proc_log.meta <- function(
    x,
    cruise,
    prev_log,
    prev_sht = c("metadata", "Chl-a"),
    old_sht_rn = "old_",
    .keep_wksht = TRUE,
    ...) {
  
  # ========================================================================== #
  # ---- Setup ----
  # ========================================================================== #
  cli_text("\n\n")
  cli_alert_info("Starting cruise: {cruise}")
  cli_text("\n-----\n")
  cli_alert_info(c("Cruise ID: ", cruise))
  cli_alert_info("Setup")

  # ---- Styles for formatting ----
  sstyles <- style_formatting()

  # ---- Load Worksheet
  wb <- loadWorkbook(prev_log)

  # ========================================================================== #
  # ---- Update Metadata Sheet ----
  # ========================================================================== #
  cli_alert_info("Updating worksheet: {.var {prev_sht[1]}}")

  cli_alert_info("Loading  old logsheet")
  old_log <-
    prev_log %>%
    read.xlsx(
      sheet       = prev_sht[1],
      sep.names   = " ",
      detectDates = TRUE
    ) %>%
    rename(
      "Instrument " = which(str_detect(
        names(.),
        "(?i)cdom sample id"
      )) - 1
    ) %>%
    select(matches("ap.*process date"):last_col()) %>%
    select(-matches("(?i)hplc sample"))
  
  cli_alert_info("Formatting Metadata")
  
  meta <-
    x %>%
    select(
      date_mm_dd_yy,
      sample_collection_time_gmt,
      lat,
      lon,
      max_depth,
      depth_m,
      station,
      identifier,
      sample_type
    ) %>%
    pivot_wider(
      names_from   = sample_type,
      values_from  = identifier,
      values_fn    = list,
      names_repair = make_clean_names
    ) %>%
    unnest(c(chl_a, cdom, hplc)) %>%
    mutate(
      .by = c(station, depth_m),
      cdom = replace(cdom, duplicated(cdom), NA),
      hplc = replace(hplc, duplicated(hplc), NA),
      ranks = str_remove(chl_a, ".*-"),
      ranks = str_remove_all(ranks, "[:alpha:]"),
      ranks = as.numeric(ranks),
      sample_collection_time_gmt = as_hms(sample_collection_time_gmt)
    ) %>%
    arrange(ranks) %>%
    select(-ranks, -cdom) %>%
    mutate(
      .keep = "unused",
      "Date (UTC)" = date_mm_dd_yy,
      "Time (UTC)" = sample_collection_time_gmt,
      "Latitude (deg. N)" = lat,
      "Longitude (deg. W)" = lon,
      "Bottom Depth (m)" = max_depth,
      "Sample Depth (m)" = depth_m,
      "Station ID" = station,
      "Sample ID" = chl_a,
      "HPLC Sample ID" = hplc,
    )
  
  hdr_name <- names(meta)
  
  recreate_sheet <- nrow(meta) > nrow(old_log)
  
  cli_alert_info("Merging old logsheet with metadata")
  
  meta <-
    meta %>%
    left_join(old_log, .,
              by = "Sample ID"
    ) %>%
    select(hdr_name, everything()) %>%
    mutate(
      ranks = str_remove(`Sample ID`, ".*-"),
      ranks = str_remove_all(ranks, "[:alpha:]"),
      ranks = as.numeric(ranks)
    ) %>%
    arrange(ranks) %>%
    select(-ranks) %>%
    relocate("ap,d(λ) process date",
             .before = Instrument
    ) %>%
    relocate("Sample ID",
             .after = Instrument
    ) %>%
    relocate("CDOM Sample ID",
             .after = "Instrument "
    ) %>%
    relocate("HPLC Sample ID",
             .before = "CDOM process date"
    ) %>%
    mutate(across(contains("date"), \(x) anytime::anydate(x)))
  
  
  sht_nm  <- prev_sht[1]
  old_sht <- paste0(old_sht_rn,  str_sub(prev_sht[1], 1, 4), "_update_on", Sys.Date())

  if (!recreate_sheet) {
    # replacing data only if same number of rows in prev and current
    cloneWorksheet(wb, clonedSheet = sht_nm, sheetName = old_sht)

    activeSheet(wb) <- which(names(wb) == sht_nm)

    # add data
    writeData(
      wb,
      x        = meta,
      sheet    = sht_nm[1],
      startCol = 1,
      startRow = 2,
      colNames = FALSE
    )
  } else {
    
    # ======================================================================== #
    # --- When needing to add rows, recreate sheet entirely ---
    # ======================================================================== #
    renameWorksheet(wb, sheet = prev_sht[1], newName = old_sht)

    # ---- Add Info
    addWorksheet(wb, sheetName = sht_nm[1])

    activeSheet(wb) <- which(names(wb) == sht_nm)

    num_sheets <- length(names(wb))

    worksheetOrder(wb) <- c(1, num_sheets, 2:(num_sheets - 1))

    # add black line to bottom of headers
    writeData(
      wb,
      x = meta,
      sheet = sht_nm[1],
      startCol = 1,
      startRow = 1,
      headerStyle = sstyles$styl_brd_head
    )

    # --- column numbers for filter pads, HPLC and CDOM
    cols <-
      list(
        # date columns
        date = which(str_detect(names(meta), "(?i)date")),

        # filter pad columns
        pads = seq(
          which(str_detect(names(meta), "ap.*process date")),
          max(which(str_detect(names(meta), "(?i)acdom.*#.*Sample.Raw")))
        ),

        # HPLC columns
        hplc = which(str_detect(names(meta), "(?i)hplc sample id")),

        # cdom columns
        cdom = seq(
          which(str_detect(names(meta), "(?i)cdom.*process date")),
          max(which(str_detect(names(meta), "(?i)acdom.*#.*Sample.Raw")))
        )
      )

    # ---- Specify Width and Heights
    format_styles(
      wb,
      sht_nm,
      cols,
      names(meta),
      type = "meta_col_wh"
    )

    # ---- Add Styles
    format_styles(
      wb,
      sht_nm     = sht_nm,
      names_meta = names(meta),
      dim_meta   = dim(meta),
      cols       = cols,
      sstyles    = sstyles,
      type       = "meta"
    )
  }

  # ========================================================================== #
  # ---- Update Chl-a Sheet ----
  # ========================================================================== #  
  
  old_log <-
    prev_log %>%
    read.xlsx(
      sheet       = prev_sht[2],
      sep.names   = " ",
      detectDates = TRUE,
      startRow    = 3
    ) %>%
    select(
      contains(c("Sample ID", "Process date")),
      contains("Volume methanol (ml)"):last_col()
    )
  
  meta <-
    x %>%
    filter(str_detect(sample_type, "Chl-a")) %>%
    select(
      date_mm_dd_yy, sample_collection_time_gmt,
      lat, lon, max_depth, depth_m,
      station, identifier, vol_ml
    ) %>%
    mutate(
      .keep = "none",
      "Date (UTC)" = date_mm_dd_yy,
      "Time (UTC)" = sample_collection_time_gmt,
      "Latitude (deg. N)" = lat,
      "Longitude (deg. W)" = lon,
      "Bottom Depth (m)" = max_depth,
      "Sample Depth (m)" = depth_m,
      "Station ID" = station,
      "Sample ID" = identifier,
      "Volume seawater (ml)" = vol_ml
    )
  
  hdr_name <- names(meta)
  
  if (nrow(meta) != nrow(old_log)) stop("Issues with number of rows!")
  
  meta <-
    meta %>%
    left_join(old_log, .,
              by = "Sample ID"
    ) %>%
    select(hdr_name, everything()) %>%
    mutate(
      ranks = str_remove(`Sample ID`, ".*-"),
      ranks = str_remove_all(ranks, "[:alpha:]"),
      ranks = as.numeric(ranks)
    ) %>%
    arrange(ranks) %>%
    select(-ranks) %>%
    relocate(contains("Process date"),
             .before = contains("volume seawater")
    )
  
  # ========================================================================== #
  # ---- Updating Chl-a Sheet ----
  # ========================================================================== #
  cli_alert_info("Updating worksheet: {.var {prev_sht[2]}}")
  
  sht_nm  <- prev_sht[2]
  old_sht <- paste0(old_sht_rn, prev_sht[2], "_update_on", Sys.Date())
  

  cloneWorksheet(wb, clonedSheet = sht_nm, sheetName = old_sht)
  
  activeSheet(wb) <- which(names(wb) == sht_nm)
  
  # add data
  writeData(
    wb,
    x        = select(meta, 1:contains("sample id")),
    sheet    = sht_nm[1],
    startCol = 1,
    startRow = 4,
    colNames = FALSE
  )
  
  writeData(
    wb,
    x        = select(meta, contains("volume seawater")),
    sheet    = sht_nm[1],
    startCol = which(str_detect(names(meta), "(?i)volume seawater")),
    startRow = 4,
    colNames = FALSE
  )
  
  if (!.keep_wksht) {
    cli_alert_info("Removing old sheet: {col_red(old_sht)}")
    removeWorksheet(wb, sheet = old_sht)
  }
  
  cli_alert_success("Finishing!\n----\n\n")
  
  return(wb)

  # ---- end of function update_proc_log
}

