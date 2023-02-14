process_log_sheet <- function(.meta, cal, cruise,
                              sht_nm = c("metadata", "Chl-a"), 
                              inst_nm = "PElam850+",
                              creator = "Sebastian Di Geronimo") {
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

  # ============================================================================ #
  # ---- Load Libraries ----
  # ============================================================================ #  
  librarian::shelf(
    librarian, ggplot2, tibble, tidyr, readr, purrr, dplyr, stringr,
    forcats, lubridate, glue, fs, magrittr, here,
    # broom # optional
    
    # additional
    openxlsx
  )
  
  # ========================================================================== #
  # ---- Filter Data ----
  # ========================================================================== #  
  cli::cli_alert_info("Filtering Data")
  .meta <- .meta %>%
    select(-1, -sample_number) %>%
    mutate(sample_collection_time_gmt = hms::as_hms(sample_collection_time_gmt)) 

  # ---- Format 1st Sheet Info ----
  meta <-
    .meta %>%
    filter(!str_detect(sample_type, "HPLC")) %>%
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
      names_repair = janitor::make_clean_names
    ) %>%
    unnest(c(chl_a, cdom)) %>%
    group_by(station, depth_m) %>%
    mutate(cdom = replace(cdom, duplicated(cdom), NA)) %>%
    ungroup() %>%
    transmute(
      "Date (UTC)"         = date_mm_dd_yy,                                
      "Time (UTC)"         = sample_collection_time_gmt,
      "Latitude (deg. N)"  = lat,
      "Longitude (deg. W)" = lon,
      "Bottom Depth (m)"   = max_depth,
      "Sample Depth (m)"   = depth_m,
      "Station ID"         = station,
      "Instrument"         = inst_nm,
      "Sample ID"          = chl_a,
      "CDOM Sample ID"     = cdom
    ) %>%
    mutate(
      "ap,d(λ) process date"                       = NA_Date_,
      "Instrument"                                 =  "PElam850+",
      "ap(λ) filename (*.Sample.Raw*)"             = NA_character_,
      "ad(λ) filename (*.Sample.Raw*)"             = NA_character_,
      "CDOM process date"                          = NA_Date_,
      "Instrument "                                =  "PElam850+",
      "aCDOM(λ) filename, Rep. #1 (*.Sample.Raw*)" = NA_character_,
      "aCDOM(λ) filename, Rep. #2 (*.Sample.Raw*)" = NA_character_,
      "aCDOM(λ) filename, Rep. #3 (*.Sample.Raw*)" = NA_character_,
      "Comments"                                   = NA_character_
    ) %>%
    relocate("ap,d(λ) process date", 
             .before = Instrument) %>%
    relocate("CDOM Sample ID", 
             .after = "Instrument ")
  
  # ---- Format 2nd Sheet Info ----
  chl_a <-
    .meta %>%
    filter(str_detect(sample_type, "Chl-a")) %>%
    select(
      date_mm_dd_yy, sample_collection_time_gmt, lat, lon, max_depth, depth_m, 
      station, identifier, vol_ml)  %>%
    transmute(
      "Date (UTC)"         = date_mm_dd_yy,                                
      "Time (UTC)"         = sample_collection_time_gmt,
      "Latitude (deg. N)"  = lat,
      "Longitude (deg. W)" = lon,
      "Bottom Depth (m)"   = max_depth,
      "Sample Depth (m)"   = depth_m,
      "Station ID"         = station,
      "Sample ID"          = identifier,
      "Volume seawater (ml)" = vol_ml
    ) %>%
    mutate(
      "Process date"                 = NA_Date_,
      "Volume methanol (ml)"         = NA_real_,
      "'No Acid' Rb"                 = NA_real_,
      "Fluorometer Calibration date" = cal$cal_date,
      "No Acid' blank"               = NA_real_,
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
  
  # ========================================================================== #
  # ---- Setup ----
  # ========================================================================== #  
  cli::cli_alert_info("Setup")
  # ---- Styles for formatting ----
  sstyles <-
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
      styl_border   = createStyle(border       = "Right", 
                                  borderStyle  = "thin"),
      styl_brd_bot  = createStyle(border       = "bottom", 
                                  borderStyle     = "thin"),
      styl_brd_head = createStyle(border          = "bottom", 
                                  borderStyle    = "thin",
                                  textDecoration = "bold",
                                  halign         = "center",
                                  wrapText       = TRUE),
      styl_align    = createStyle(halign = "center"),
      styl_brd_dash = createStyle(border      = "right", 
                                  borderStyle = "dotted")
    ) 
  
  # ---- Comments for Chlor-a Sheet ----
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
  
  # ---- Formulas for Chlor-a Sheet ----
  # =IF(R4>0,((Q4/R4)+(S4/T4))/2,-999)
  no_acid_ratio <- paste0(
    "IF(", paste0("R", 1:nrow(chl_a) + 3), ">0,((", paste0("Q", 1:nrow(chl_a) + 3), 
    "/", paste0("R", 1:nrow(chl_a) + 3), ")+(", paste0("S", 1:nrow(chl_a) + 3), 
    "/", paste0("T", 1:nrow(chl_a) + 3), "))/2,-999)")
  
  # V
  # =+IF(L4>0,(O4*((L4-N4)*U4)+P4)*K4/J4,-999)
  chla_conc <- 
    paste0("+IF(", paste0("L", 1:nrow(chl_a) + 3),">0,(", 
           paste0("O", 1:nrow(chl_a) + 3),"*((",paste0("L", 1:nrow(chl_a) + 3),
           "-",paste0("N", 1:nrow(chl_a) + 3),")*",paste0("U", 1:nrow(chl_a) + 3),
           ")+",paste0("P", 1:nrow(chl_a) + 3),")*",paste0("K", 1:nrow(chl_a) + 3),
           "/",paste0("J", 1:nrow(chl_a) + 3),",-999)")
  
  formulas <- 
    tibble(
      # W
      # =AVERAGE(V4:V5)
      # avg = paste0("AVERAGE(",paste0("V", seq(1, nrow(chl_a), 2) + 3), ":",
      #              paste0("V", seq(1, (nrow(chl_a)), 2) + 4),")"),
      avg = paste0(
        "IF(AND(", paste0("V", seq(1, nrow(chl_a), 2) + 3), ">=0, ", 
        paste0("V", seq(1, nrow(chl_a), 2) + 4), ">=0), ",
        paste0("AVERAGE(",paste0("V", seq(1, nrow(chl_a), 2) + 3), ":",
               paste0("V", seq(1, nrow(chl_a), 2) + 4),")"), ", \"\")"
      ),
      # X
      # =STDEV(V4:V5)
      # stdev = paste0("STDEV(",
      #                paste0("V", seq(1, nrow(chl_a), 2) + 3), ":", 
      #                paste0("V", seq(1, nrow(chl_a), 2) + 4),")"),
      stdev = paste0(
        "IF(AND(", paste0("V", seq(1, nrow(chl_a), 2) + 3), ">=0, ", 
        paste0("V", seq(1, nrow(chl_a), 2) + 4), ">=0), ",
        paste0("STDEV(",paste0("V", seq(1, nrow(chl_a), 2) + 3), ":",
               paste0("V", seq(1, nrow(chl_a), 2) + 4),")"), ", \"\")"
      ),
    )  %>%
    bind_rows(tibble(avg = rep(NA, nrow(chl_a) / 2))) %>%
    mutate(rows = rep(1:(nrow(chl_a) / 2), 2)) %>%
    arrange(rows, !is.na(avg), avg) %>%
    select(-rows) %>%
    mutate( 
      # col U 
      no_acid_ratio = no_acid_ratio,
      # col V
      chla_conc = chla_conc) 
  
  # ========================================================================== #
  # ---- Create 1st Sheet ----
  # ========================================================================== #  
  cli::cli_alert_info("Creating sheet: {.var {sht_nm[1]}}")
  # ---- Create Worksheet ----
  wb <- createWorkbook(
    creator = creator, 
    title = glue("{cruise} Process Log"))
  
  # ---- Add Info ----
  addWorksheet(wb, sheetName = sht_nm[1])
  
  writeData(wb, sheet = sht_nm[1], startCol = 1, startRow = 1, 
            x = meta, headerStyle = sstyles$styl_brd_head)
  
  writeData(wb, sheet = sht_nm[1], startCol = "H", startRow = 1, 
            x = meta[,8:12], headerStyle = sstyles$styl_col1)
  
  writeData(wb, sheet = sht_nm[1], startCol = "M", startRow = 1, 
            x = meta[,13:18], headerStyle = sstyles$styl_col2)
  
  writeData(wb, sheet = sht_nm[1], startCol = "S", startRow = 1, 
            x = meta[, 19], headerStyle = sstyles$styl_brd_head)
  
  # ---- Specify Width and Heights ----
  setRowHeights(wb, sheet = sht_nm[1], rows = 1, heights = 73.2)
  setColWidths(wb, sheet = sht_nm[1], cols = "S", widths = 39.56)
  
  # ---- Add Styles ----
  options(openxlsx.dateFormat = "mm/dd/yy")
  addStyle(wb, sht_nm[1],
           style      = sstyles$styl_date,
           rows       = 1:(nrow(meta) + 1),
           cols       = 1,
           gridExpand = TRUE,
           stack      = TRUE)
  
  addStyle(wb, sht_nm[1],
           style      = sstyles$styl_time,
           rows       = 1:nrow(meta) + 1,
           cols       = 2,
           gridExpand = TRUE,
           stack      = TRUE)
  
  addStyle(wb, sht_nm[1],
           style      = sstyles$styl_num,
           rows       = 1:nrow(meta) + 1,
           cols       = 5:6,
           gridExpand = TRUE,
           stack      = TRUE)
  
  addStyle(wb, 
           sheet      = sht_nm[1], 
           style      = sstyles$styl_col1,
           cols       = 8:12,
           rows       = 1:nrow(meta) + 1,
           gridExpand = TRUE,
           stack      = TRUE)
  
  addStyle(wb, 
           sheet      = sht_nm[1], 
           style      = sstyles$styl_col2,
           cols       = 13:18,
           rows       = 1:nrow(meta) + 1,
           gridExpand = TRUE,
           stack      = TRUE)
  
  addStyle(wb, 
           sheet      = sht_nm[1], 
           style      = sstyles$styl_border,
           cols       = c("G", "L", "S", "R"),
           rows       = 1:(nrow(meta) + 1),
           gridExpand = TRUE, 
           stack      = TRUE)
  
  addStyle(wb, 
           sheet      = sht_nm[1], 
           style      = sstyles$styl_brd_bot,
           cols       = 1:19,
           rows       = nrow(meta) + 1,
           gridExpand = TRUE, 
           stack      = TRUE)
  
  addStyle(wb, 
           sheet      = sht_nm[1], 
           style      = sstyles$styl_align,
           cols       = 1:19,
           rows       = 1:nrow(meta) + 1,
           gridExpand = TRUE, 
           stack      = TRUE)
  
  # ========================================================================== #
  # ---- Create 2nd Sheet ----
  # ========================================================================== # 
  cli::cli_alert_info("Creating sheet: {.var {sht_nm[2]}}")
  # ---- Add sheet ----
  addWorksheet(wb, sheetName = sht_nm[2])
  
  # ---- Add Large Header ----
  mergeCells(wb, sht_nm[2], cols = 9:24, rows = 1)
  writeData(wb, sht_nm[2], "Trilogy Fluorometer", startCol = 9, startRow = 1, 
            colNames = FALSE)
  addStyle(wb, sht_nm[2], rows = 1, cols = 9:24, stack = TRUE, 
           style = sstyles$styl_col3)
  
  # ---- Add Smaller Headers ----
  mergeCells(wb, sht_nm[2], cols = 9:12,  rows = 2)
  mergeCells(wb, sht_nm[2], cols = 13:16, rows = 2)
  mergeCells(wb, sht_nm[2], cols = 17:21, rows = 2)
  mergeCells(wb, sht_nm[2], cols = 22:24, rows = 2)
  
  writeData(wb, sht_nm[2], "Secondary Solid Standard", 
            startCol = 17, startRow = 2, colNames = FALSE)
  writeData(wb, sht_nm[2], "Welschmeyer (no acid)", 
            startCol = 22, startRow = 2, colNames = FALSE)
  
  
  # ---- Add Info ----
  writeData(wb, sheet = sht_nm[2], startCol = 1, startRow = 3, x = chl_a,
            headerStyle = sstyles$styl_brd_head)
  
  # ---- Add Styles ----
  options(openxlsx.dateFormat = "mm/dd/yy")
  addStyle(wb, sht_nm[2],
           style      = sstyles$styl_date,
           rows       = 3:(nrow(meta) + 3),
           cols       = 1,
           gridExpand = TRUE,
           stack      = TRUE)
  
  addStyle(wb, sht_nm[2],
           style      = sstyles$styl_date,
           rows       = 3:(nrow(meta) + 3),
           cols       = "M",
           gridExpand = TRUE,
           stack      = TRUE)
  
  addStyle(wb, sht_nm[2],
           style      = sstyles$styl_time,
           rows       = 3:(nrow(meta) + 3),
           cols       = 2,
           gridExpand = TRUE,
           stack      = TRUE)
  
  
  
  addStyle(wb, sht_nm[2], rows = 2:(nrow(chl_a) + 3), cols = 9:24, stack = TRUE,
           style = sstyles$styl_col4, gridExpand = TRUE)
  
  addStyle(wb, sht_nm[2], 
           style      = sstyles$styl_border, 
           rows       = 2:(nrow(chl_a) + 3), 
           cols       = c("H", "L", "P", "U", "X", "Y"), 
           stack      = TRUE,
           gridExpand = TRUE)
  
  addStyle(wb, sht_nm[2], 
           style      = sstyles$styl_brd_dash, 
           rows       = 3:(nrow(chl_a) + 3), 
           cols       = "T", 
           stack      = TRUE,
           gridExpand = TRUE)
  
  addStyle(wb, sht_nm[2], 
           style      = sstyles$styl_brd_bot,
           rows       = nrow(chl_a) + 3,
           cols       = 1:ncol(chl_a), 
           stack      = TRUE)
  
  addStyle(wb, sht_nm[2],
           style      = sstyles$styl_num,
           rows       = 1:nrow(meta) + 1,
           cols       = 5:6,
           gridExpand = TRUE,
           stack      = TRUE)
  
  # ---- Specify Width and Heights ----
  setRowHeights(wb, sheet = sht_nm[2], rows = 1, heights = 29.3)
  setRowHeights(wb, sheet = sht_nm[2], rows = 2, heights = 14.4)
  setRowHeights(wb, sheet = sht_nm[2], rows = 3, heights = 57)
  setColWidths(wb, sheet = sht_nm[2], cols = "Y", widths = 40.56)
  setColWidths(wb, sheet = sht_nm[2], cols = "H", widths = 12)
  
  # ---- Add formulas to Cells ----
  writeFormula(wb, sht_nm[2],
               formulas$no_acid_ratio, 
               startCol = "U",
               startRow = 4)
  writeFormula(wb, sht_nm[2],
               formulas$chla_conc, 
               startCol = "V",
               startRow = 4)
  writeFormula(wb, sht_nm[2],
               formulas$avg, 
               startCol = "W",
               startRow = 4)
  writeFormula(wb, sht_nm[2],
               formulas$stdev, 
               startCol = "X",
               startRow = 4)
  
  # ---- Write Comments ----
  writeComment(wb, sht_nm[2], col = "L", row = 4, comment = comm$fluor)
  writeComment(wb, sht_nm[2], col = "N", row = 4, comment = comm$blank)
  writeComment(wb, sht_nm[2], col = "R", row = 4, comment = comm$low_read)
  writeComment(wb, sht_nm[2], col = "T", row = 4, comment = comm$high_read)
  
  
  # ---- Freeze Pane ----
  freezePane(wb, sht_nm[2], firstActiveRow = 4)
  
  # ---- Reorder Sheets ----
  worksheetOrder(wb) <- c(2, 1)
  
  return(wb)
  # ---- end of function ----
  }

library("docstring")
docstring(process_log_sheet)


