# ============================================================================ #
# 
# 
# ---- Check SeaBASS Data ----
# 
# 
# ============================================================================ #  

##%######################################################%##
#                                                          #
####        Read SeaBASS Files and Extract Data         ####
#                                                          #
##%######################################################%##
#' Read SeaBASS Files and Extract Data
#'
#' FUNCTION_DESCRIPTION
#'
#' @param x Tibble of file paths.
#' @param sample_type default: ag, ap, chl, or HPLC
#' @param col_types The format to read each column
#'             - `c` for character
#' @param add_na Other NAs that may exists in the file.
#'               - default: c("", "NA")
#'
#' @author Sebastian Di Geronimo (August 25, 2023)
#'
#' @rdname read_sb_files
#'
#' @export
#' 
#' @return RETURN_DESCRIPTION
#' @examples
#' # ADD_EXAMPLES_HERE
#' 
read_sb_files <- function(
    x, 
    sample_type = c("default", "hplc"), 
    col_types   = NULL, 
    add_na      = NULL,
    show_col_types = FALSE,
    ...) {
  
  na_val <- unique(c("", "NA", add_na))
  
  if (is.null(col_types)) {
    col_types <- cols(.default = col_guess())
  } else {
    col_types <- cols(.default = col_types)
  }
  
  skips <-
    read_lines(x) %>% 
    str_detect("end_header") %>%
    which()
  
  head <-
    read_lines(x) %>% 
    str_detect("fields") %>%
    which()
  
  col_names <-
    read_csv(
      x,
      skip           = head - 1,
      n_max          = 1,
      col_names      = FALSE,
      show_col_types = FALSE
    ) %>%
    mutate(X1 = str_remove(X1, "/fields=")) %>%
    as.character()

  sep_pattern <- rep(c("\\S+", "\\s+"), length(col_names))
  names(sep_pattern) <-  
    map(col_names, \(x) c(x, NA)) %>%
    list_c()
  
  x2 <- list(
    file           = x,
    skips          = skips,
    col_names      = col_names,
    show_col_types = show_col_types,
    col_types      = col_types,
    na_val         = na_val,
    sep_pattern    = sep_pattern
  )
  
  class(x2) <- sample_type

  UseMethod("read_sb_files", x2)
  
  # ---- end of function read_sb_files
}

#' @rdname read_sb_files
read_sb_files.hplc <- function(...) {
  x <-
  read_csv(
    file           = x2$file,
    skip           = x2$skips,
    col_names      = x2$col_names,
    col_types      = x2$col_types,
    na             = x2$na_val,
    show_col_types = x2$show_col_types
  )
  
  return(x)
  
  # ---- end of function read_sb_files.hplc
}

#' @rdname read_sb_files
read_sb_files.default <- function(...) {
  x <- 
    read_lines(
    file = x2$file,
    skip = x2$skips,
    na   = x2$na_val
  ) %>%
  tibble(data = .) %>%
  separate_wider_regex(
    cols = data,
    patterns = x2$sep_pattern,
    too_few = "align_start")
  
  return(x)
  # ---- end of function read_sb_files.default
}

##%######################################################%##
#                                                          #
####           FCHECK for SeaBASS Submissions           ####
#                                                          #
##%######################################################%##
#' FCHECK for SeaBASS Submissions
#'
#' Using FCHECK from SeaBASS, will check if the seabass files (.sb) have errors
#' or warnings that need adressing
#'
#' @param sb_files A tibble of files with .sb extension
#' @param col_name the column name where file paths are located
#' @param loc_fcheck where to look for the fcheck folder or download to if 
#'                   doesn't exist
#' @param row_check the amount of files to go through before pausing to review
#'                  the output
#'                  - default = NULL: no pause
#'                  - 1 to n rows
#'                  - "end" to pause at the end
#' @param verb print the results of each checked file automatically
#'              Note: can call using <var>$fcheck2
#'
#' @return RETURN_DESCRIPTION
#' @examples
#' # ADD_EXAMPLES_HERE
sb_fcheck <- function(
    sb_files,
    col_name = "files",
    loc_fcheck = here(),
    row_check = NULL,
    verb = FALSE) {
  
  # exxtract location of fcheck files and colname within sb_files
  dir_loc  <- here(loc_fcheck, "fcheck")
  col_name <- rlang::ensym(col_name)

  # capture number of rows before pausing
  row_check <- ifelse(is.null(row_check) | is.na(row_check), NA, row_check)
  row_check <- 
    ifelse(
      !is.null(row_check)
      && is.character(row_check) 
      && row_check == "end",
      nrow(sb_files),
      row_check
      )

  row_check <- ifelse(is.na(row_check), nrow(sb_files) + 1, row_check)
  row_check <- as.integer(row_check)
  
  # look for Perl on machine
  tryCatch(
    {
      system2("perl", "-v", stdout = TRUE)
      invisible()
    },
    error = function(e) {
      stop(glue(
        "The programming language \"Perl\" does not seem to be ",
        "downloaded to your computer.\n",
        "\"Perl\" is used to run the `fcheck4.pl` script to check issues ",
        "with SeaBASS files (.sb).\n",
        "----\n",
        "To continue checking .sb files:\n",
        "Download perl at: ",
        "<https://www.perl.org/get.html>\n",
        "----\n"
      ))
    }
  )

  # download fcheck if it doens't exists in the location set
  # <https://seabass.gsfc.nasa.gov/wiki/FCHECK/fcheck4.tar>
  if (!dir_exists(here(loc_fcheck, "fcheck")) |
    !file_exists(here("fcheck", "fcheck4.pl"))) {
    url <- "https://seabass.gsfc.nasa.gov/wiki/FCHECK/fcheck4.tar"

    message(
      glue_col(
        "The directory `fcheck` does not exists in your project repo.\n",
        "If you type `2` in the console, this will create the repo in ",
        "{blue {dir_loc}}\n",
        "and download `fcheck` from {blue {url}}\n",
        "<ESC> will cancel."
      )
    )

    x <- menu(c("No, stop download", "Yes, continue download"))

    if (x != 2) {
      return(message("Not downloading `fcheck`"))
    }


    message("Creating `fcheck` folder and downloading `fcheck.pl`")

    dir_create(dir_loc)

    file <- here(dir_loc, basename(url))

    # download
    message(glue_col("Downloading to {blue {dir_loc}}"))
    download.file(
      url      = url,
      destfile = file,
      method   = "curl"
    )

    # unzip
    message(glue_col("Unzipping to {blue {dir_loc}}"))
    untar(
      tarfile = file,
      exdir   = dir_loc
    )

    message("Finished downloading and unzipping!")
  }

  message("\n\nStarting FCHECK")
  cat("\n\n---------\n\n")

  fcheck_results <-
    sb_files %>%
    mutate(
      fcheck2 = map2(
        {{ col_name }}, row_number(),
        \(x, y) {
          rows <- .env$row_check

          print(glue::glue(
            "File Number: {y} of {nrow(.env$sb_files)}\n",
            "File: {basename(x)}",
            "\n\n---------\n\n"
          ))
          results <-
            paste(here(.env$dir_loc, "fcheck4.pl"), x) %>%
            system2("perl", ., stdout = TRUE)

          if (verb) {
            cat(results, "\n", sep = "\n")

            if (rows <= nrow(.env$sb_files) &
              y == nrow(.env$sb_files)) {
              rows <- y
            }

            if (interactive() &
              y %% rows == 0
            ) {
              message(paste(
                "This is a momentary pause to check over the",
                "previous", row_check, "files."
              ))
              message("Hit [Enter} to continue.")
              readline()
            }
          }

          return(results)
        }
      )
    )

  # print file and summary of errors/warnings
  for (i in seq(nrow(fcheck_results))) {
    print(
      with(
        fcheck_results,
        glue(basename(files[[i]]),
          fcheck2[[i]][which(str_detect(
            fcheck2[[i]],
            "\\d+.*error"
          ))],
          .sep = ": "
        )
      )
    )
  }

  return(fcheck_results)

  # ---- end of function sb_fcheck
}

