# ============================================================================ #
# ---- header ----
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
#' @param type The format to read each column
#'             - `c` for character
#' @param add_na Other NAs that may exists in the file.
#'               - default: c("", "NA")
#'
#' @return RETURN_DESCRIPTION
#' @examples
#' # ADD_EXAMPLES_HERE
read_sb_files <- function(x, type = NULL, add_na = NULL) {
  
  na_val <- unique(c("", "NA", add_na))
  
  if (is.null(type)) {
    type <- cols(.default = col_guess())
  } else {
    type <- cols(.default = type)
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
  
  read_csv(
    x,
    skip           = skips,
    col_names      = col_names,
    show_col_types = FALSE,
    col_types      = type,
    na             = na_val
  )
  
  # ---- end of function read_sb_files
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
#' @param verb print the results of each checked file automatically
#'              Note: can call using <var>$fcheck2
#'
#' @return RETURN_DESCRIPTION
#' @examples
#' # ADD_EXAMPLES_HERE
sb_fcheck <- function(
    sb_files,
    col_name   = "files",
    loc_fcheck = here(),
    verb       = FALSE) {
  
 
  dir_loc  <- here(loc_fcheck, "fcheck")
  col_name <- rlang::ensym(col_name)
    
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
  
  message("Starting FCHECK")
  
  fcheck_results <- 
    sb_files %>%
    mutate(
      fcheck2 = map(
        {{ col_name }},
        \(x) paste(here(.env$dir_loc, "fcheck4.pl"), x) %>%
          system2("perl", ., stdout = TRUE)
      )
    ) %T>% {
      if (verb) {
        pull(., fcheck2) %>%
          print()
      } else {
        .
      }
    }

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
