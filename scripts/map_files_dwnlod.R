##%######################################################%##
#                                                          #
####         Download World Shapefiles for Maps         ####
#                                                          #
##%######################################################%##
#' Download World Shapefiles for Maps
#'
#' This function will download topography, state lines, and coastline from
#' NOAA if it does not exist at the designated path.
#' 
#' The topography should be re-downloaded for new projects since it's only a 
#' subset of the world based on the extent given.
#' 
#' The land boundaries only need to be downloaded once, so specifying where it's 
#' located may be a better option. 
#' 
#' Location of data:
#' Topography, ETOPO1, 0.0166667 degrees, Global (longitude -180 to 180), 
#' (Ice Sheet Surface) from https://coastwatch.pfeg.noaa.gov/erddap/griddap/
#' 
#' Global Self-consistent, Hierarchical, High-resolution Geography Database
#' (GSHHG) from https://www.ngdc.noaa.gov/mgg/shorelines/
#'
#' @param path_land File directory to be used for coastline files. This may need to be
#'                  created or already exists. This is where the land map will be
#'                  downloaded to if it doesn't exists.
#' @param path_topo File directory to be used for topgraphy file. This may need to be
#'                  created or already exists. This is where the topo map will be
#'                  downloaded to if it doesn't exists. This will require a spatial
#'                  extent to subset the data
#' @param extent Spatial extent for the topography data.
#'               Format: exnt <- c(xmin = -82,    # West
#'                                 xmax = -80,    # East
#'                                 ymin = 24.25,  # South
#'                                 ymax = 25.75   # North
#'                                 ) 
#' @param file_suffix Suffix to end of topography name
#'
#' @author Sebastian Di Geronimo (2023-01-11 14:55:54)
#' 
#' @return NULL, saves files
#' @examples
#' # ADD_EXAMPLES_HERE
#' 
world_download <- function(
    path_land   = NULL,
    path_topo   = path_land,
    extent      = NULL,
    file_suffix = NULL) {
  # ========================================================================== #
  # ---- Load libraries ----
  # ========================================================================== #
  library("here")
  library("fs")
  library("rerddap")
  library("cli")
  library("rlang")
  
  # ========================================================================== #
  # ---- Create directories for files ----
  # ========================================================================== #
  dir_create(path_land)
  dir_create(here(path_topo, "temp"))

  # ========================================================================== #
  # ---- Download topography for the specific location ----
  # ========================================================================== #
  # TODO: add suffix to name to specify spatial location
  # space <- paste(as.character(round(extent)), sep = "_")
  # unite(as.character(round(extent)), col = "one", c(1:4))
  # topo_file <- here(path_topo,"etopo1_{space}.nc")

  topo_file <- here(
    path_topo,
    glue("etopo1{file_suffix}.nc",
      .null = ""
    )
  )

  if (length(extent) != 4) {
    cli_abort(c(
      "{.var extent} needs to have 4 values.",
      "x" = "You supplied {length(extent)} values.",
      "Format Example: 
            exnt <- c(xmin = -82,xmax = -80, ymin = 24.25, ymax = 25.75) "
    ))
  }

  if (!file.exists(topo_file)) {
    cli_alert_info("Topography data doesn't exists in {.file {path_topo}}")
    cli_alert_info(c(
      "Downloading Topography Data\n",
      "Longitude: {extent[1]}, {extent[2]} \n",
      "Latitude:  {extent[3]}, {extent[4]} \nURL: ",
      "{.url https://coastwatch.pfeg.noaa.gov/erddap/griddap/}"
    ))

    # ERDDAP extract and save
    griddap(
      info("etopo180"),
      latitude  = extent[3:4],
      longitude = extent[1:2],
      stride    = c(1, 1),
      fields    = "altitude",
      store     = disk(here(path_topo, "temp"))
    )

    # Move and rename .nc file
    file_move(
      dir_ls(here(path_topo, "temp"), regexp = "\\.nc$"),
      topo_file
    )
    
    # delete temp folder
    dir_delete(here(path_topo, "temp"))

    cli_alert_success("Downloaded etopo1.nc")
    
  } else {
    cli_alert_info(c(
      "Topography data exists in {.file {path_topo}}.\n",
      "You may need to {.emph {col_red('check')}} the spatial",
      " extent."
    ))
  }

  # ========================================================================== #
  # ---- Download GSHHS Coastline Shapefile ----
  # ========================================================================== #
  # download GSHHS shapefile if not already downloaded
  coast <-
    dir_ls(
      path    = path_land,
      recurse = TRUE,
      regexp  = "GSHHS_h_L1.shp"
    )

  if (rlang::is_empty(coast)) {
    cli_alert_info(c(
      "GSHHS_h_L1 Coastline data doesn't exists in ",
      "{.file {path_land}}"
    ))
    cli_alert_info(c(
      "Downloading Coastline Shapefile\nURL:",
      "{.url https://www.ngdc.noaa.gov/mgg/shorelines/}"
    ))

    temp <- tempfile()
    download.file(
      "ftp://ftp.soest.hawaii.edu/gshhg/gshhg-shp-2.3.7.zip",
      temp,
      method = "libcurl",
      mode = "wb"
    )
    
    unzip(exdir = path_land, temp)
    # delete temp folder and files
    unlink(temp)
    
  } else {
    cli_alert_info("Coastline shapefile exists in {.file {path_land}}.\n")
  }

  return(invisible(NULL))
  # ---- End of Function ----
}

##%######################################################%##
#                                                          #
####                  Load Map Objects                  ####
#                                                          #
##%######################################################%##
#' Load Map Objects
#'
#' FUNCTION_DESCRIPTION
#'
#' @param .map_coast DESCRIPTION.
#' @param .map_bath DESCRIPTION.
#' @param .extent DESCRIPTION.
#'
#' @return RETURN_DESCRIPTION
#' @examples
#' # ADD_EXAMPLES_HERE
load_map_obj <- function(
    .map_coast, 
    .map_bath = .map_coast, 
    .extent) {
  
  # ---- libraries
  library("sf")
  library("raster")
  library("fs")
  
  # ---- coastline
  coast_topo <-
    dir_ls(
      path    = .map_coast,
      recurse = TRUE,
      regexp  = "GSHHS_h_L1.shp"
    ) %>%
    st_read(.) %>%
    st_crop(
      .,
      st_bbox(
        .extent
      )
    )
  
  # ---- state lines
  state <-
    dir_ls(
      path    = .map_coast,
      recurse = TRUE,
      regexp  = "WDBII_border_h_L2.shp"
    ) %>%
    st_read(.) %>%
    st_crop(
      .,
      st_bbox(
        .extent
      )
    )
  
  # ---- bathymetry
  bathy <-
    here(.map_bath, "etopo1.nc") %>%
    raster::raster() %>%
    as.data.frame(xy = TRUE)
  
  
  return(
    list(
      coast_topo = coast_topo,
      state      = state,
      bathy      = bathy
    )
  )
  # ---- End of Function ----
}


##%######################################################%##
#                                                          #
####                   Base Map Plot                    ####
#                                                          #
##%######################################################%##
#' Base Map Plot 
#'
#' FUNCTION_DESCRIPTION
#'
#' @param topo DESCRIPTION.
#' @param bathy DESCRIPTION.
#' @param extnt DESCRIPTION.
#'
#' @return RETURN_DESCRIPTION
#' @examples
#' # ADD_EXAMPLES_HERE
base_map_plot <- function(
  .topo,
  .bathy,
  .extent
  ) {
  
  # ---- libraries
  library("ggplot2")
  library("metR")
  
  # ---- plot 
  ggplot() +
    geom_sf(data = .topo) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0), breaks = seq(24.5, 28.0, 0.5)) +
    coord_sf(xlim = .extent[1:2], ylim = .extent[3:4]) +
    labs(
      x = NULL,
      y = NULL,
    ) +
    geom_contour2(
      data = .bathy,
      aes(
        x = x,
        y = y,
        z = -Altitude
      ),
      col = "grey70",
      breaks = c(100, 50, 25, 10, 0)
    ) +
    directlabels::geom_dl(
      data = filter(
        .bathy,
        between(x, -84, -81.2)
        & between(y, 25, 25.5)
      ),
      aes(
        x = x, 
        y = y, 
        z = -Altitude,
        label = ..level..
      ), 
      method = list(
        "bottom.pieces",
                    fontfamily = "serif",
                    alpha = 0.5,
                    hjust = 1.5),
      stat   = "contour",
      breaks = c(100, 50, 25, 10, 0)
    ) +
    theme_bw() +
    theme(
      text = element_text(family = "serif", size = 10),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
  # ---- End of Function ----
}

##%######################################################%##
#                                                          #
####                     Inset Map                      ####
#                                                          #
##%######################################################%##
#' Inset Map
#'
#' FUNCTION_DESCRIPTION
#'
#' @param .topo DESCRIPTION.
#' @param .state_line DESCRIPTION.
#' @param .extent DESCRIPTION.
#'
#' @return RETURN_DESCRIPTION
#' @examples
#' # ADD_EXAMPLES_HERE
map_inset <- function(
    .topo, 
    .state_line, 
    .extent) {
  
  # ---- plot
  ggplot() +
    geom_sf(data = .topo) +
    geom_sf(data      = .state_line, 
            color     = "grey40", 
            linewidth = 0.2) +

    # create red box on map to show sampling locations
    geom_rect(
      aes(
        xmin = .extent[1] - 0.4, # West
        xmax = .extent[2] - 0.4, # East
        ymin = .extent[3] - 0.4, # South
        ymax = .extent[4] - 0.4  # North
      ),
      color = "red",
      fill  = NA
    ) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    theme_bw() +
    theme(
      axis.text.x      = element_blank(),
      axis.text.y      = element_blank(),
      axis.ticks       = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.margin      = grid::unit(c(0, 0, 0, 0), "in"),
      panel.border     = element_rect(
        linetype = "solid",
        color    = "black",
        fill     = NA
      )
    )
  # ---- End of Function ----
}


##%######################################################%##
#                                                          #
####                Plot Samples on Map                 ####
#                                                          #
##%######################################################%##
#' Plot Samples on Map
#'
#' FUNCTION_DESCRIPTION
#'
#' @param .base_plt DESCRIPTION.
#' @param .dat DESCRIPTION.
#' @param .id DESCRIPTION.
#' @param .title DESCRIPTION.
#' @param .extent_cruise DESCRIPTION.
#' @param .inset description
#'
#' @return RETURN_DESCRIPTION
#' @examples
#' # ADD_EXAMPLES_HERE
map_plot <- function(
    .base_plt,
    .dat, 
    .id,
    .title, 
    .extent_cruise,
    .inset = NULL
    ) {
  
  set.seed(123)
  
  plt <- 
    .base_plt +
  geom_point(
    data = .dat, 
    aes(x = lon, 
        y = lat, 
        color = labels)
  ) +
  ggrepel::geom_text_repel(
    data  = .dat,
    aes(x = lon, 
        y = lat, 
        label = station),
    size  = 2.5,
    hjust = 0,
  ) +
  labs(title = .title) +
  scale_color_manual(
    name   = "Sample Type:",
    values = c("#1E90FF", "#EE3B3B", "#00FF00", "purple")
  ) +
  theme(
    legend.position = "bottom",
    legend.text     = element_text(size = 10),
    legend.title    = element_text(size = 15),
    title           = element_text(size = 15)
  )
  
  if (!is.null(.inset)) {
   plt <- 
     ggdraw() +
     draw_plot(plt) +
     draw_plot(.inset, 
               x = 0.85, 
               y = 0.755, 
               width  = 0.1, 
               height = 0.4)
  }
 
  return(plt) 
  # ---- End of Function ----
}
