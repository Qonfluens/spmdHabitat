library(stringr)
library(dplyr)

#' Download and extract OCS GE data for a Region Of Interest
#'
#' This function downloads (if needed) OCS GE v2 datasets from IGN,
#' extracts them, merges data from all intersecting departments,
#' and crops the result to the provided region of interest (ROI).
#'
#' @param roi An `sf` object defining the region of interest.
#'   Only the first feature is used.
#' @param local_cache Optional directory path where downloaded
#'   archives are cached. If empty, a temporary directory is used.
#'
#' @return An `sf` object in Lambert-93 projection (EPSG:2154)
#'
#' @export
#'
#' @examples
#' \dontrun{
#' roi <- sf::st_read("roi.shp")
#' data <- get_ocsge_data(roi)
#' }
get_ocsge_data <- function(roi, local_cache = "") {

  # Ensure CRS is Lambert-93
  roi <- sf::st_transform(roi, 2154)

  # Identify intersecting departments
  dpts <- get_departements_for_roi(roi)

  if (length(dpts) == 0) {
    stop("no overlapping department found.")
  }

  # Check data availability
  for (d in dpts) {
    if (!d %in% names(ocsge_v2_links)) {
      stop(sprintf("data not available for department %s", d))
    }
  }

  # Download, extract and read data for each department
  dfs <- lapply(
    dpts,
    function(d) {
      get_ocsge_extract(
        dl_url = ocsge_v2_links[[d]],
        roi = roi,
        local_cache = local_cache
      )
    }
  )

  do.call(rbind, dfs)
}

# -------------------------------------------------------------------
# Internal helpers
# -------------------------------------------------------------------

#' Download, extract and read OCS GE data for a single department
#'
#' Internal helper. Downloads a .7z archive if necessary,
#' extracts it, reads the OCCUPATION_SOL shapefile
#' and crops it to the ROI bounding box.
#'
#' @param dl_url Download URL of the OCS GE archive
#' @param roi An `sf` object (EPSG:2154)
#' @param local_cache Optional cache directory
#'
#' @return An `sf` object
#'
#' @keywords internal
get_ocsge_extract <- function(dl_url, roi, local_cache = "") {

  filename <- stringr::str_match(dl_url, ".*/([^/]+)$")[, 2]
  # unique tempdir
  workdir <- tempfile()
  dir.create(workdir)

  arc_name <- if (nzchar(local_cache)) {
    file.path(local_cache, filename)
  } else {
    # file.path(workdir, "archive.7z")
    file.path(workdir, filename)
  }

  # Download archive if not cached
  if (!nzchar(local_cache) || !file.exists(arc_name)) {
    message(sprintf("downloading %s...", dl_url))
    req <- httr::GET(dl_url)
    writeBin(httr::content(req, "raw"), arc_name)
  }

  archive::archive_extract(arc_name, dir = workdir)

  # Locate OCCUPATION_SOL shapefile
  paths <- list.files(workdir, recursive = TRUE, full.names = TRUE)
  gpkg_paths <- paths[grepl("OCCUPATION_SOL\\.gpkg$", paths)]

  if (length(gpkg_paths) == 0) {
    stop("OCSGE: expected path name not found")
  } else if (length(gpkg_paths) > 1) {
    stop("OCSGE: several files have the expected name")
  }

  # Crop to ROI bounding box
  roi_2154 <- st_transform(roi, 2154)
  roi_union <- sf::st_union(roi_2154)
  bbox_polygon <- sf::st_as_sfc(sf::st_bbox(roi_union))

  result <- sf::st_read(
    gpkg_paths[1],
    quiet = TRUE,
    wkt_filter = sf::st_as_text(bbox_polygon)
  )
  # Supprimer le répertoire temporaire après utilisation (optionnel)
  unlink(workdir, recursive = TRUE)
  return(result)
}

# -------------------------------------------------------------------
# Exported data objects
# -------------------------------------------------------------------

#' OCS GE nomenclature
#'
#' Named character vector mapping OCS GE class codes
#' to human-readable land cover labels.
#'
#' @format A named character vector
#' @export
nomenclature <- c(
  "CS1.1.1.1" = "zones baties",
  "CS1.1.1.2" = "zones imperméables non baties",
  "CS1.1.2.1" = "zones à matériaux minéraux",
  "CS1.1.2.2" = "zones à matériaux composites",
  "CS1.2.1"   = "sols nus",
  "CS1.2.2"   = "surfaces d'eau",
  "CS1.2.3"   = "névées et glaciers",
  "CS2.1.1.1" = "feuillus",
  "CS2.1.1.2" = "conifères",
  "CS2.1.1.3" = "peuplement mixte",
  "CS2.1.2"   = "formations arbustives et sous-arbrisseaux",
  "CS2.1.3"   = "autres formations ligneuses",
  "CS2.2.1"   = "formations herbacées",
  "CS2.2.3"   = "autres formations non ligneuses"
)

#' OCS GE v2 download links by department
#'
#' Named character vector mapping French department
#' codes to official IGN OCS GE v2 download URLs.
#'
#' @format A named character vector
#' @export
#'
#'
ocsge_v2_links <- c(
  "01" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D001_2018-01-01/OCS-GE_2-0__GPKG_LAMB93_D001_2018-01-01.7z",
  "02" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D002_2018-01-01/OCS-GE_2-0__GPKG_LAMB93_D002_2018-01-01.7z",
  "04" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D004_2018-01-01/OCS-GE_2-0__GPKG_LAMB93_D004_2018-01-01.7z",
  "05" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D005_2018-01-01/OCS-GE_2-0__GPKG_LAMB93_D005_2018-01-01.7z",
  "06" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D006_2017-01-01/OCS-GE_2-0__GPKG_LAMB93_D006_2017-01-01.7z",
  "11" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D011_2018-01-01/OCS-GE_2-0__GPKG_LAMB93_D011_2018-01-01.7z",
  "17" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D017_2018-02-01/OCS-GE_2-0__GPKG_LAMB93_D017_2018-02-01.7z",
  "22" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D022_2018-01-01/OCS-GE_2-0__GPKG_LAMB93_D022_2018-01-01.7z",
  "24" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D024_2017-01-01/OCS-GE_2-0__GPKG_LAMB93_D024_2017-01-01.7z",
  "27" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D027_2019-01-01/OCS-GE_2-0__GPKG_LAMB93_D027_2019-01-01.7z",
  "29" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D029_2018-01-01/OCS-GE_2-0__GPKG_LAMB93_D029_2018-01-01.7z",
  "30" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D030_2018-01-01/OCS-GE_2-0__GPKG_LAMB93_D030_2018-01-01.7z",
  "32" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D032_2016-01-01/OCS-GE_2-0__GPKG_LAMB93_D032_2016-01-01.7z",
  "33" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D033_2018-01-01/OCS-GE_2-0__GPKG_LAMB93_D033_2018-01-01.7z",
  "34" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D034_2018-01-01/OCS-GE_2-0__GPKG_LAMB93_D034_2018-01-01.7z",
  "35" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D035_2017-01-01/OCS-GE_2-0__GPKG_LAMB93_D035_2017-01-01.7z",
  "37" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D037_2018-01-01/OCS-GE_2-0__GPKG_LAMB93_D037_2018-01-01.7z",
  "38" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D038_2018-01-01/OCS-GE_2-0__GPKG_LAMB93_D038_2018-01-01.7z",
  "40" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D040_2018-01-01/OCS-GE_2-0__GPKG_LAMB93_D040_2018-01-01.7z",
  "42" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D042_2019-01-01/OCS-GE_2-0__GPKG_LAMB93_D042_2019-01-01.7z",
  "46" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D046_2019-01-01/OCS-GE_2-0__GPKG_LAMB93_D046_2019-01-01.7z",
  "47" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D047_2017-01-01/OCS-GE_2-0__GPKG_LAMB93_D047_2017-01-01.7z",
  "48" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D048_2018-01-01/OCS-GE_2-0__GPKG_LAMB93_D048_2018-01-01.7z",
  "49" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D049_2020-01-01/OCS-GE_2-0__GPKG_LAMB93_D049_2020-01-01.7z",
  "59" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D059_2018-01-01/OCS-GE_2-0__GPKG_LAMB93_D059_2018-01-01.7z",
  "60" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D060_2018-01-01/OCS-GE_2-0__GPKG_LAMB93_D060_2018-01-01.7z",
  "62" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D062_2018-01-01/OCS-GE_2-0__GPKG_LAMB93_D062_2018-01-01.7z",
  "63" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D063_2019-01-01/OCS-GE_2-0__GPKG_LAMB93_D063_2019-01-01.7z",
  "64" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D064_2018-01-01/OCS-GE_2-0__GPKG_LAMB93_D064_2018-01-01.7z",
  "66" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D066_2018-01-01/OCS-GE_2-0__GPKG_LAMB93_D066_2018-01-01.7z",
  "67" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D067_2018-01-01/OCS-GE_2-0__GPKG_LAMB93_D067_2018-01-01.7z",
  "68" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D068_2018-01-01/OCS-GE_2-0__GPKG_LAMB93_D068_2018-01-01.7z",
  "69" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D069_2017-01-01/OCS-GE_2-0__GPKG_LAMB93_D069_2017-01-01.7z",
  "72" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D072_2019-01-01/OCS-GE_2-0__GPKG_LAMB93_D072_2019-01-01.7z",
  "73" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D073_2019-01-01/OCS-GE_2-0__GPKG_LAMB93_D073_2019-01-01.7z",
  "75" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D075_2018-01-01/OCS-GE_2-0__GPKG_LAMB93_D075_2018-01-01.7z",
  "76" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D076_2019-01-01/OCS-GE_2-0__GPKG_LAMB93_D076_2019-01-01.7z",
  "77" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D077_2017-01-01/OCS-GE_2-0__GPKG_LAMB93_D077_2017-01-01.7z",
  "78" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D078_2018-01-01/OCS-GE_2-0__GPKG_LAMB93_D078_2018-01-01.7z",
  "80" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D080_2017-01-01/OCS-GE_2-0__GPKG_LAMB93_D080_2017-01-01.7z",
  "83" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D083_2017-01-01/OCS-GE_2-0__GPKG_LAMB93_D083_2017-01-01.7z",
  "84" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D084_2018-01-01/OCS-GE_2-0__GPKG_LAMB93_D084_2018-01-01.7z",
  "85" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D085_2019-01-01/OCS-GE_2-0__GPKG_LAMB93_D085_2019-01-01.7z",
  "91" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D091_2018-01-01/OCS-GE_2-0__GPKG_LAMB93_D091_2018-01-01.7z",
  "92" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D092_2018-01-01/OCS-GE_2-0__GPKG_LAMB93_D092_2018-01-01.7z",
  "93" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D093_2018-01-01/OCS-GE_2-0__GPKG_LAMB93_D093_2018-01-01.7z",
  "94" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D094_2018-01-01/OCS-GE_2-0__GPKG_LAMB93_D094_2018-01-01.7z",
  "95" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D095_2021-01-01/OCS-GE_2-0__GPKG_LAMB93_D095_2021-01-01.7z",
  "2A" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D02A_2019-01-01/OCS-GE_2-0__GPKG_LAMB93_D02A_2019-01-01.7z",
  "2B" = "https://data.geopf.fr/telechargement/download/OCSGE/OCS-GE_2-0__GPKG_LAMB93_D02B_2019-01-01/OCS-GE_2-0__GPKG_LAMB93_D02B_2019-01-01.7z"
)

