testthat::context("OCS GE data access")

library(sf)
library(testthat)

# -------------------------------------------------------------------
# Helpers
# -------------------------------------------------------------------

skip_if_no_internet <- function() {
  if (!curl::has_internet()) {
    skip("No internet connection available")
  }
}

# -------------------------------------------------------------------
# Fixtures
# -------------------------------------------------------------------

# Petite ROI simple : carré metaleurop
roi_metaleurop <- sf::st_read("data/metaleurop_roi.geojson")
# -------------------------------------------------------------------
# Tests get_departements_for_roi()
# -------------------------------------------------------------------

test_that("get_departements_for_roi returns department codes", {

  dpts <- get_departements_for_roi(roi_metaleurop)

  expect_type(dpts, "character")
  expect_true(length(dpts) >= 1)
  expect_true(any(dpts %in% c("59", "62")))
})

# -------------------------------------------------------------------
# Tests .get_ocsge_extract()
# -------------------------------------------------------------------

test_that(".get_ocsge_extract downloads and reads OCSGE data", {

  skip_if_no_internet()

  dpt <- "59"
  url <- ocsge_v2_links[[dpt]]

  sf_data <- spmdHabitat:::get_ocsge_extract(
    dl_url = url,
    roi = roi_metaleurop,
    local_cache = ""
  )

  ## Visual check
  # library(ggplot2)
  # ggplot() + geom_sf(data=sf_data, aes(fill=code_cs))

  expect_s3_class(sf_data, "sf")
  expect_true(nrow(sf_data) > 0)
  expect_equal(st_crs(sf_data)$epsg, 2154)
})

# -------------------------------------------------------------------
# Tests get_ocsge_data()
# -------------------------------------------------------------------

test_that("get_ocsge_data merges multiple departments", {

  skip_if_no_internet()
  roi=roi_metaleurop
  sf_data <- get_ocsge_data(roi_metaleurop)

  ## Visual check
  # library(ggplot2)
  # ggplot() + geom_sf(data=sf_data, aes(fill=code_cs))

  expect_s3_class(sf_data, "sf")
  expect_true(nrow(sf_data) > 0)
  expect_equal(st_crs(sf_data)$epsg, 2154)
})

