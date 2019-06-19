#' Spatial data map of NUTS level 1 units of Poland 2016
#'
#' A `sf` map from `shp` with NUTS level 1 units of Poland 2010-2018 with its
#' name and NUTS id's.
#'
#' @format A `sf` map with 7 rows and 3 variables:
#' \describe{
#'   \item{name}{name of unit}
#'   \item{id}{unit's NUTS id code}
#'   \item{geometry}{Shp coordinates}
#' }
"level1_map"

#' Spatial data map of NUTS level 2 units of Poland
#'
#' A `sf` map from `shp`` with NUTS level 2 units of Poland 2010-2018 with its
#' name and NUTS id's.
#'
#' @format A `sf` map with 16 rows and 3 variables:
#' \describe{
#'   \item{name}{name of unit}
#'   \item{id}{unit's NUTS id code}
#'   \item{geometry}{Shp coordinates}
#' }
"level2_map"

#' Spatial data map of NUTS level 3 units of Poland
#'
#' A `sf` map from `shp` with NUTS level 3 units of Poland 2010-2018 with its
#' name and NUTS id's.
#'
#' @format A `sf` map with 17 rows and 3 variables:
#' \describe{
#'   \item{name}{name of unit}
#'   \item{id}{unit's NUTS id code}
#'   \item{geometry}{Shp coordinates}
#' }
"level3_map"

#' Spatial data map of NUTS level 4 units of Poland
#'
#' A `sf` map from `shp` with NUTS level 4 units of Poland 2010-2018 with its
#' name and NUTS id's.
#'
#' @format A `sf` map with 73 rows and 3 variables:
#' \describe{
#'   \item{name}{name of unit}
#'   \item{id}{unit's NUTS id code}
#'   \item{geometry}{Shp coordinates}
#' }
"level4_map"

#' Spatial data map of unique NUTS level 5 units of Poland
#'
#' A `sf` map from `shp` with all unique NUTS level 5 units of Poland 
#' from 2010-2018 with itsname and NUTS id's. They should bee filtered
#' by inner join with corresponding NUTS ids from given year.
#'
#' @format A `sf` map with 380 rows and 3 variables:
#' \describe{
#'   \item{name}{name of unit}
#'   \item{id}{unit's NUTS id code}
#'   \item{geometry}{Shp coordinates}
#' }
"level5_map_units"

#' Spatial data frame with level 5 units that area has been changed
#'
#' A `sf` data frame with spatial data for units that area has been changed
#' without changing its NUTS id. You should replace corresponding rows in 
#' `level5_map_units` with updated geometry from this data frame if 
#' data's year is higher or eagual to given in `year` column.
#'
#' @format A `sf` map with 3 rows and 3 variables:
#' \describe{
#'   \item{name}{name of unit}
#'   \item{id}{unit's NUTS id code}
#'   \item{year}{year when change applies}
#'   \item{geometry}{Shp coordinates}
#' }
"level5_map_changes"

#' Spatial data map of NUTS level 6 units of Poland
#'
#' A `sf` map from `shp` with all unique NUTS level 6 units of Poland 
#' from 2010-2018 with itsname and NUTS id's. They should bee filtered
#' by inner join with corresponding NUTS ids from given year.
#'
#' @format A `sf` map with 2511 rows and 3 variables:
#' \describe{
#'   \item{name}{name of unit}
#'   \item{id}{unit's NUTS id code}
#'   \item{geometry}{Shp coordinates}
#' }
"level6_map_units"

#' Spatial data frame with level 6 units that area has been changed
#'
#' A `sf` data frame with spatial data for units that area has been changed
#' without changing its NUTS id. You should replace corresponding rows in 
#' `level6_map_units` with updated geometry from this data frame if 
#' data's year is higher or eagual to given in `year` column.
#'
#' @format A `sf` map with 1 rows and 3 variables:
#' \describe{
#'   \item{name}{name of unit}
#'   \item{id}{unit's NUTS id code}
#'   \item{year}{year when change applies}
#'   \item{geometry}{Shp coordinates}
#' }
"level6_map_changes"
