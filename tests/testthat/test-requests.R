context("Url builder")
library(bdl)

test_that("Proper url build", {
  expect_equal(build_url("xxx", "yyy",list(aaa = "bbb",ccc = "ddd")),
    "https://bdl.stat.gov.pl/api/v1/xxx/yyy?aaa=bbb&ccc=ddd&page-Size=100&format=json")
  expect_equal(build_url("xxx", "yyy",NULL),
    "https://bdl.stat.gov.pl/api/v1/xxx/yyy?page-Size=100&format=json")
  expect_equal(build_url("xxx", "yyy",list(year = c("2010", "2011"))),
    "https://bdl.stat.gov.pl/api/v1/xxx/yyy?year=2010&year=2011&page-Size=100&format=json")
  expect_equal(build_url("xxx", "yyy",list(year = c("2010", "2011"), "var-Id" = c("2137","148190"))),
    "https://bdl.stat.gov.pl/api/v1/xxx/yyy?year=2010&year=2011&var-Id=2137&var-Id=148190&page-Size=100&format=json")
  expect_equal(build_url("Subjects", "", NULL),
    "https://bdl.stat.gov.pl/api/v1/Subjects?page-Size=100&format=json")
})

context("Get request")
dir <- "data/By-Variable"
vid <- "3643"

filters <- list(year = c("2000", "2010"))
# request_by_variable <- get_request(dir = dir, id = id, filters = filters, key = key)
request_by_variable <- get_request(dir = dir, id = vid, filters = filters)

test_that("Code errors", {
  expect_error(get_request("foo","bar"),
    "Failure to get data. Probably invalid directory. Status code: 404.")
  expect_error(get_request("data/By-Unit","foo"),
    "Failure to get data. Probably invalid id. Status code: 400.")
})

test_that("Proper data", {
  expect_equal(request_by_variable$variableId, as.integer(vid))
  expect_equal(tibble::is_tibble(tibble::as_tibble(request_by_variable$results)), TRUE)
})

context("Get data by unit")
path <- "https://bdl.stat.gov.pl/api/v1/Data/By-Unit/023200000000?var-Id=3643&var-Id=2137&var-Id=148190"
json <- jsonlite::fromJSON(path)
df <- tibble::as_tibble(json$results)
df$values <- lapply(df$values,tibble::as_tibble)
df <- df %>%
  tidyr::unnest(values)
df$id <- as.character(df$id)

test_df <- get_data_by_unit(unitId = "000000000000", varId =  "2137", type = "label")
test_df <- test_df %>%
  head(1)
test_df$id <- as.character(test_df$id)

df <- add_attribute_labels(df)

test_that("Error codes", {
  expect_error(get_data_by_unit(unitId = "12345678901234", varId = "420"),
                                "Unit id should be length of 12.")
  expect_error(get_data_by_unit(unitId = c("123456789012","12345"), varId = "420"),
               "Unit id should be length of 12.")
  expect_error(get_data_by_unit(unitId = c("123456789012", NA), varId = "420"),
               "Unit id should be length of 12.")
  expect_error(get_data_by_unit(unitId = c("123456789012"), varId = c("", "112")),
               "Variable id should be non-zero length string.")
  expect_error(get_data_by_unit(unitId = c("123456789012"), varId = c("422", NA)),
               "Variable id should be non-zero length string.")
})

test_that("Proper labels", {
  expect_match(toString(test_df$variableName), "Gospodarka wodna - Gospodarka wodna:uzdatnianie wody - wydajność na dobę")
  expect_match(toString(test_df$measureName), "m3")
})

test_that("Proper data", {
  expect_equal(get_data_by_unit(unitId = "023200000000", varId =  c("3643", "2137", "148190")), df)
})


context("Get data by variable")
path <- "https://bdl.stat.gov.pl/api/v1/data/By-Variable/60559?year=2006&unit-Parent-Id=010000000000&page-Size=100
&format=json"
dir <- "data/By-Variable"

filters <- list(year = "2006", "unit-Parent-Id" = "010000000000", "unit-Level" = NULL, "aggregate-Id" = NULL)

df <- tibble::tibble()
repeat{
  json <- get_request(dir, 60559, filters)

  if (is.list(json$results) && length(json$results) == 0) {
    stop("Filters returned empty set.")
  }
  df_t <- tibble::as_tibble(json$results)
  df_t <- df_t %>%
    tidyr::unnest(values)
  df <- rbind(df,df_t)
  if (is.null(filters$page)) {
    filters$page <- 1
  } else {
    filters$page <- filters$page + 1
  }
  json
  if(is.null(json$links) || json$links$self == json$links$last){
    break
  }
}
class(df) <- c("bdl", class(df))
df <- add_attribute_labels(df)

test_df <- get_data_by_variable("420", year = "2000", unitLevel = "0")
test_df <- test_df %>%
  head(1)

test_that("Error codes", {
  expect_error(get_data_by_variable(varId = "420", unitParentId = "12345678901234"), "Unit id should be 12 characters NUTS id code.")
  expect_error(get_data_by_variable(varId = "", unitParentId = "123456789012"), "Variable id should be non-zero length string.")
  expect_error(get_data_by_variable(varId = c("120", ""), unitParentId = "123456789012"), "Variable id should be non-zero length string.")
  expect_error(get_data_by_variable(varId = c(NA, "120"), unitParentId = "123456789012"), "Variable id should be non-zero length string.")
})

test_that("Proper labels", {
  expect_match(test_df$name, "POLSKA")
})


test_that("Proper data", {
  expect_equal(get_data_by_variable("60559", unitParentId = "010000000000", year = "2006"),df)
})


context("Get data by variable and locality")

dir <- "data/Localities/By-Variable"

filters <- list(year = NULL, "unit-Parent-Id" = "070000000000")

df <- tibble::tibble()
repeat{
  json <- get_request(dir, 420, filters)

  if (is.list(json$results) && length(json$results) == 0) {
    stop("Filters returned empty set.")
  }
  df_t <- tibble::as_tibble(json$results)
  df_t <- df_t %>%
    tidyr::unnest(values)
  df <- rbind(df,df_t)
  if (is.null(filters$page)) {
    filters$page <- 1

  } else {
    filters$page <- filters$page + 1
  }
  if(is.null(json$links) || json$links$self == json$links$last){
    break
  }
}
class(df) <- c("bdl", class(df))
df <- add_attribute_labels(df)

test_df <- get_data_by_variable_locality("270672","030200000000")
test_df <- test_df %>%
  head(1)

test_that("Error codes", {
  expect_error(get_data_by_variable_locality(varId = "420", unitParentId = "12345678901234"), "Unit id should be 12 characters NUTS id code.")
  expect_error(get_data_by_variable_locality(varId = "", unitParentId = "030200000000"), "Variable id should be non-zero length string.")
  expect_error(get_data_by_variable_locality(varId = c("120", ""), unitParentId = "030200000000"), "Variable id should be non-zero length string.")
  expect_error(get_data_by_variable_locality(varId = c(NA, "120"), unitParentId = "030200000000"), "Variable id should be non-zero length string.")
})

test_that("Proper labels", {
  expect_match(test_df$name, "Wrocław")
})

test_that("Proper data", {
  expect_equal(get_data_by_variable_locality("420", unitParentId = "070000000000"),df)
})


context("Get data by unit and locality")
path <- "https://bdl.stat.gov.pl/api/v1/Data/Localities/By-Unit/011212001011-0981682?var-Id=148190"
dir <- "Data/Localities/By-Unit"

filters <- list(year = NULL, "var-Id"="148190")

df <- tibble::tibble()
repeat{
  json <- get_request(dir, "011212001011-0981682", filters)

  if (is.list(json$results) && length(json$results) == 0) {
    stop("Filters returned empty set.")
  }
  df_t <- tibble::as_tibble(json$results)
  df_t <- df_t %>%
    tidyr::unnest(values)
  df <- rbind(df,df_t)
  if (is.null(filters$page)) {
    filters$page <- 1

  } else {
    filters$page <- filters$page + 1
  }
  if(is.null(json$links) || json$links$self == json$links$last){
    break
  }
}
class(df) <- c("bdl", class(df))
df <- add_attribute_labels(df)

test_df <- get_data_by_unit_locality(unitId = "030210564011-0986283", varId =  "270672", type = "label")
test_df <- test_df %>%
  head(1)
test_df$id <- as.character(test_df$id)

test_that("Error codes", {
  expect_error(get_data_by_unit_locality(unitId = "12345678901234", varId = "420"),
    "Unit id should be 12 characters NUTS id code with 7 characters locality individual id, separated by dash.")
  
  expect_error(get_data_by_unit_locality(unitId = c("030210564011-0986283","12345"), varId = "420"),
               "Unit id should be 12 characters NUTS id code with 7 characters locality individual id, separated by dash.")
  
  expect_error(get_data_by_unit_locality(unitId = c("030210564011-0986283", NA), varId = "420"),
               "Unit id should be 12 characters NUTS id code with 7 characters locality individual id, separated by dash.")
  
  expect_error(get_data_by_unit_locality(unitId = "030210564011-0986283", varId = c("", "112")),
               "Variable id should be non-zero length string.")
  
  expect_error(get_data_by_unit_locality(unitId = "030210564011-0986283", varId = c("422", NA)),
               "Variable id should be non-zero length string.")
})

test_that("Proper labels", {
  expect_match(toString(test_df$variableName), "Licea ogólnokształcące specjalne dla młodzieży - Typy szkół:licea ogólnokształcące specjalne dla młodzieży, Wykaz:uczniowie - mężczyźni, Gestor:prowadzone przez samorząd powiatowy - miasta na prawach powiatu")
  expect_match(toString(test_df$measureName), "osoba")
})

test_that("Proper data", {
  expect_equal(get_data_by_unit_locality(unitId = "011212001011-0981682", varId =  "148190"), df)
  # expect_equal(get_data_by_unit_locality(unitId = "011212001011-0981682",
  #   varId =  "148190",
  #   key = key), df)
  expect_equal(get_data_by_unit_locality(unitId = "011212001011-0981682",
    varId =  "148190"), df)
})



context("Summary for bdl data frames")
non_bdl_data_frame <- tibble::tibble()

test_that("Error codes", {
  expect_error(summary.bdl(non_bdl_data_frame), "Wrong data frame column names.")
})

detach("package:bdl", unload=TRUE)
