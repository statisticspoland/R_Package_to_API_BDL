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


context("Get data by unit")

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


context("Get data by variable")

test_that("Error codes", {
  expect_error(get_data_by_variable(varId = "420", unitParentId = "12345678901234"), "Unit id should be 12 characters NUTS id code.")
  expect_error(get_data_by_variable(varId = "", unitParentId = "123456789012"), "Variable id should be non-zero length string.")
  expect_error(get_data_by_variable(varId = c("120", ""), unitParentId = "123456789012"), "Variable id should be non-zero length string.")
  expect_error(get_data_by_variable(varId = c(NA, "120"), unitParentId = "123456789012"), "Variable id should be non-zero length string.")
})




context("Get data by variable and locality")



test_that("Error codes", {
  expect_error(get_data_by_variable_locality(varId = "420", unitParentId = "12345678901234"), "Unit id should be 12 characters NUTS id code.")
  expect_error(get_data_by_variable_locality(varId = "", unitParentId = "030200000000"), "Variable id should be non-zero length string.")
  expect_error(get_data_by_variable_locality(varId = c("120", ""), unitParentId = "030200000000"), "Variable id should be non-zero length string.")
  expect_error(get_data_by_variable_locality(varId = c(NA, "120"), unitParentId = "030200000000"), "Variable id should be non-zero length string.")
})



context("Get data by unit and locality")

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



context("Summary for bdl data frames")
non_bdl_data_frame <- tibble::tibble()

test_that("Error codes", {
  expect_error(summary.bdl(non_bdl_data_frame), "Wrong data frame column names.")
})

detach("package:bdl", unload=TRUE)
