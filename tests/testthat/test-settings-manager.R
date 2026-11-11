library(testthat)

test_that("save_settings and load_settings serialize and restore properly", {
  tmp_json <- tempfile(fileext = ".json")
  tmp_rds <- tempfile(fileext = ".rds")

  fake_settings <- list(
    gain = 2.5,
    sample = "EXP_TEST",
    enabled = TRUE,
    hardware = list(gain = 2.5)
  )

  # Test JSON save
  save_settings(fake_settings, file_path = tmp_json, format = "json")
  expect_true(file.exists(tmp_json))

  loaded_json <- jsonlite::read_json(tmp_json, simplifyVector = FALSE)
  expect_equal(loaded_json$gain, 2.5)
  expect_equal(loaded_json$sample, "EXP_TEST")
  expect_equal(loaded_json$enabled, TRUE)
  expect_equal(loaded_json$metadata$package, "BindableWidgets")

  # Test RDS save
  save_settings(fake_settings, file_path = tmp_rds, format = "rds")
  expect_true(file.exists(tmp_rds))

  loaded_rds <- readRDS(tmp_rds)
  expect_equal(loaded_rds$gain, 2.5)
  expect_equal(loaded_rds$sample, "EXP_TEST")
  expect_equal(loaded_rds$metadata$package, "BindableWidgets")

  unlink(c(tmp_json, tmp_rds))
})

test_that("registry registers inputs and maintains subgroups", {
  clear_registry()
  register_input("temp_1", "numeric", c("thermal", "hardware"))
  register_input("notes", "text", "experiment")

  types <- get_registered_types()
  subgroups <- get_registered_subgroups()

  expect_equal(types[["temp_1"]], "numeric")
  expect_equal(types[["notes"]], "text")
  expect_equal(subgroups[["temp_1"]], c("thermal", "hardware"))
  expect_equal(subgroups[["notes"]], "experiment")

  clear_registry()
  expect_equal(length(get_registered_types()), 0)
})

