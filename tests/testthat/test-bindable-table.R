library(testthat)
library(data.table)

test_that("normalize_bindable_table_data handles empty and populated data correctly", {
  # Empty input
  empty_res <- normalize_bindable_table_data(NULL)
  expect_s3_class(empty_res, "data.table")
  expect_equal(nrow(empty_res), 0)
  expect_true(all(c("_step_id", "_group_id", "_repeats") %in% names(empty_res)))

  # Populated data missing internal tracking columns
  raw_df <- data.frame(
    use = c(TRUE, FALSE),
    name = c("Step 1", "Step 2"),
    position = c(10, 20)
  )
  res <- normalize_bindable_table_data(raw_df)
  expect_equal(res$`_step_id`, c(1L, 2L))
  expect_equal(res$`_group_id`, c(NA_integer_, NA_integer_))
  expect_equal(res$`_repeats`, c(1L, 1L))
})

test_that("coerce_bindable_table_payload processes list payloads from client", {
  client_list <- list(
    list(use = TRUE, name = "A", position = 0, `_step_id` = 1, `_group_id` = NULL, `_repeats` = 1),
    list(use = TRUE, name = "B", position = 90, `_step_id` = 2, `_group_id` = 1, `_repeats` = 5)
  )
  res <- coerce_bindable_table_payload(client_list)
  expect_s3_class(res, "data.table")
  expect_equal(nrow(res), 2)
  expect_equal(res$`_step_id`, c(1L, 2L))
  expect_equal(res$`_group_id`, c(NA_integer_, 1L))
  expect_equal(res$`_repeats`, c(1L, 5L))

  # Test enveloped payload with rows and _rev timestamp
  enveloped <- list(
    rows = client_list,
    `_rev` = 1731362700000
  )
  res_env <- coerce_bindable_table_payload(enveloped)
  expect_s3_class(res_env, "data.table")
  expect_equal(nrow(res_env), 2)
  expect_equal(res_env$`_step_id`, c(1L, 2L))
})

test_that("expand_protocol correctly unrolls contiguous loop groups and repeats", {
  protocol <- data.table(
    use = c(TRUE, TRUE, TRUE, TRUE),
    name = c("Home", "Raise", "Lower", "Hold"),
    position = c(0, 0, 90, 90),
    speed = c(100, 100, 100, 10),
    hold = c(1, 5, 5, 10),
    `_step_id` = 1:4,
    `_group_id` = c(NA_integer_, 1L, 1L, NA_integer_),
    `_repeats` = c(1L, 3L, 3L, 1L)
  )

  expanded <- expand_protocol(protocol, filter_active = TRUE)

  # Step 1: 1 time
  # Steps 2 & 3: 3 repeats = 6 steps
  # Step 4: 1 time
  # Total = 1 + 6 + 1 = 8 steps
  expect_equal(nrow(expanded), 8)
  expect_equal(expanded$execution_step, 1:8)
  expect_equal(expanded$name[1], "Home")
  expect_equal(expanded$name[2], "Raise")
  expect_equal(expanded$name[3], "Lower")
  expect_equal(expanded$name[4], "Raise")
  expect_equal(expanded$name[5], "Lower")
  expect_equal(expanded$name[6], "Raise")
  expect_equal(expanded$name[7], "Lower")
  expect_equal(expanded$name[8], "Hold")

  expect_equal(expanded$cycle_index[2:7], c(1L, 1L, 2L, 2L, 3L, 3L))
  expect_equal(expanded$total_cycles[2:7], rep(3L, 6))
})

test_that("bindableTableInput generates proper allow-grouping attributes", {
  tag_default <- bindableTableInput("tbl1")
  html_str_def <- as.character(tag_default)
  expect_true(grepl('data-allow-grouping="true"', html_str_def))

  tag_no_group <- bindableTableInput("tbl2", allow_grouping = FALSE)
  html_str_no_group <- as.character(tag_no_group)
  expect_true(grepl('data-allow-grouping="false"', html_str_no_group))
})

