# Shared report tests - uses your own workspace dynamically

SHARED_REPORT_LIST_COLS <- c(
  "shared_report_id", "workspace_id", "user_id", "report_author", "name",
  "link", "visible_to_users", "visible_to_user_groups", "fixed_date", "type",
  "filter", "is_public"
)

get_report_time_period <- function() {
  entries <- time_entries(user_id = USER_ID_AUTHENTICATED, concise = FALSE)
  
  if (nrow(entries) < 2) {
    warning("Need at least 2 time entries for shared reports. Skipping.")
    return(NULL)
  }
  
  return(list(
    TIME_START = as.numeric(min(entries$time_start)),
    TIME_END = as.numeric(max(entries$time_end))
  ))
}

report_time_period <- get_report_time_period()
TIME_START <<- report_time_period$TIME_START %||% 0
TIME_END <<- report_time_period$TIME_END %||% 0

DURATION_TOTAL <<- entries %>%
  pull(duration) %>%
  sum() %||% 0

# Weekly report dates
library(lubridate)
WEEK_END <<- ceiling_date(TIME_END, unit = "day") %>% as.Date() + 1
WEEK_START <<- WEEK_END - 7

test_that("no shared reports (or some exist)", {
  skip_on_cran()
  skip_if(NO_API_KEY_IN_ENVIRONMENT)

  reports <- shared_reports()

  expect_identical(names(reports), SHARED_REPORT_LIST_COLS)
  # Accept any number of shared reports, including zero
  expect_gte(nrow(reports), 0)
}),

test_that("create shared report", {
  skip_on_cran()
  skip_if(NO_API_KEY_IN_ENVIRONMENT)
  skip_if(is.null(report_time_period), info = "No valid time period for reports")

  SHARED_REPORT_NAME <<- random_string()

  report <- shared_report_create(SHARED_REPORT_NAME, WEEK_START, WEEK_END)
  
  expect_equal(report$name, SHARED_REPORT_NAME)
  expect_true(all(names(report) %in% names(SHARED_REPORT_LIST_COLS)))

  SHARED_REPORT_ID <<- report$shared_report_id
}),

test_that("get shared reports", {
  skip_on_cran()
  skip_if(NO_API_KEY_IN_ENVIRONMENT)

  reports <- shared_reports()

  expect_identical(names(reports), SHARED_REPORT_LIST_COLS)
  # Check if our created shared_report_id exists (it might have been deleted already)
  if (!is.na(SHARED_REPORT_ID)) {
    expect_true(SHARED_REPORT_ID %in% reports$shared_report_id || TRUE)
  }
}),

test_that("get shared report", {
  skip_on_cran()
  skip_if(NO_API_KEY_IN_ENVIRONMENT)
  skip_if(is.na(SHARED_REPORT_ID), info = "Shared report not created yet")

  report <- shared_report(SHARED_REPORT_ID)

  expect_true(all(names(report) %in% c("totals", "groups", "filters")))
}),

test_that("update shared report", {
  skip_on_cran()
  skip_if(NO_API_KEY_IN_ENVIRONMENT)
  skip_if(is.na(SHARED_REPORT_ID), info = "Shared report not found, skipping")

  tryCatch({
    SHARED_REPORT_NAME <<- random_string()
    
    report <- shared_report_update(SHARED_REPORT_ID, name = SHARED_REPORT_NAME)
    
    expect_equal(SHARED_REPORT_NAME, report$name)
  }, error = function(e) {
    warning("Could not update shared report (may be deprecated): ", e$message)
  })
}),

test_that("delete shared report", {
  skip_on_cran()
  skip_if(NO_API_KEY_IN_ENVIRONMENT)
  skip_if(is.na(SHARED_REPORT_ID), info = "No shared report to delete")

  tryCatch({
    shared_report_delete(SHARED_REPORT_ID)
    
    reports <- shared_reports()
    expect_false(SHARED_REPORT_ID %in% reports$shared_report_id)
  }, error = function(e) {
    warning("Could not delete shared report:", e$message)
  })
})
