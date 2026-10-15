# Report tests - uses your own workspace and time entries dynamically

REPORT_SUMMARY_COLS <- c("user_name", "duration", "amount", "amounts", "projects")
REPORT_DETAILED_COLS <- c(
  "id",
  "description",
  "user_id",
  "start",
  "end",
  "duration",
  "billable",
  "project_id",
  "task_id",
  "tag_ids",
  "approval_request_id",
  "is_locked",
  "amount",
  "rate",
  "user_name",
  "user_email",
  "project_name",
  "client_name",
  "client_id"
)
REPORT_WEEKLY_COLS <- c("user_name", "duration", "amount", "projects")

get_report_time_period <- function() {
  w <- workspaces()
  if (nrow(w) == 0) {
    stop("No workspaces found")
  }
  workspace <- w$name[1]
  
  entries <- time_entries(user_id = USER_ID_AUTHENTICATED, concise = FALSE)
  if (nrow(entries) == 0) {
    warning("No time entries to build report. Skip report tests.")
    return(NULL)
  }
  
  return(list(
    workspace = workspace,
    TIME_START = as.numeric(min(entries$time_start)),
    TIME_END = as.numeric(max(entries$time_end)),
    DURATION_TOTAL = sum(entries$duration)
  ))
}

report_tests_setup <- get_report_time_period()
if (is.null(report_tests_setup)) {
  # Tests were skipped due to no valid entries
  return()
}

TIME_START <<- report_tests_setup$TIME_START
TIME_END <<- report_tests_setup$TIME_END
DURATION_TOTAL <<- report_tests_setup$DURATION_TOTAL

test_that("summary report", {
  skip_on_cran()
  skip_if(NO_API_KEY_IN_ENVIRONMENT)
  skip_if(is.null(report_tests_setup), info = "No time entries found for reports")

  report <- reports_summary(report_tests_setup$TIME_START, report_tests_setup$TIME_END)

  if (!is.null(report_tests_setup$DURATION_TOTAL)) {
    expect_equal(report$duration, report_tests_setup$DURATION_TOTAL)
  }
  expect_identical(names(report), REPORT_SUMMARY_COLS)
}),

test_that("detailed report", {
  skip_on_cran()
  skip_if(NO_API_KEY_IN_ENVIRONMENT)
  skip_if(is.null(report_tests_setup), info = "No time entries found for reports")

  report <- reports_detailed(report_tests_setup$TIME_START, report_tests_setup$TIME_END)

  if (!is.null(report_tests_setup$DURATION_TOTAL)) {
    total_duration <- report %>% pull(duration) %>% sum(na.rm = TRUE)
    expect_equal(total_duration, report_tests_setup$DURATION_TOTAL)
  }
  expect_true(all(sort(names(report)) %in% sort(REPORT_DETAILED_COLS)))
}),

test_that("weekly report", {
  skip_on_cran()
  skip_if(NO_API_KEY_IN_ENVIRONMENT)
  skip_if(is.null(report_tests_setup), info = "No time entries found for reports")
  
  # Calculate weekly dates
  library(lubridate)
  week_end <- ceiling_date(report_tests_setup$TIME_END, unit = "day") %>% as.Date() + 1
  week_start <- week_end - 7
  
  report <- reports_weekly(week_start, week_end)

  if (!is.null(report_tests_setup$DURATION_TOTAL)) {
    expect_equal(report$duration, report_tests_setup$DURATION_TOTAL)
  }
  expect_identical(names(report), REPORT_WEEKLY_COLS)
})
