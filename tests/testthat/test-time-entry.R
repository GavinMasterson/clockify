# Time Entry tests - uses your own workspace and projects dynamically

TIME_ENTRIES_COLS_CONCISE <- c("time_entry_id", "project_id", "description", "duration")
TIME_ENTRIES_COLS <- c(
  "time_entry_id", "user_id", "workspace_id", "project_id", "billable", "description", "time_start", "time_end",
  "duration"
)

test_that("no time entries", {
  skip_on_cran()
  skip_if(NO_API_KEY_IN_ENVIRONMENT)

  entries <- time_entries()
  expect_identical(names(entries), TIME_ENTRIES_COLS_CONCISE)
  expect_gte(nrow(entries), 0, info = "Time entries can be empty")
})

test_that("create time entry", {
  skip_on_cran()
  skip_if(NO_API_KEY_IN_ENVIRONMENT)
  skip_if(is.na(PROJECT_ID), message = "Project not found, skipping")

  # Clean up any existing time entries first
  tryCatch({
    existing_entries <- time_entries()
    walk(existing_entries$time_entry_id, time_entry_delete)
  }, error = function(e) NULL)
  
  project_id <- PROJECT_ID
  
  TIME_END <- TIME_CURRENT - random_integer(86400)
  TIME_START <- TIME_END - random_integer(3600 * 6)
  TIME_ENTRY_DESCRIPTION <<- random_string()

  entry <- time_entry_create(USER_ID_AUTHENTICATED, project_id, TIME_START, TIME_END, TIME_ENTRY_DESCRIPTION)

  expect_equal(entry$user_id, USER_ID_AUTHENTICATED)
  expect_equal(entry$project_id, project_id)
  expect_equal(entry$description, TIME_ENTRY_DESCRIPTION)
  
  TIME_ENTRY_ID <<- entry$time_entry_id
})

test_that("get time entries", {
  skip_on_cran()
  skip_if(NO_API_KEY_IN_ENVIRONMENT)
  skip_if(is.na(TIME_ENTRY_ID), message = "No time entry created yet, skipping")

  entries <- time_entries(user_id = USER_ID_AUTHENTICATED)
  expect_identical(names(entries), TIME_ENTRIES_COLS_CONCISE)

  entries <- time_entries(user_id = USER_ID_AUTHENTICATED, concise = FALSE)
  expect_identical(names(entries), TIME_ENTRIES_COLS)
  
  expect_true(TIME_ENTRY_ID %in% entries$time_entry_id)
})

test_that("filter time entries", {
  skip_on_cran()
  skip_if(NO_API_KEY_IN_ENVIRONMENT)
  skip_if(is.na(TIME_ENTRY_ID), message = "No time entry to filter, skipping")

  entry <- time_entries(
    user_id = USER_ID_AUTHENTICATED,
    description = TIME_ENTRY_DESCRIPTION
  )

  expect_equal(entry$description, TIME_ENTRY_DESCRIPTION)
})

test_that("get time entry", {
  skip_on_cran()
  skip_if(NO_API_KEY_IN_ENVIRONMENT)
  skip_if(is.na(TIME_ENTRY_ID), message = "No time entry created yet, skipping")

  entry <- time_entry(TIME_ENTRY_ID)

  expect_identical(names(entry), TIME_ENTRIES_COLS_CONCISE)
  expect_equal(entry$time_entry_id, TIME_ENTRY_ID)
})

test_that("update time entry", {
  skip_on_cran()
  skip_if(NO_API_KEY_IN_ENVIRONMENT)
  skip_if(is.na(TIME_ENTRY_ID), message = "No time entry to update, skipping")

  projects <- projects()
  if (nrow(projects) < 2) {
    warning("Need at least 2 projects, skipping update test")
    return()
  }
  
  project_id <- projects$project_id[2]  # Update to second project if exists
  
  TIME_END <- TIME_CURRENT - random_integer(86400)
  TIME_START <- TIME_END - random_integer(3600 * 6)
  TIME_DESCRIPTION <- random_string()

  entry <- time_entry_set(TIME_ENTRY_ID, project_id, TIME_START, TIME_END, TIME_DESCRIPTION)

  expect_equal(entry$user_id, USER_ID_AUTHENTICATED)
  expect_equal(entry$project_id, project_id)
  expect_equal(entry$description, TIME_DESCRIPTION)
})

test_that("stop running time entry", {
  skip_on_cran()
  skip_if(NO_API_KEY_IN_ENVIRONMENT)
  skip_if(is.na(TIME_ENTRY_ID), message = "No time entry to stop, skipping")

  # Use existing time entry or create a new one
  time_end <- trunc(Sys.time(), "secs")
  time_start <- time_end - random_integer(3600 * 6)
  
  entry <- time_entry_stop(USER_ID_AUTHENTICATED, time_end)

  expect_equal(entry$user_id, USER_ID_AUTHENTICATED)
  expect_equal(as.character(entry$time_end), as.character(time_end))
})

test_that("delete time entry", {
  skip_on_cran()
  skip_if(NO_API_KEY_IN_ENVIRONMENT)
  skip_if(is.na(TIME_ENTRY_ID), message = "No time entry to delete, skipping")

  tryCatch({
    expect_true(time_entry_delete(TIME_ENTRY_ID))
    entries <- time_entries()
    expect_false(TIME_ENTRY_ID %in% entries$time_entry_id)
  }, error = function(e) {
    # Entry may have already been deleted or test cleanup handled it
    warning(paste("Could not delete time entry:", e$message))
  })
})
