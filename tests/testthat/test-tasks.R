# Task tests - uses your own projects dynamically

TASKS_COLS <- c("task_id", "name", "project_id", "status", "billable", "assignees")

test_that("no tasks (or some tasks exist)", {
  skip_on_cran()
  skip_if(NO_API_KEY_IN_ENVIRONMENT)

  get_projects <- function() {
    projects <- projects()
    if (nrow(projects) == 0) {
      stop("No projects found")
    }
    return(project_id = projects$project_id[1])
  }
  
  project_id <- get_projects()$project_id
  
  tasks <- tasks(project_id)
  expect_gte(nrow(tasks), 0, info = "Projects may have zero or more tasks")
  expect_identical(names(tasks), TASKS_COLS)
}),

test_that("create task", {
  skip_on_cran()
  skip_if(NO_API_KEY_IN_ENVIRONMENT)
  skip_if(is.na(project_id), info = "Project not found, skipping")

  TASK_NAME <<- sprintf("Test Task - %s", random_string())
  
  task <- task_create(project_id, TASK_NAME)

  expect_equal(TASK_NAME, task$name)
  TASK_ID <<- task$task_id
}),

test_that("get tasks", {
  skip_on_cran()
  skip_if(NO_API_KEY_IN_ENVIRONMENT)
  skip_if(is.na(project_id), info = "Project not found, skipping")

  tasks <- tasks(project_id)
  expect_identical(names(tasks), TASKS_COLS)
}),

test_that("get task from ID", {
  skip_on_cran()
  skip_if(NO_API_KEY_IN_ENVIRONMENT)
  skip_if(is.na(TASK_ID), info = "Task not created, skipping")

  task <- task(project_id, TASK_ID)

  expect_equal(task %>% pull(name), TASK_NAME)
}),

test_that("update task", {
  skip_on_cran()
  skip_if(NO_API_KEY_IN_ENVIRONMENT)
  skip_if(is.na(TASK_ID), info = "Task not found, skipping")

  TASK_NAME_UPDATED <<- random_string()
  
  task <- task_update(
    project_id,
    TASK_ID,
    name = TASK_NAME_UPDATED,
    billable = FALSE,
    assignee_id = USER_ID_AUTHENTICATED
  )

  expect_equal(task %>% pull(name), TASK_NAME_UPDATED)
}),

test_that("delete task", {
  skip_on_cran()
  skip_if(NO_API_KEY_IN_ENVIRONMENT)
  skip_if(is.na(TASK_ID), info = "Task not found, skipping")

  # Status needs to be changed to "DONE" first or use delete directly if supported
  if (!is.null(task_update) && "status" %in% names(formals(task_update))) {
    task <- task_update(project_id, TASK_ID, status = "DONE")
    task_delete(project_id, TASK_ID)
  } else {
    task_delete(project_id, TASK_ID)
  }

  tasks <- tasks(project_id)
  expect_false(TASK_ID %in% tasks$task_id)
})
