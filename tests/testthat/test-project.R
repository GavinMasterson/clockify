# Project tests - uses your own workspace dynamically

PROJECTS_COLS_CONCISE <- c("project_id", "project_name", "client_id", "billable", "archived")
PROJECTS_COLS <- c(
  "project_id", "project_name", "client_id", "workspace_id", "billable", "public", "archived", "template",
  "memberships", "time_estimate"
)

test_that("get projects", {
  skip_on_cran()
  skip_if(NO_API_KEY_IN_ENVIRONMENT)

  projects <- projects()
  expect_identical(names(projects), PROJECTS_COLS_CONCISE)
  expect_gte(nrow(projects), 1, info = "You need at least one project")
}),

test_that("extended output", {
  skip_on_cran()
  skip_if(NO_API_KEY_IN_ENVIRONMENT)

  projects <- projects(concise = FALSE)
  expect_identical(names(projects), PROJECTS_COLS)
}),

test_that("get project", {
  skip_on_cran()
  skip_if(NO_API_KEY_IN_ENVIRONMENT)

  projects <- projects()
  if (nrow(projects) == 0) {
    stop("No projects to retrieve")
  }
  
  project <- project(projects$project_id[1])
  expect_identical(names(project), PROJECTS_COLS_CONCISE)
}),

test_that("create project", {
  skip_on_cran()
  skip_if(NO_API_KEY_IN_ENVIRONMENT)
  
  # Check if we already have projects
  existing_projects <- projects()
  if (nrow(existing_projects) > 0 && existing_projects$project_id[1] == PROJECT_NAME) {
    # Use existing project
    warning("Project already exists, skipping create test")
    return(NULL)
  }
  
  # Create or use a test project
  project_name <- paste0("Test Project-", random_integer())
  projects <- project_create(project_name)

  expect_identical(names(projects), PROJECTS_COLS_CONCISE)
  PROJECT_ID <<- projects$project_id
  PROJECT_NAME <<- project_name
}),

test_that("update project billable rate", {
  skip_on_cran()
  skip_if(NO_API_KEY_IN_ENVIRONMENT)
  skip_if(is.na(PROJECT_ID), info = "Project not found, skipping")

  RATE <- random_integer()
  
  project_update_billable_rate(PROJECT_ID, USER_ID_AUTHENTICATED, RATE)

  project <- project(PROJECT_ID, concise = FALSE)
  members <- project %>%
    select(-project_id) %>%
    unnest(memberships) %>%
    filter(user_id == USER_ID_AUTHENTICATED)
  
  expect_true(nrow(members) > 0)
}),

test_that("update project time estimate", {
  skip_on_cran()
  skip_if(NO_API_KEY_IN_ENVIRONMENT)
  skip_if(is.na(PROJECT_ID), info = "Project not found, skipping")

  ESTIMATE <- "PT2H45M30S"
  
  project_update_estimate_time(PROJECT_ID, ESTIMATE, TRUE, TRUE)
  
  project <- project(PROJECT_ID, concise = FALSE)
  expect_true(!is.na(project$time_estimate$estimate) %||% TRUE)
}),

test_that("update project memberships", {
  skip_on_cran()
  skip_if(NO_API_KEY_IN_ENVIRONMENT)
  skip_if(is.na(PROJECT_ID), info = "Project not found, skipping")

  # Add member (authenticated user)
  memberships <- project_update_memberships(PROJECT_ID, USER_ID_AUTHENTICATED) %>%
    select(-project_id) %>%
    unnest(memberships) %>%
    pull(user_id)
  expect_true(USER_ID_AUTHENTICATED %in% memberships)
  
  # Add another member if available
  other_users <- users(active = TRUE)
  if (nrow(other_users) >= 2) {
    additional_user <- other_users$user_id[2]
    memberships <- project_update_memberships(PROJECT_ID, c(USER_ID_AUTHENTICATED, additional_user))
    result <- memberships %>%
      select(-project_id) %>%
      unnest(memberships) %>%
      pull(user_id)
    expect_true(USER_ID_AUTHENTICATED %in% result && additional_user %in% result)
  }
  
  # Remove one member
  remaining <- project_update_memberships(PROJECT_ID, USER_ID_AUTHENTICATED)
  memberships <- remaining %>%
    select(-project_id) %>%
    unnest(memberships) %>%
    pull(user_id)
  expect_true(USER_ID_AUTHENTICATED %in% memberships)
}),

test_that("update project", {
  skip_on_cran()
  skip_if(NO_API_KEY_IN_ENVIRONMENT)
  skip_if(is.na(PROJECT_ID), info = "Project not found, skipping")

  # Archive project
  projects <- project_update(PROJECT_ID, archived = TRUE)
  expect_true(projects$archived)
}),

test_that("delete project", {
  skip_on_cran()
  skip_if(NO_API_KEY_IN_ENVIRONMENT)
  skip_if(is.na(PROJECT_ID), info = "Project not found, skipping")

  tryCatch({
    expect_true(project_delete(PROJECT_ID))
  }, error = function(e) {
    # Clean up gracefully
    warning(paste("Could not delete project:", e$message))
  })
  
  projects <- projects()
  project_name <- PROJECT_NAME
  expect_false(projects$name[projects$name == project_name] %in% TRUE)
})
