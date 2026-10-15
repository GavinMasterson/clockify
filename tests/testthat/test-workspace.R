# Workspace tests - uses your own workspace dynamically

# Get active workspace from cache or API
get_active_workspace <- function() {
  # Try to get current workspace, falling back to getting workspaces
  if (!is.null(getOption("last_active_workspace"))) {
    return(getOption("last_active_workspace"))
  }
  
  wks <- workspaces()
  if (nrow(wks) == 0) {
    stop("No workspaces found")
  }
  
  return(wks$name[1])
}

get_active_workspace_id <- function() {
  wks <- workspaces()
  if (nrow(wks) == 0) {
    stop("No workspaces found")
  }
  
  return(wks$workspace_id[1])
}

WORKSPACE_COLS <- c("workspace_id", "name", "memberships", "subdomain")

test_that("get workspaces", {
  skip_on_cran()
  skip_if(NO_API_KEY_IN_ENVIRONMENT)

  workspaces <- workspaces()
  expect_identical(names(workspaces), WORKSPACE_COLS)
  expect_gte(nrow(workspaces), 1, info = "You need at least one workspace")
})

test_that("set default workspace for authenticated user", {
  skip_on_cran()
  skip_if(NO_API_KEY_IN_ENVIRONMENT)

  # Delete cached workspace (set when authenticated).
  rm("workspace_id", envir = clockify:::cache)

  workspace <- workspace()
  expect_false(workspace == "")
}),

test_that("choose workspace", {
  skip_on_cran()
  skip_if(NO_API_KEY_IN_ENVIRONMENT)

  current_ws <- get_active_workspace()
  
  # Test that we can set and get workspace
  set_active_workspace(current_ws)
  expect_identical(workspace(), current_ws)
}),

test_that("get cached workspace", {
  skip_on_cran()
  skip_if(NO_API_KEY_IN_ENVIRONMENT)

  current_ws <- get_active_workspace()
  set_active_workspace(current_ws)
  
  workspace <- workspace()
  expect_identical(workspace(), current_ws)
})

# test_that("get all users", {
#   skip_on_cran()
#   skip_if(NO_API_KEY_IN_ENVIRONMENT)
#
#   users <- users(active = FALSE)
#   # expect_identical(names(users), USERS_COLS_CONCISE)
# })
#
# test_that("extended output", {
#   skip_on_cran()
#   skip_if(NO_API_KEY_IN_ENVIRONMENT)
#
#   users <- users(concise = FALSE)
#   # expect_identical(names(users), USERS_COLS)
# })
