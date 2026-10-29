# User tests - uses your own workspace and users dynamically

USERS_COLS_CONCISE <- c("user_id", "user_name", "status")
USERS_COLS <- c(
  "user_id", "email", "user_name", "memberships", "active_workspace",
  "default_workspace", "status"
)

test_that("get active users", {
  skip_on_cran()
  skip_if(NO_API_KEY_IN_ENVIRONMENT)

  users <- users(active = TRUE)
  expect_identical(names(users), USERS_COLS_CONCISE)
  expect_gte(nrow(users), 1, info = "You need at least one active user")
})

test_that("get all users", {
  skip_on_cran()
  skip_if(NO_API_KEY_IN_ENVIRONMENT)

  users <- users(active = FALSE)
  expect_identical(names(users), USERS_COLS_CONCISE)
  expect_gte(nrow(users), 1, info = "You need at least one user (active or inactive)")
})

test_that("ID for authenticated user", {
  skip_on_cran()
  skip_if(NO_API_KEY_IN_ENVIRONMENT)

  user <- user()
  expect_scalar(user$user_id)
})

test_that("extended output", {
  skip_on_cran()
  skip_if(NO_API_KEY_IN_ENVIRONMENT)

  users <- users(concise = FALSE)
  expect_identical(names(users), USERS_COLS)
})

test_that("NA for missing username (optional)", {
  skip_on_cran()
  skip_if(NO_API_KEY_IN_ENVIRONMENT)
  skip_if(is.null(USER_ID_MISSING_NAME), message = "User missing name not found, skipping test")

  result <- users() %>%
    filter(user_id == USER_ID_MISSING_NAME) %>%
    summarise(has_na = any(is.na(user_name)))
  
  expect_true(result$has_na)
})

test_that("update hourly rate", {
  skip_on_cran()
  skip_if(NO_API_KEY_IN_ENVIRONMENT)

  rate <- runif(1, 0, 1000) %>% ceiling()
  users <- users(active = TRUE)
  
  # Use first available user ID for testing, or the authenticated user
  test_user_id <- USER_ID_ALICE %||% user_get_id()
  
  workspace <- user_update_hourly_rate(test_user_id, rate, NULL)

  expect_equal(
    workspace %>%
      unnest(memberships) %>%
      filter(user_id == test_user_id) %>%
      pull(rate_amount),
    rate
  )
})

test_that("update user status", {
  skip_on_cran()
  skip_if(NO_API_KEY_IN_ENVIRONMENT)
  skip_if(is.null(USER_ID_CAROL), message = "Test user CAROL not found, skipping status test")

  workspace <- user_update_status(USER_ID_CAROL, FALSE)
  
  users <- workspace %>%
    unnest(cols = c(memberships)) %>%
    filter(user_id == USER_ID_CAROL)
  
  expect_in(users$membership_status, c("INACTIVE", "PENDING_EMAIL_VERIFICATION"))

  workspace <- user_update_status(USER_ID_CAROL, "ACTIVE")
  users <- workspace %>%
    unnest(cols = c(memberships)) %>%
    filter(user_id == USER_ID_CAROL)
  
  expect_equal(users$membership_status, "ACTIVE")
})

test_that("update user role", {
  skip_on_cran()
  skip_if(NO_API_KEY_IN_ENVIRONMENT)
  skip_if(is.null(USER_ID_BOB), message = "Test user BOB not found, skipping role test")
  
  workspace <- user_update_role(USER_ID_BOB, "WORKSPACE_ADMIN", clockify::workspace())

  expect_equal(
    workspace %>% unnest(cols = c(role)) %>% pull(role_name),
    "WORKSPACE_ADMIN"
  )
})

test_that("delete user role", {
  skip_on_cran()
  skip_if(NO_API_KEY_IN_ENVIRONMENT)
  skip_if(is.null(USER_ID_BOB), message = "Test user BOB not found, skipping role delete test")

  workspace <- user_update_role(USER_ID_BOB, "WORKSPACE_ADMIN", clockify::workspace())
  
  expect_true(user_delete_role(USER_ID_BOB, "WORKSPACE_ADMIN", clockify::workspace()))
})
