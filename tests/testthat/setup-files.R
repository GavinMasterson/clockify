library(stringi)

CLOCKIFY_API_KEY <- Sys.getenv("CLOCKIFY_API_KEY")
NO_API_KEY_IN_ENVIRONMENT <- CLOCKIFY_API_KEY == ""

set_api_key(CLOCKIFY_API_KEY)

# Dynamic workspace lookup - use first workspace that has the "clockify" project
# or fall back to your default workspace.
get_test_workspace <- function() {
  wks <- workspaces()
  if (nrow(wks) == 0) {
    stop("No workspaces found. Please create a workspace in your Clockify account.")
  }
  if ("clockify" %in% wks$name) {
    return("clockify")
  } else {
    # Prefer workspace with a "clockify" project, or use first workspace
    warning("No 'clockify' workspace found, using first workspace.")
    return(wks$name[1])
  }
}

# Get your user ID from API response
get_authenticated_user_id <- function() {
  user_data <- user()
  if (nrow(user_data) == 0) {
    stop("Cannot find authenticated user. Please check your clockify API key.")
  }
  return(user_data$user_id)
}

# Get other test users (optional - use your own collaborators)
# If no other users exist, we'll create test users or use dummy IDs for testing
USER_ID_MISSING_NAME <- NULL  # Will be resolved if you need it
USER_ID_BOB <- NULL
USER_ID_ALICE <- NULL
USER_ID_CAROL <- NULL

# Get projects dynamically
get_test_project <- function(project_name_pattern = NULL) {
  existing_projects <- projects()
  if (nrow(existing_projects) == 0) {
    stop("No projects found. Please create a project in your Clockify workspace.")
  }
  if ("test" %in% existing_projects$project_name) {
    project <-
      existing_projects |>
      dplyr::filter(project_name == "test")
    return(list(
      project_id = project$project_id[1],
      project_name = project$project_name[1]
    ))
  } else {
    warning("No 'test' project found, using first project")
    # Return first project if no pattern or no match
    return(list(
      project_id = existing_projects$project_id[1],
      project_name = existing_projects$project_name[1]
    ))
  }
}

# IDs set by earlier tests (with <<-) and checked with skip_if(is.na(...)) in
# later ones. Start them as NA so that later tests skip (rather than error) if
# the test that sets them is skipped or fails.
PROJECT_ID <- NA
CLIENT_ID_PSF <- NA
TAG_ID <- NA
TASK_ID <- NA
TIME_ENTRY_ID <- NA
SHARED_REPORT_ID <- NA

# These lookups need the API, so only run them if there's a key. Without one the
# API tests are skipped but offline tests (like test-util.R) can still run.
if (!NO_API_KEY_IN_ENVIRONMENT) {
  CLOCKIFY_WORKSPACE <- get_test_workspace()
  USER_ID_AUTHENTICATED <- get_authenticated_user_id()

  PROJECT <- get_test_project()
  PROJECT_ID <- PROJECT$project_id
  PROJECT_NAME <- PROJECT$project_name
}

# Client info - use your own or create one
CLIENT_ID_RSTUDIO <- character(0)  # Will be NULL if no clients
CLIENT_NAME_PSF <- "Python Software Foundation"  # Example client name

random_string <- function(length = 24) {
  stri_rand_strings(1, length)
}

random_integer <- function(min = 0, max = 100) {
  round(runif(1) * (max - min) + min)
}

TAG_NAME <- random_string()
TAG_NAME_UPDATED <- random_string()

TASK_NAME <- random_string()
TASK_NAME_UPDATED <- random_string()

TIME_CURRENT <- Sys.time()
# Truncate time to seconds (Clockify doesn't have sub-second precision).
TIME_CURRENT <- trunc(TIME_CURRENT, units = "secs")

options(duration.units = "secs")
# Don't truncate columns in tibbles (for debugging tests).
options(pillar.width = Inf)
