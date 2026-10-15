library(stringi)

CLOCKIFY_API_KEY <- Sys.getenv("CLOCKIFY_API_KEY")

# Dynamic workspace lookup - use first workspace that has the "clockify" project
# or fall back to your default workspace.
get_test_workspace <- function() {
  wks <- workspaces()
  if (nrow(wks) == 0) {
    stop("No workspaces found. Please create a workspace in your Clockify account.")
  }
  # Prefer workspace with a "clockify" project, or use first workspace
  if (length(grep("clockify", toupper(wks$name), ignore.case = TRUE)) > 0) {
    return(wks$name[wks$name %in% grep("clockify", toupper(wks$name), ignore.case = TRUE)[1]])
  }
  warning("No 'clockify' workspace found, using first workspace.")
  return(wks$name[1])
}
CLOCKIFY_WORKSPACE <- get_test_workspace()

# Get your user ID from API response
get_authenticated_user_id <- function() {
  user_data <- user()
  if (nrow(user_data) == 0) {
    stop("Cannot find authenticated user. Please check your clockify API key.")
  }
  return(user_data$user_id)
}
USER_ID_AUTHENTICATED <- get_authenticated_user_id()

# Get other test users (optional - use your own collaborators)
# If no other users exist, we'll create test users or use dummy IDs for testing
USER_ID_MISSING_NAME <- NULL  # Will be resolved if you need it
USER_ID_BOB <- NULL
USER_ID_ALICE <- NULL
USER_ID_CAROL <- NULL

# Get projects dynamically
get_projects <- function(project_name_pattern = NULL) {
  projects <- projects()
  if (nrow(projects) == 0) {
    stop("No projects found. Please create a project in your Clockify workspace.")
  }
  if (!is.null(project_name_pattern)) {
    # Filter projects by name pattern if provided
    matching <- projects$name[toupper(projects$name) %in% toupper(project_name_pattern)]
    if (length(matching) > 0) {
      return(projects$name[projects$name %in% matching][1])
    }
  }
  # Return first project if no pattern or no match
  return(projects$name[1])
}

# Get/create a project for testing
get_test_project <- function() {
  # Check if test project exists
  existing_projects <- projects()
  
  # If we have projects, use first one
  if (nrow(existing_projects) > 0 && nchar(existing_projects$name[1]) > 0) {
    msg <- sprintf("Using project: %s", existing_projects$name[1])
    warning(msg)
    return(list(
      project_id = existing_projects$project_id[1],
      project_name = existing_projects$name[1]
    ))
  }
  
  stop("No projects found in workspace. Please create a project or check your workspace.")
}

PROJECT <- get_test_project()
PROJECT_ID <- PROJECT$project_id
PROJECT_NAME <- PROJECT$project_name

# Client info - use your own or create one
CLIENT_ID_RSTUDIO <- character(0)  # Will be NULL if no clients
CLIENT_NAME_PSF <- "Python Software Foundation"  # Example client name

NO_API_KEY_IN_ENVIRONMENT <- CLOCKIFY_API_KEY == ""

random_string <- function(length = 24) {
  stri_rand_strings(1, length)
}

random_integer <- function(min = 0, max = 100) {
  round(runif(1) * (max - min) + min)
}

TAG_NAME <- random_string()
TAG_ID <- NULL
TAG_NAME_UPDATED <- random_string()

TASK_NAME <- random_string()
TASK_ID <- NULL
TASK_NAME_UPDATED <- random_string()

TIME_CURRENT <- Sys.time()
# Truncate time to seconds (Clockify doesn't have sub-second precision).
TIME_CURRENT <- trunc(TIME_CURRENT, units = "secs")

options(duration.units = "secs")
# Don't truncate columns in tibbles (for debugging tests).
options(pillar.width = Inf)
