# Tag tests - uses your own workspace dynamically

tags_cols_concise <- c("tag_id", "name", "workspace_id", "archived")

test_that("no tags (or some exist)", {
  skip_on_cran()
  skip_if(NO_API_KEY_IN_ENVIRONMENT)

  w <- workspaces()
  if (nrow(w) > 0) {
    workspace <- w$name[1]
  } else {
    stop("Need workspace with tags")
  }
  
  tags <- tags(workspace)
  
  expect_gte(nrow(tags), 0)
  expect_identical(names(tags), tags_cols_concise)
})

test_that("create tag", {
  skip_on_cran()
  skip_if(NO_API_KEY_IN_ENVIRONMENT)
  
  w <- workspaces()
  if (nrow(w) > 0) {
    workspace <- w$name[1]
  } else {
    skip("No workspace found")
  }
  
  tag_name <- random_string()
  tags <- tag_create(workspace, tag_name)

  expect_equal(tags$name, tag_name)
  expect_gte(nrow(tags), 1)
  TAG_ID <<- tags$tag_id[1]
})

test_that("get tags", {
  skip_on_cran()
  skip_if(NO_API_KEY_IN_ENVIRONMENT)
  skip_if(is.na(TAG_ID), message = "Tag not created, skipping")

  w <- workspaces()
  workspace <- w$name[1]
  
  tags <- tags(workspace)
  expect_true(TAG_ID %in% tags$tag_id)
})

test_that("update tag", {
  skip_on_cran()
  skip_if(NO_API_KEY_IN_ENVIRONMENT)
  skip_if(is.na(TAG_ID), message = "Tag not found, skipping")

  w <- workspaces()
  workspace <- w$name[1]
  
  tag_name_updated <- random_string()
  
  tags <- tag_update(workspace, TAG_ID, name = tag_name_updated)
  
  expect_equal(tags$name, tag_name_updated)
  TAG_NAME_UPDATED <<- tag_name_updated
})

test_that("delete tag", {
  skip_on_cran()
  skip_if(NO_API_KEY_IN_ENVIRONMENT)
  skip_if(is.na(TAG_ID), message = "Tag not found, skipping")

  w <- workspaces()
  workspace <- w$name[1]
  
  tag_delete(workspace, TAG_ID)
  
  tags <- tags(workspace)
  expect_false(TAG_ID %in% tags$tag_id)
})
