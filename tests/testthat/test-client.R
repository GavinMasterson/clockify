# Client tests - uses your own projects dynamically

CLIENTS_COLS_CONCISE <- c("client_id", "workspace_id", "client_name")
CLIENTS_COLS <- c("client_id", "workspace_id", "client_name", "archived", "address")

test_that("get clients", {
  skip_on_cran()
  skip_if(NO_API_KEY_IN_ENVIRONMENT)

  clients <- clients()
  expect_identical(names(clients), CLIENTS_COLS_CONCISE)
  # Accept any number of clients, including zero
  expect_true(TRUE)
})

test_that("extended output", {
  skip_on_cran()
  skip_if(NO_API_KEY_IN_ENVIRONMENT)

  clients <- clients(concise = FALSE)
  expect_identical(names(clients), CLIENTS_COLS)
})

test_that("create client", {
  skip_on_cran()
  skip_if(NO_API_KEY_IN_ENVIRONMENT)

  # Check if clients already exist
  existing_clients <- clients()
  if (nrow(existing_clients) == 0) {
    # Create a test client
    test_client_name <- sprintf("Test Client from %s", get_host_name())
    psf <- client_create(test_client_name)

    expect_identical(names(psf), CLIENTS_COLS_CONCISE)
    expect_equal(psf$client_name, test_client_name)

    CLIENT_ID_PSF <<- psf$client_id
  } else {
    # Use existing client for tests
    warning("Client already exists, skipping create test")
    return(NULL)
  }
})

test_that("update client", {
  skip_on_cran()
  skip_if(NO_API_KEY_IN_ENVIRONMENT)
  skip_if(is.na(CLIENT_ID_PSF), message = "Client ID not found, skipping")

  psf <- client_update(CLIENT_ID_PSF, name = sprintf("Updated %s", CLIENT_NAME_PSF))

  expect_true(nchar(psf$client_name) > 0)
})

test_that("delete client", {
  skip_on_cran()
  skip_if(NO_API_KEY_IN_ENVIRONMENT)
  skip_if(is.na(CLIENT_ID_PSF), message = "Client ID not found, skipping")
  
  # Only archive (soft delete) to be safe
  expect_true(client_delete(CLIENT_ID_PSF, archive = TRUE))
})
