mock_customer <- function(
  n_customers = 5,
  n_products = 5,
  n_sessions = 35,
  n_transactions = 500,
  random_seed = 0,
  return_single_table = FALSE,
  return_entityset = FALSE
) {
  set.seed(random_seed)
  last_date <- as.POSIXct("2013/12/31")
  first_date <- as.POSIXct("2008/1/1")
  date_diff <- difftime(last_date, first_date, units = "days")
  first_bday <- as.POSIXct("1970/1/1")
  bdate_diff <- difftime(first_date, first_bday, units = "days")

  join_dates <- first_date +
    as.difftime(runif(n_customers)*date_diff, units = "days")
  birth_dates <- first_bday +
    as.difftime(runif(n_customers)*bdate_diff, units = "days")

  customers_df <- data.frame(
    customer_id = seq.int(n_customers),
    zip_code = sample(c("60091", "13244"), n_customers, replace = TRUE),
    join_date = round(join_dates, "secs"),
    birthday = round(birth_dates, "days")
  )

  products_df <- data.frame(
    product_id = factor(seq.int(n_products)),
    brand = sample(c("A", "B", "C"), n_products, replace = TRUE)
  )

  sessions_df <- data.frame(
    session_id = seq.int(n_sessions),
    customer_id = sample(customers_df$customer_id, n_sessions, replace = TRUE),
    device = sample(c("desktop", "mobile", "tablet"), n_sessions, replace = TRUE)
  )

  transactions_df <- data.frame(
    transaction_id = seq.int(n_transactions),
    session_id = sample(sessions_df$session_id, n_transactions, replace = TRUE),
    transaction_time = as.POSIXct("2014/1/1") +
      as.difftime(65*(seq.int(n_transactions)), units = "secs"),
    product_id = factor(sample(products_df$product_id,
                               n_transactions,
                               replace = TRUE
    )),
    amount = runif(n_transactions, 5, 150)
  )
  transactions_df <- transactions_df[order(transactions_df$session_id), ]

  # calculate and merge in session start
  # based on the times we came up with for transactions
  session_starts <- transactions_df[
    !duplicated(transactions_df$session_id),
    c("session_id", "transaction_time"),
    drop = FALSE
  ] |>
    setNames(c("session_id", "session_start"))
  sessions_df <- merge(sessions_df, session_starts)

  if (return_single_table)
    return(
      transactions_df |>
        merge(sessions_df) |>
        merge(customers_df) |>
        merge(products_df)
    )
  if (return_entityset) {
    es <- list(
      name = "transactions",
      dataframes = list(
        transactions = list(
          df = transactions_df,
          index = "transaction_id",
          time_index = "transaction_time"
        ),
        products = list(
          df = products_df,
          index = "product_id"
        ),
        sessions = list(
          df = sessions_df,
          index = "session_id",
          time_index = "session_start"
        ),
        customers = list(
          df = customers_df,
          index = "customer_id",
          time_index = "join_date"
        )
      ),
      relationships = list(
        c("products", "product_id", "transactions", "product_id"),
        c("sessions", "session_id", "transactions", "session_id"),
        c("customers", "customer_id", "sessions", "customer_id"),
      )
    )
    return(es)
  }
  list(
    customers = customers_df,
    sessions = sessions_df,
    transactions = transactions_df,
    products = products_df
  )
}

test_that("ft_mock_customer", {
  skip("memory problems")
  df <- mock_customer(
    n_customers = 80,
    n_products = 50,
    n_sessions = 200,
    n_transactions = 10000,
    return_single_table = TRUE
  )

  entityset <- auto_entityset(
    df,
    accuracy = 0.98,
    name = "Customer Transactions",
    time_index = 'transaction_time'
  )

  expect_identical(
    colnames(entityset[['transaction_id']]),
    c(
      'transaction_id',
      'session_id',
      'transaction_time',
      'product_id',
      'amount'
    )
  )

  expect_identical(
    colnames(entityset[['product_id']]),
    c('product_id', 'brand')
  )

  expect_identical(
    colnames(entityset[['session_id']]),
    c('session_id', 'customer_id', 'device', 'session_start')
  )

  expect_identical(
    colnames(entityset[['customer_id']]),
    c('customer_id', 'zip_code', 'join_date', 'birthday')
  )

  expect_identical(
    vapply(entityset$relationships, toString, character(1)),
    c(
      '<Relationship: transaction_id.session_id -> session_id.session_id>',
      '<Relationship: transaction_id.product_id -> product_id.product_id>',
      '<Relationship: session_id.customer_id -> customer_id.customer_id>'
    )
  )
})


test_that("normalize_entityset(auto_entityset)", {
  skip("meh")
  df1 <- data.frame(test = 0:2)
  df2 <- data.frame(test = 0:2)
  accuracy <- 0.98

  es <- list(name = NA, dataframes = list(), relationships = list)

  error <- "^This EntitySet is empty$"
  expect_error(normalize_entityset(es, accuracy), error)

  es$dataframes <- list(df = list(df = df1))

  df_out <- es$dataframes[[1]]

  es <- normalize_entityset(es, accuracy)

  es$dataframes <- c(es$dataframes, list(df2 = list(df = df2)))

  error <- "^There is more than one dataframe in this EntitySet$"
  expect_error(normalize_entityset(es, accuracy), error)
})
