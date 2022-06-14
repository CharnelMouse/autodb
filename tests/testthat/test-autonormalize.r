describe("auto_entityset", {
  it("runs DFD and normalises the given data.frame", {
    df <- data.frame(
      Title = rep(
        c(
          "Beginning MySQL Database Design and Optimization",
          "The Relational Model for Database Management: Version 2"
        ),
        each = 2
      ),
      Format = c("Hardcover", "E-book", "E-book", "Paperback"),
      Author = rep(c("Chad Russell", "E.F. Codd"), each = 2),
      Author_Nationality = rep(c("American", "British"), each = 2),
      Price = c(4999L, 2234L, 1388L, 3999L),
      Thickness = "Thick",
      Genre_ID = rep(1:2, each = 2),
      Genre_Name = rep(c("Tutorial", "Popular science"), each = 2),
      Publisher_ID = rep(1:2, each = 2)
    )
    es <- auto_entityset(df, 1)
    skip("non-deterministic, fix next")
    expect_setequal(
      names(es$dataframes),
      c("Publisher_ID", "Price", "Format")
    )
    expect_identical(
      es$relationships,
      list(
        c("Price", "Publisher_ID", "Publisher_ID", "Publisher_ID"),
        c("Price", "Format", "Format", "Format")
      )
    )
  })
  it("doesn't choose keys with incorrect types as the index", {
    df <- data.frame(
      Title = rep(
        c(
          "Beginning MySQL Database Design and Optimization",
          "The Relational Model for Database Management: Version 2"
        ),
        each = 2
      ),
      Format = c("Hardcover", "E-book", "E-book", "Paperback"),
      Author = rep(c("Chad Russell", "E.F. Codd"), each = 2),
      Author_Nationality = rep(c("American", "British"), each = 2),
      Price = c(4999, 2234, 1388, 3999),
      Thickness = "Thick",
      Genre_ID = rep(1:2, each = 2),
      Genre_Name = rep(c("Tutorial", "Popular science"), each = 2),
      Publisher_ID = rep(1:2, each = 2)
    )
    es <- auto_entityset(df, 1)
    skip("wait on key filtering")
  })
})

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
  skip("too long, skip for now")
  df <- mock_customer(
    n_customers = 80,
    n_products = 50,
    n_sessions = 200,
    n_transactions = 10000,
    return_single_table = TRUE
  )

  exact_entityset <- auto_entityset(
    df,
    accuracy = 1,
    name = "Customer Transactions",
    time_index = 'transaction_time'
  )

  expect_true(setequal(
    colnames(exact_entityset$dataframes[['transaction_id']]$df),
    c(
      'transaction_id',
      'session_id',
      'transaction_time',
      'product_id',
      'amount'
    )
  ))

  expect_identical(
    colnames(exact_entityset$dataframes[['product_id']]$df),
    c('product_id', 'brand')
  )

  expect_identical(
    colnames(exact_entityset$dataframes[['session_id']]$df),
    c('session_id', 'customer_id', 'device', 'session_start')
  )

  expect_identical(
    colnames(exact_entityset$dataframes[['customer_id']]$df),
    c('customer_id', 'zip_code', 'join_date', 'birthday')
  )

  expect_identical(
    vapply(
      exact_entityset$relationships,
      \(r) paste0(r[1], ".", r[2], " -> ", r[3], ".", r[4]),
      character(1)
    ),
    c(
      'transaction_id.session_id -> session_id.session_id',
      'transaction_id.product_id -> product_id.product_id',
      'session_id.customer_id -> customer_id.customer_id'
    )
  )

  approx_entityset <- auto_entityset(
    df,
    accuracy = 0.98,
    name = "Customer Transactions",
    time_index = 'transaction_time'
  )

  expect_identical(approx_entityset, exact_entityset)
})
