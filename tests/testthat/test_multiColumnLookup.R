test_that("multiColumnLookup() works", {

  f <- multiColumnLookup
  
  (data <- rbind(
    noFactorDataFrame(name = "Peter", city = "Berlin"),
    noFactorDataFrame(name = "Paul", city = "Paris"),
    noFactorDataFrame(name = "Mary", city = "Berlin"),
    noFactorDataFrame(name = "Paul", city = "Berlin"),
    noFactorDataFrame(name = "Peter", city = "Paris")
  ))

  data2 <- rbind(
    data, 
    noFactorDataFrame(name = "Heidi", city = "Almhuette")
  )
  
  # Who is cool, which city is cool and which combination is coolest?
  (lookup <- kwb.utils::safeRowBindAll(list(
    noFactorDataFrame(name = "Paul", city = "Berlin", value = "astro"),
    noFactorDataFrame(city = "Berlin", value = "cool"),
    noFactorDataFrame(name = "Paul", value = "mega"),
    noFactorDataFrame(city = "Paris", value = "ca va")
  )))

  lookup2 <- renameColumns(lookup, list(value = "value2"))
  lookup3 <- lookup[, c(1L, 3L, 2L)]
  
  lookup4 <- rbind(
    lookup, 
    noFactorDataFrame(name = "", city = "", value = "default")
  )
  
  # Lookup the coolness based on name and city
  expected <- c("cool", "mega", "cool", "astro", "ca va")
  expect_identical(f(data, lookup), expected)
  expect_error(f(data, lookup, value = "value2"))
  expect_identical(f(data, lookup2), expected)
  expect_identical(f(data, lookup2, value = "value2"), expected)
  expect_error(f(data, lookup3))
  expect_identical(f(data, lookup3, value = "value"), expected)
  expect_warning(f(data2, lookup))
})
