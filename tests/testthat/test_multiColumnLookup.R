skip_on_appveyor()

test_that("multiColumnLookup() works", {
  
  (persons <- rbind(
    noFactorDataFrame(name = "Peter", city = "Berlin"),
    noFactorDataFrame(name = "Paul", city = "Paris"),
    noFactorDataFrame(name = "Mary", city = "Berlin"),
    noFactorDataFrame(name = "Paul", city = "Berlin"),
    noFactorDataFrame(name = "Peter", city = "Paris")
  ))

  # Who is cool, which city is cool and which combination is coolest?
  (is_cool <- kwb.utils::safeRowBindAll(list(
    noFactorDataFrame(name = "Paul", city = "Berlin", value = "astro"),
    noFactorDataFrame(city = "Berlin", value = "cool"),
    noFactorDataFrame(name = "Paul", value = "mega"),
    noFactorDataFrame(city = "Paris", value = "ca va")
  )))

  # Lookup the coolness based on name and city
  coolness <- multiColumnLookup(persons, is_cool, value = "value")
  
  expect_identical(coolness, c("cool", "mega", "cool", "astro", "ca va"))
})
