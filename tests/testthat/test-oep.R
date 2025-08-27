test_that("isco08_to_oep works correctly with individual codes", {
  # Test individual ISCO08 codes
  expect_equal(isco08_to_oep("4110"), "41")
  expect_equal(isco08_to_oep("3413"), "43")
  expect_equal(isco08_to_oep("2330"), "67")
  expect_equal(isco08_to_oep("3111"), "54")
  expect_equal(isco08_to_oep("2161"), "70")
})

test_that("isco08_to_oep works correctly with vectorized input", {
  # Test the exact user example that was failing
  input_codes <- as.character(c(4110, 3413, 2330, 3111, 2161, 4322, 2269, 2166, 2341, 2221))
  expected_output <- c("41", "43", "67", "54", "70", "49", "52", "58", "53", "64")
  
  result <- isco08_to_oep(input_codes)
  
  expect_equal(result, expected_output)
  expect_false(any(is.na(result)), info = "No NAs should be present in results")
})

test_that("isco08_to_oep handles mixed digit codes correctly", {
  # Test codes that have different trailing zero patterns
  mixed_codes <- c("4110", "2330", "1000", "5000")
  result <- isco08_to_oep(mixed_codes)
  
  # All should return valid results, no NAs
  expect_false(any(is.na(result)))
  expect_length(result, 4)
  expect_type(result, "character")
})

test_that("isco08_to_oep handles edge cases", {
  # Test with NA values
  result_with_na <- isco08_to_oep(c("4110", NA, "3413"))
  expect_equal(result_with_na[1], "41")
  expect_true(is.na(result_with_na[2]))
  expect_equal(result_with_na[3], "43")
  
  # Test empty vector
  expect_equal(isco08_to_oep(character(0)), character(0))
  
  # Test all NA vector
  expect_true(all(is.na(isco08_to_oep(c(NA, NA)))))
})

test_that("isco88_to_oep works correctly with vectorized input", {
  # Test some ISCO88 codes
  test_codes <- c("1110", "2113", "3330", "4111", "5161")
  result <- isco88_to_oep(test_codes)
  
  expect_false(any(is.na(result)), info = "No NAs should be present in ISCO88 results")
  expect_length(result, 5)
  expect_type(result, "character")
})

test_that("isco88_to_oep handles mixed digit codes correctly", {
  # Test codes that have different trailing zero patterns
  mixed_codes <- c("1110", "2330", "3000", "4000")
  result <- isco88_to_oep(mixed_codes)
  
  # All should return valid results, no NAs
  expect_false(any(is.na(result)))
  expect_length(result, 4)
  expect_type(result, "character")
})

test_that("both oep functions maintain consistency between individual and vectorized processing", {
  # Test that processing individually vs as vector gives same results
  test_codes <- c("4110", "3413", "2330", "3111", "2161")
  
  # Process as vector
  vector_result <- isco08_to_oep(test_codes)
  
  # Process individually
  individual_results <- sapply(test_codes, isco08_to_oep)
  names(individual_results) <- NULL  # Remove names for comparison
  
  expect_equal(vector_result, individual_results, 
               info = "Vectorized and individual processing should give identical results")
})

test_that("oep functions return character vectors", {
  result08 <- isco08_to_oep("4110")
  result88 <- isco88_to_oep("1110")
  
  expect_type(result08, "character")
  expect_type(result88, "character")
})

test_that("oep functions handle to_factor parameter", {
  result_char <- isco08_to_oep("4110", to_factor = FALSE)
  result_factor <- isco08_to_oep("4110", to_factor = TRUE)
  
  expect_type(result_char, "character")
  expect_s3_class(result_factor, "factor")
})