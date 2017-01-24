library(testthat)
library(Reconcile)
context("Check double data entry")

repository_A <- data.frame(subject = rep(LETTERS[1:5], 2), visit = rep(1:5, 2), case = 1:10,
                           specimen = c(rep("DNA", 2), rep("Plasma", 3), 
                                        rep("Serum", 3), rep("RNA", 2)),
                           aliquots = 1:10, stringsAsFactors = FALSE)

repository_B <- data.frame(subject = c(rep(LETTERS[1:5], 2), rep(LETTERS[6], 2)), 
                           visit = c(rep(1:5, 2), 6:7), case = 1:12,
                           specimen = c(rep("DNA", 2), rep("Plasma", 2), "RNA", 
                                        rep("Serum", 2), rep("RNA", 2), rep("DNA", 3)),
                           aliquots = c(NA, 2:7, 19, 12, 10, 11, 13), stringsAsFactors = FALSE)

repository_C <- data.frame(subject = c(rep(LETTERS[1:5], 2), rep(LETTERS[6], 2)), 
                           visit = c(rep(1:5, 2), 6:7), case = 1:12,
                           specimen = c(rep("DNA", 2), "Plasma", rep("RNA", 2), 
                                        rep("Serum", 2), rep("RNA", 2), rep("DNA", 3)),
                           aliquots = c(1:12), stringsAsFactors = FALSE)

repository_D <- repository_B
names(repository_D)[5] <- "aliquot"

repository_E <- repository_A
repository_E[10, "specimen"] <- "DNA"

repository_F <- repository_A[1, ]


test1 <- doubleDataEntry(repository_A, repository_B, id = c("subject", "visit", "case"))

test2 <- doubleDataEntry(repository_B, repository_C, id = c("subject", "visit", "case"))

test3 <- doubleDataEntry(repository_A, repository_E, id = c("subject", "visit", "case"))



test_that("function stops if column names are different", {
  expect_error(doubleDataEntry(repository_A, repository_D, "case"), "column names aren't identical")
})

test_that("warning message for ids that don't match", {
  expect_warning(doubleDataEntry(repository_A, repository_B, id = c("subject", "visit", "case")),
                 "Some ids don't match", fixed = TRUE)
})

test_that("output column names are correct",{
  expect_equal(names(test1), c("error_number", "id", "x", "y"))
})

test_that("id columns are pasted together", {
  expect_equal(length(strsplit(test1$id[1], "-")[[1]]), length(c("subject", "visit", "case")))
})

test_that("output catches number of errors", {
  expect_equal(dim(test1)[1], 6)
  expect_equal(dim(test2)[1], 5)
  expect_equal(dim(test3)[1], 1)
})

