library(Reconcile)
context("Check for partial visits")

repository <- data.frame(subject = c(LETTERS[1:5], LETTERS[1:5]), visit = rep(1:2, each = 5), 
                         specimen = c(rep("blood", 5), rep("DNA", 5)),
                         stringsAsFactors = FALSE)

site <- data.frame(patno = c(LETTERS[2:6], LETTERS[2:6]), clinevent = rep(1:2, each = 5), 
                   sample = c("RNA", rep("blood", 4), "Serum", rep("DNA",  4)),
                   stringsAsFactors = FALSE)


test.visits <- reconcileVisits(repository.data = repository, site.data = site, repo.subject.column = "subject", repo.visit.column = "visit",
                repo.specimen.column = "specimen", site.subject.column = "patno", site.visit.column = "clinevent",
                site.specimen.column = "sample")

repo.spec.na <- filter(test.visits, is.na(repository_specimen))
site.spec.na <- filter(test.visits, is.na(site_specimen))


test_that("the column names are what they should be", {
  expect_equal(names(test.visits), c("subject", "visit", "repository_specimen", "site_specimen"))
  
})

test_that("the NAs in the repository_specimen column don't have values in the repository table", {
  expect_true(dim(semi_join(repository, repo.spec.na, by = c("subject", "visit", "specimen" = "site_specimen")))[1] == 0)
})

test_that("the NAs in the site_specimen column don't have values in the site table", {
  expect_true(dim(semi_join(site, site.spec.na, by = c("patno" = "subject", "clinevent" = "visit", "sample" = "repository_specimen")))[1] == 0)
})

complete.visits <- group_by(test.visits, subject, visit) %>% 
  mutate(sumNAs = sum(is.na(site_specimen), is.na(repository_specimen))) %>%
  filter(sumNAs == 0)

test_that("there are no visits that have complete specimen collections", {
  expect_true(dim(complete.visits)[1] == 0)
})
