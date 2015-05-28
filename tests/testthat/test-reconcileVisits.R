library(Reconcile)
context("Check for partial visits")

repository <- data.frame(subject = c(LETTERS[1:5], LETTERS[1:5]), visit = c(1:5, 1:5), 
                         specimen = c(rep("blood", 5), rep("DNA", 5)),
                         stringsAsFactors = FALSE)

site <- data.frame(patno = c(LETTERS[2:6], LETTERS[2:6]), clinevent = c(2:6, 2:6), 
                   sample = c("RNA", rep("blood", 4), "Serum", rep("DNA",  4)),
                   stringsAsFactors = FALSE)


test.visits <- reconcileVisits(repository.data = repository, site.data = site, repo.subject.column = "subject", repo.visit.column = "visit",
                repo.specimen.column = "specimen", site.subject.column = "patno", site.visit.column = "clinevent",
                site.specimen.column = "sample")

repo.spec.na <- filter(test.visits, is.na(repository_specimen))
site.spec.na <- filter(test.visits, is.na(site_specimen))


test_that("reconcileVisits", {
  expect_equal(names(test.visits), c("subject", "visit", "repository_specimen", "site_specimen"))
  expect_true(dim(semi_join(repository, repo.spec.na, by = c("subject", "visit", "specimen" = "site_specimen")))[1] == 0)
  expect_true(dim(semi_join(site, site.spec.na, by = c("patno" = "subject", "clinevent" = "visit", "sample" = "repository_specimen")))[1] == 0)
})
