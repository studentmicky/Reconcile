library(Reconcile)
context("Check for unequal aliquots")

repository <- data.frame(subject = c(LETTERS[1:5], LETTERS[1:5], LETTERS[1:3]), visit = c(1:5, 1:5, 1:3), 
                         specimen = c(rep("blood", 5), rep("DNA", 5), "blood", "blood", "DNA"),
                         stringsAsFactors = FALSE)

site <- data.frame(patno = c(LETTERS[2:6], LETTERS[2:6]), clinevent = c(2:6, 2:6), 
                   sample = c("RNA", rep("blood", 4), "Serum", rep("DNA",  4)),
                   stringsAsFactors = FALSE)


test.aliquots <- reconcileAliquots(repository.data = repository, site.data = site, repo.subject.column = "subject", repo.visit.column = "visit",
                               repo.specimen.column = "specimen", site.subject.column = "patno", site.visit.column = "clinevent",
                               site.specimen.column = "sample")

repo.aliq.na <- filter(test.aliquots, is.na(repository_aliquots))
site.aliq.na <- filter(test.aliquots, is.na(site_aliquots))

repo.aliq.num <- filter(test.aliquots, !is.na(repository_aliquots))
site.aliq.num <- filter(test.aliquots, !is.na(site_aliquots))

test_that("reconcileAliquots works", {
  expect_equal(names(test.aliquots), c("subject", "visit", "specimen", "repository_aliquots", "site_aliquots"))
  expect_true(dim(semi_join(repository, repo.aliq.na, by = c("subject", "visit", "specimen")))[1] == 0)
  expect_true(dim(semi_join(site, site.aliq.na, by = c("patno" = "subject", "clinevent" = "visit", "sample" = "specimen")))[1] == 0)
  expect_true(
    sum(apply(repo.aliq.num, 1, function(row){
      !(dim(filter(repository, subject == row["subject"], visit == row["visit"], specimen == row["specimen"]))[1] == as.numeric(row["repository_aliquots"]))
    })) == 0
    )
  expect_true(
    sum(apply(site.aliq.num, 1, function(row){
      !(dim(filter(site, patno == row["subject"], clinevent == row["visit"], sample == row["specimen"]))[1] == as.numeric(row["site_aliquots"]))
    })) == 0
  )
})
