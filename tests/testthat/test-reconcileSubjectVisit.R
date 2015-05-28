library(Reconcile)
context("Something/something comparisons")

repository <- data.frame(subject = c(LETTERS[1:5], LETTERS[1:5]), visit = c(1:5, 1:5), 
                specimen = c(rep("blood", 5), rep("DNA", 5)),
                stringsAsFactors = FALSE)

site <- data.frame(patno = c(LETTERS[2:6], LETTERS[2:6]), clinevent = c(2:6, 2:6), 
                specimen = c("RNA", rep("blood", 4), "Serum", rep("DNA",  4)),
                stringsAsFactors = FALSE)

test_that("data.no.samples works",{
  expect_equal(names(reconcileSubjectVisits(report.type = "data.no.samples", repository.data = repository,
                                            site.data = site, repo.subject.column = "subject", 
                                            repo.visit.column = "visit", site.subject.column = "patno", 
                                            site.visit.column = "clinevent")), c("patno", "clinevent"))
  expect_equal(dim(reconcileSubjectVisits(report.type = "data.no.samples", repository.data = repository,
                                          site.data = site, repo.subject.column = "subject", 
                                          repo.visit.column = "visit", site.subject.column = "patno", 
                                          site.visit.column = "clinevent")), c(1, 2))
  expect_equal(reconcileSubjectVisits(report.type = "data.no.samples", repository.data = repository,
                                          site.data = site, repo.subject.column = "subject", 
                                          repo.visit.column = "visit", site.subject.column = "patno", 
                                          site.visit.column = "clinevent")[1, 1], "F")
  expect_equal(reconcileSubjectVisits(report.type = "data.no.samples", repository.data = repository,
                                      site.data = site, repo.subject.column = "subject", 
                                      repo.visit.column = "visit", site.subject.column = "patno", 
                                      site.visit.column = "clinevent")[1, 2], 6)
})

test_that("samples.no.data works",{
  expect_equal(names(reconcileSubjectVisits(report.type = "samples.no.data", repository.data = repository,
                                            site.data = site, repo.subject.column = "subject", 
                                            repo.visit.column = "visit", site.subject.column = "patno", 
                                            site.visit.column = "clinevent")), c("subject", "visit"))
  expect_equal(dim(reconcileSubjectVisits(report.type = "samples.no.data", repository.data = repository,
                                          site.data = site, repo.subject.column = "subject", 
                                          repo.visit.column = "visit", site.subject.column = "patno", 
                                          site.visit.column = "clinevent")), c(1, 2))
  expect_equal(reconcileSubjectVisits(report.type = "samples.no.data", repository.data = repository,
                                      site.data = site, repo.subject.column = "subject", 
                                      repo.visit.column = "visit", site.subject.column = "patno", 
                                      site.visit.column = "clinevent")[1, 1], "A")
  expect_equal(reconcileSubjectVisits(report.type = "samples.no.data", repository.data = repository,
                                      site.data = site, repo.subject.column = "subject", 
                                      repo.visit.column = "visit", site.subject.column = "patno", 
                                      site.visit.column = "clinevent")[1, 2], 1)
})
