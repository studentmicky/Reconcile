#' Checks if there are unequal aliquots for a subject/visit/specimen between the site and biorepository
#'
#' This function helps you determine if the same number of aliquots for a particular subject/visit/specimen are in
#' the site data and the biorepository data.
#'
#' @param repository.data \code{data.frame} of repository data with columns that will provide unique rows of interest
#' @param site.data \code{data.frame} of site data with columns that will provide unique rows of interest
#' @param repo.subject.column Column name of subject IDs in repository \code{data.frame}
#' @param repo.visit.column Column name of visit in repository \code{data.frame}
#' @param repo.specimen.column Column name of specimen in repository \code{data.frame}
#' @param site.subject.column Column name of subject IDs in site \code{data.frame}
#' @param site.visit.column Column name of visit in repository \code{data.frame}
#' @param site.specimen.column  Column name of specimen in site \code{data.frame}
#' @return A \code{data.frame} with column names \code{subject}, \code{visit}, \code{specimen},
#' \code{repository_aliquots}, and \code{site_aliquots}.
#' @examples
#' repository <- data.frame(subject = c(LETTERS[1:5], LETTERS[1:5], LETTERS[1:3]),
#'                          visit = c(1:5, 1:5, 1:3),
#'                          specimen = c(rep("blood", 5), rep("DNA", 5), "blood", "blood", "DNA"),
#'                          stringsAsFactors = FALSE)
#'
#' site <- data.frame(patno = c(LETTERS[2:6], LETTERS[2:6]), clinevent = c(2:6, 2:6),
#'                    sample = c("RNA", rep("blood", 4), "Serum", rep("DNA",  4)),
#'                    stringsAsFactors = FALSE)
#'
#' reconcileAliquots(repository.data = repository, site.data = site, repo.subject.column = "subject",
#'                   repo.visit.column = "visit", repo.specimen.column = "specimen",
#'                   site.subject.column = "patno", site.visit.column = "clinevent",
#'                   site.specimen.column = "sample")

reconcileAliquots <- function(repository.data, site.data, repo.subject.column, repo.visit.column,
                              repo.specimen.column, site.subject.column, site.visit.column,
                              site.specimen.column
){
  repository.data$I <- 1
  repo.summary <- group_by_(repository.data, repo.subject.column, repo.visit.column, repo.specimen.column)
  repo.summary <- summarise(repo.summary, repository_aliquots = sum(I))
  
  site.data$I <- 1
  site.summary <- group_by_(site.data, site.subject.column, site.visit.column, site.specimen.column)
  site.summary <- summarise(site.summary, site_aliquots = sum(I))
  
  merged.df <- merge(repo.summary, site.summary, by.x = c(repo.subject.column, repo.visit.column, repo.specimen.column),
                     by.y = c(site.subject.column, site.visit.column, site.specimen.column), all = TRUE)
  names(merged.df)[names(merged.df) %in% c(repo.subject.column, repo.visit.column, repo.specimen.column)] = c("subject", "visit", "specimen")
  merged.df
}