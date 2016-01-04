#' Function for doing something/something comparisons
#'
#' This function helps you see which subject/visit combinations are missing either in the site data or in the
#' biorepository data.
#'
#' @param report.type Which records to return
#' @param repository.data \code{data.frame} of repository data with columns that will provide unique rows of interest
#' @param site.data \code{data.frame} of site data with columns that will provide unique rows of interest
#' @param repo.subject.column Column name of subject IDs in repository \code{data.frame}
#' @param repo.visit.column Column name of visit in repository \code{data.frame}
#' @param site.subject.column Column name of subject IDs in site \code{data.frame}
#' @param site.visit.column Column name of visit in repository \code{data.frame}
#' @return A \code{data.frame} with the appropriate column headings. If the \code{report.type} is \code{"data.no.samples"},
#' then the column headings are from the site \code{data.frame}. If the \code{report.type} is \code{"samples.no.data"},
#' then the column headings are from the repository \code{data.frame}
#' @examples
#' repository <- data.frame(subject = c(LETTERS[1:5], LETTERS[1:5]), visit = c(1:5, 1:5),
#'                          specimen = c(rep("blood", 5), rep("DNA", 5)),
#'                          stringsAsFactors = FALSE)
#'
#' site <- data.frame(patno = c(LETTERS[2:6], LETTERS[2:6]), clinevent = c(2:6, 2:6),
#'                    specimen = c("RNA", rep("blood", 4), "Serum", rep("DNA",  4)),
#'                    stringsAsFactors = FALSE)
#'
#' # Check to see if there is data from the site but no samples at the biorepository
#' reconcileSubjectVisits(report.type = "data.no.samples", repository.data = repository,
#'                        site.data = site, repo.subject.column = "subject",
#'                        repo.visit.column = "visit", site.subject.column = "patno",
#'                        site.visit.column = "clinevent")
#'
#' # Check to see if there are samples at the biorepository but no data from the site
#' reconcileSubjectVisits(report.type = "data.no.samples", repository.data = repository,
#'                        site.data = site, repo.subject.column = "subject",
#'                        repo.visit.column = "visit", site.subject.column = "patno",
#'                        site.visit.column = "clinevent")
#'

reconcileSubjectVisits <- function(report.type = c("data.no.samples", "samples.no.data"),
                                   repository.data, site.data, repo.subject.column,
                                   repo.visit.column, site.subject.column, site.visit.column
){
  repository.data <- unique(repository.data[, c(repo.subject.column, repo.visit.column)])
  site.data <- unique(site.data[, c(site.subject.column, site.visit.column)])
  if(report.type[1] == "data.no.samples"){
    keep.df <- site.data
    compare.df <- repository.data
    merge.by.x <- c(site.subject.column, site.visit.column)
    merge.by.y <- c(repo.subject.column, repo.visit.column)
  }else if(report.type[1] == "samples.no.data"){
    keep.df <- repository.data
    compare.df <- site.data
    merge.by.x <- c(repo.subject.column, repo.visit.column)
    merge.by.y <- c(site.subject.column, site.visit.column)
  }
  compare.df$compare.index <- 1:nrow(compare.df)
  merged.df <- merge(keep.df, compare.df, by.x = merge.by.x,
                     by.y = merge.by.y, all.x = TRUE)
  merged.df <- merged.df[is.na(merged.df$compare.index), ]
  merged.df <- subset(merged.df, select = -compare.index)
  as.data.frame(merged.df)
}

