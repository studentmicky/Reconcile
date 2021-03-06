% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reconcileAliquots.R
\name{reconcileAliquots}
\alias{reconcileAliquots}
\title{Checks if there are unequal aliquots for a subject/visit/specimen between the site and biorepository}
\usage{
reconcileAliquots(repository.data, site.data, repo.subject.column,
  repo.visit.column, repo.specimen.column, site.subject.column,
  site.visit.column, site.specimen.column)
}
\arguments{
\item{repository.data}{\code{data.frame} of repository data with columns that will provide unique rows of interest}

\item{site.data}{\code{data.frame} of site data with columns that will provide unique rows of interest}

\item{repo.subject.column}{Column name of subject IDs in repository \code{data.frame}}

\item{repo.visit.column}{Column name of visit in repository \code{data.frame}}

\item{repo.specimen.column}{Column name of specimen in repository \code{data.frame}}

\item{site.subject.column}{Column name of subject IDs in site \code{data.frame}}

\item{site.visit.column}{Column name of visit in repository \code{data.frame}}

\item{site.specimen.column}{Column name of specimen in site \code{data.frame}}
}
\value{
A \code{data.frame} with column names \code{subject}, \code{visit}, \code{specimen},
\code{repository_aliquots}, and \code{site_aliquots}.
}
\description{
This function helps you determine if the same number of aliquots for a particular subject/visit/specimen are in
the site data and the biorepository data.
}
\examples{
repository <- data.frame(subject = c(LETTERS[1:5], LETTERS[1:5], LETTERS[1:3]),
                         visit = c(1:5, 1:5, 1:3),
                         specimen = c(rep("blood", 5), rep("DNA", 5), "blood", "blood", "DNA"),
                         stringsAsFactors = FALSE)

site <- data.frame(patno = c(LETTERS[2:6], LETTERS[2:6]), clinevent = c(2:6, 2:6),
                   sample = c("RNA", rep("blood", 4), "Serum", rep("DNA",  4)),
                   stringsAsFactors = FALSE)

reconcileAliquots(repository.data = repository, site.data = site, repo.subject.column = "subject",
                  repo.visit.column = "visit", repo.specimen.column = "specimen",
                  site.subject.column = "patno", site.visit.column = "clinevent",
                  site.specimen.column = "sample")
}

