#' Checks if there are any visit data missing from site or biorepository for each subject.
#'
#' This function looks at a subject/visit and helps you see if specimen data are missing from the
#' site or from the biorepository.
#'
#' @param repository.data \code{data.frame} of repository data with columns that will provide unique rows of interest
#' @param site.data \code{data.frame} of site data with columns that will provide unique rows of interest
#' @param repo.subject.column Column name of subject IDs in repository \code{data.frame}
#' @param repo.visit.column Column name of visit in repository \code{data.frame}
#' @param repo.specimen.column Column name of specimen in repository \code{data.frame}
#' @param site.subject.column Column name of subject IDs in site \code{data.frame}
#' @param site.visit.column Column name of visit in repository \code{data.frame}
#' @param site.specimen.column  Column name of specimen in site \code{data.frame}
#' @param simplify Simplify the output
#' @return A \code{data.frame} with columns \code{subject}, \code{visit}, \code{repository_specimen},
#' and \code{site_specimen}. An \code{NA} indicates that the corresponding specimen type was not found.
#' For example, if a subject/visit contains \code{"DNA"} in the
#' \code{repository_specimen} column and \code{NA} in the \code{site_specimen} column, then the site data is
#' missing a DNA sample for that subject/visit. (This does not give any information about the number of samples
#' for each specimen--only whether or not at least a single specimen for each subject/visit is present in either data
#' set.)
#' @examples
#' repository <- data.frame(subject = c(LETTERS[1:5], LETTERS[1:6]), visit = c(1:5, 1:6),
#'                          specimen = c(rep("blood", 5), rep("DNA", 5), "serum"),
#'                          stringsAsFactors = FALSE)
#'
#' site <- data.frame(patno = c(LETTERS[2:6], LETTERS[2:6], "F"), clinevent = c(2:6, 2:6, 6),
#'                    sample = c("RNA", rep("blood", 4), "serum", rep("DNA",  4), "serum"),
#'                    stringsAsFactors = FALSE)
#'
#' reconcileVisits(repository.data = repository, site.data = site, repo.subject.column = "subject",
#'                 repo.visit.column = "visit", repo.specimen.column = "specimen", site.subject.column = "patno",
#'                 site.visit.column = "clinevent", site.specimen.column = "sample")

reconcileVisits <- function(repository.data, site.data, repo.subject.column,
                            repo.visit.column, repo.specimen.column,
                            site.subject.column, site.visit.column,
                            site.specimen.column, simplify = FALSE
){
  names(repository.data)[names(repository.data) == repo.specimen.column] = "repository_specimen"
  names(site.data)[names(site.data) == site.specimen.column] = "site_specimen"
  repository.data$specimen = repository.data$repository_specimen
  site.data$specimen  = site.data$site_specimen
  merged.df <- merge(repository.data, site.data, by.x = c(repo.subject.column, repo.visit.column, "specimen"),
                     by.y = c(site.subject.column, site.visit.column, "specimen"), all = TRUE)
  merged.df <- subset(merged.df, select = -specimen)
  names(merged.df)[names(merged.df) %in% c(repo.subject.column, repo.visit.column)] = c("subject", "visit")
  merged.df <- group_by(merged.df, subject, visit) %>%
    mutate(totalNAs = sum(is.na(repository_specimen), is.na(site_specimen))) %>%
    filter(totalNAs > 0) %>% select(-(totalNAs))
  if(simplify){
    merged.df <- merged.df %>% group_by(subject, visit) %>%
      summarize(repository_specimen = paste(repository_specimen[!is.na(repository_specimen)], collapse = ","),
                site_specimen = paste(site_specimen[!is.na(site_specimen)], collapse = ","))
  }
  as.data.frame(merged.df)
}
