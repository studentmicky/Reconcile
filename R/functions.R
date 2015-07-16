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
#' @return A \code{data.frame} with columns \code{subject}, \code{visit}, \code{repository_specimen},
#' and \code{site_specimen}. An \code{NA} indicates that the corresponding specimen type was not found.
#' For example, if a subject/visit contains \code{"DNA"} in the 
#' \code{repository_specimen} column and \code{NA} in the \code{site_specimen} column, then the site data is 
#' missing a DNA sample for that subject/visit. (This does not give any information about the number of samples
#' for each specimen--only whether or not at least a single specimen for each subject/visit is present in either data
#' set.)
#' @examples
#' repository <- data.frame(subject = c(LETTERS[1:5], LETTERS[1:5]), visit = c(1:5, 1:5), 
#'                          specimen = c(rep("blood", 5), rep("DNA", 5)),
#'                          stringsAsFactors = FALSE)
#' 
#' site <- data.frame(patno = c(LETTERS[2:6], LETTERS[2:6]), clinevent = c(2:6, 2:6), 
#'                    sample = c("RNA", rep("blood", 4), "Serum", rep("DNA",  4)),
#'                    stringsAsFactors = FALSE)
#'                    
#' reconcileVisits(repository.data = repository, site.data = site, repo.subject.column = "subject", 
#'                 repo.visit.column = "visit", repo.specimen.column = "specimen", site.subject.column = "patno",
#'                 site.visit.column = "clinevent", site.specimen.column = "sample")

reconcileVisits <- function(repository.data, site.data, repo.subject.column, repo.visit.column,
                            repo.specimen.column, site.subject.column, site.visit.column,
                            site.specimen.column 
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
  merged.df
}

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

#' Double data entry check
#' 
#' This function takes two data frames that have identical column names, which should also have idenctical records,
#' and returns a table of errors where the two tables don't match up.
#' 
#' @param x,y Two \code{data.frame}s with identical column names
#' @param id Character vector of column(s) to join the two \code{data.frame}s
#' @return A \code{data.frame} with columns \code{error_number}, \code{id}, \code{x}, 
#' and \code{y}. The last two columns specify what the discrepancy is for each input
#' \code{data.frame}.
#' @examples 
#' repository_A <- data.frame(subject = rep(LETTERS[1:5], 2), visit = rep(1:5, 2), case = 1:10,
#'                            specimen = c(rep("DNA", 2), rep("Plasma", 3), 
#'                            rep("Serum", 3), rep("RNA", 2)),
#'                            aliquots = 1:10, stringsAsFactors = FALSE)
#' 
#' repository_B <- data.frame(subject = c(rep(LETTERS[1:5], 2), rep(LETTERS[6], 2)), 
#'                            visit = c(rep(1:5, 2), 6:7), case = 1:12,
#'                            specimen = c(rep("DNA", 2), rep("Plasma", 2), "RNA", 
#'                                         rep("Serum", 2), rep("RNA", 2), rep("DNA", 3)),
#'                            aliquots = c(NA, 2:8, 12, 10, 11, 13), stringsAsFactors = FALSE)
#'                            
#' errors <- doubleDataEntry(repository_A, repository_B, id = c("subject", "visit", "case"))          

doubleDataEntry <- function(x, y, id){
  # x = repository_A; y = repository_B; id = c("subject", "visit", "case")
  if(sum(!(names(x) %in% names(y))) > 0 | sum(!(names(y) %in% names(x))) > 0){
    stop("column names aren't identical")
  }
  if(length(id) > 1){
    id_x <- do.call(paste, c(x[id], sep="-")) 
    x <- select(x, one_of(names(x)[!(names(x) %in% id)]))
    x$id <- id_x
    id_y <- do.call(paste, c(y[id], sep="-"))
    y <- select(y, one_of(names(y)[!(names(y) %in% id)]))
    y$id <- id_y
    id <- "id"
  }
  if(sum(!(x$id %in% y$id)) > 0 | sum(!(y$id %in% x$id)) > 0){
    warning("Some ids don't match") 
  }
  anti_joins <- suppressMessages(lapply(names(x)[names(x) != id], function(column){
    x_anti_join <- anti_join(x[, c(column, id)], y[, c(column, id)])
    x_anti_join[, column] <- paste(column, x_anti_join[, column], sep = " = ")
    names(x_anti_join)[names(x_anti_join) == column] <- "x"
    y_anti_join <- anti_join(y[, c(column, id)], x[, c(column, id)])
    y_anti_join[, column] <- paste(column, y_anti_join[, column], sep = " = ")
    names(y_anti_join)[names(y_anti_join) == column] <- "y"
    inner_join(x_anti_join, y_anti_join)
  }))
  error_table <- Reduce(rbind, anti_joins)
  error_table <- arrange(error_table, id)
  error_table$error_number <- 1:dim(error_table)[1]
  error_table <- error_table[, c("error_number", id, "x", "y")]
  error_table
}
