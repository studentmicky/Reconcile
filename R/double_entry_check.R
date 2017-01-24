#' Double data entry check
#'
#' This function takes two data frames that have identical column names, 
#' which should also have idenctical records, and returns a table of errors where 
#' the two tables don't match up.
#'
#' @param x,y Two \code{data.frame}s with identical column names
#' @param id Character vector of column(s) to join the two \code{data.frame}s
#' @return A \code{data.frame} with columns \code{id}, \code{field}, \code{x},
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
#' errors <- double_entry_check(repository_A, repository_B, id = c("subject", "visit", "case"))
#' @export
#' @import dplyr tidyr
double_entry_check <- function(x, y, id) {
  
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
  } else {
    names(x)[names(x) == id] <- "id"
    names(y)[names(y) == id] <- "id"
  }
  if(sum(!(x$id %in% y$id)) > 0 | sum(!(y$id %in% x$id)) > 0){
    warning("Some ids don't match")
  }
  
  x <- gather_(x, "field", "x", names(x)[names(x) != "id"])
  y <- gather_(y, "field", "y", names(y)[names(y) != "id"])
  
  error_table <- inner_join(x, y, c("id", "field")) %>%
    filter(x != y | is.na(x) & !is.na(y) | !is.na(x) & is.na(y))
  
  error_table
}
