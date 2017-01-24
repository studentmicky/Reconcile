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
    # column = "aliquots"
    x_anti_join <- anti_join(x[, c(column, id)], y[, c(column, id)])
    if(dim(x_anti_join)[1] == 0){
      names(x_anti_join)[names(x_anti_join) == column] <- "x"
      x_anti_join[1, ] <- c(NA, NA)
      x_anti_join$y <- NA
      return(x_anti_join)
    }
    x_anti_join[, column] <- paste(column, x_anti_join[, column], sep = " = ")
    names(x_anti_join)[names(x_anti_join) == column] <- "x"
    y_anti_join <- anti_join(y[, c(column, id)], x[, c(column, id)])
    y_anti_join[, column] <- paste(column, y_anti_join[, column], sep = " = ")
    names(y_anti_join)[names(y_anti_join) == column] <- "y"
    inner_join(x_anti_join, y_anti_join)
  }))
  error_table <- Reduce(rbind, anti_joins)
  if(dim(error_table)[1] == 0) {
    error_table <- data.frame(error_number = integer(), id = character(),
                              x = character(), y = character())
    return(error_table)
  }
  error_table <- filter(error_table, !is.na(x))
  error_table <- arrange_(error_table, id)
  error_table$error_number <- 1:dim(error_table)[1]
  error_table <- error_table[, c("error_number", id, "x", "y")]
  error_table
}


