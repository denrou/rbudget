#' @name read_qif
#' 
#' @title read_qif
#' 
#' @description Reads a file in qif format and creates a data frame from it, with operations corresponding to lines and variables to fields in the file.
#' 
#' @param file the name of the file which the data are to be read from. Each recording operation is separated by a blank line in the file. File is read from \code{\link{read.table}}
#' 
#' @return A data frame containing a representation of the data in the file.
#' 
#' @export

library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)

read_qif <- function(file) {
  file %>%
    read.table() %>% 
    # First line of a qif file indicates the origin
    filter(row_number() != 1) %>%
    # Each operation is separated by an empty line
    mutate(newline = str_count(V1, "") == 1) %>%
    mutate(operation = cumsum(newline)) %>% 
    filter(!newline) %>%
    # The type of the variable is define by the first letter
    mutate(col = str_extract(V1, "^.?")) %>%
    spread(col, V1) %>%
    select(-newline, -operation) %>%
    mutate(Date = mdy(str_replace(str_replace(D, "^D", ""), "'", "/")),
           Description = str_replace(P, "^P", ""),
           Amount = as.numeric(str_replace(str_replace(T, "^T", ""), ",", "")))
}
