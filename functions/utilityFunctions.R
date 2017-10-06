notNAs <- function(vector) {
  vector <- vector[!is.na(vector)]
  return(vector)
}

notEmpty <- function(vector) {
  vector <- vector[!grepl(x = vector, pattern = '^$')]
  return(vector)
}