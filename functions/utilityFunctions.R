#' notNAs
#'
#' @param vector Input vector to be cleaned
#'
#' @return Vector stripped of any NA values.
#' @export
#'
#' Simple function to remove NA values from input vector
#'
notNAs <- function(vector) {
  vector <- vector[!is.na(vector)]
  return(vector)
}




#' notEmpty
#'
#' @param vector Input vector to be cleaned
#'
#' @return Vector stripped of any empty values.
#' @export
#'
#' @examples
#'
#' Simple function to remove empty elements from a vector
#'
notEmpty <- function(vector) {
  vector <- vector[!grepl(x = vector, pattern = "^$")]
  return(vector)
}




#' matchHMDB
#'
#' @param hmdbID HMDB ID to be cleaned and returned
#'
#' @return Sanitized HMDB IDs which can be used in mapping.
#' @export
#'
#' MetaCyc only supports the older format, five digit HMDB IDs. If we detect
#' your HMDB IDs are in the newer seven digit format, we will trim the leading
#' characters if they are zeros. If they are not zeros, we will return an error
#'
matchHMDB <- function(hmdbID) {

  # Make sure the ID is a character amd starts with 'HMDB' or 'hmdb' Look at the
  # syntax very carefully here, the parentheses are IMPORTANT
  if (!is.character(hmdbID) | !(str_detect(hmdbID, "^HMDB") | str_detect(hmdbID, "^hmdb"))) {
    return(NA)

  # If the ID is in the new, 7 digit format, check the leading digits
  } else if (nchar(hmdbID) == 11) {

    # If the leading characters are 00, simply trim the string
    if (str_sub(hmdbID, start = 5, end = 6) == "00") {
      newID <- paste0("HMDB", str_sub(hmdbID, start = -5, end = -1))
      return(newID)

    # Otherwise, return an error
    } else {
      return(NA)
    }

  # Otherwise, if the ID is in the older, 5-digit format, simply return the ID
  # as-is.
  } else if (nchar(hmdbID) == 9) {

    # Do this **anyways** because it'll ensure we have capital letters at the
    # start of the ID
    newID <- paste0("HMDB", str_sub(hmdbID, start = -5, end = -1))
    return(newID)

  # If there is an edge case where the ID is not 9 or 11 charactesr in length(),
  # also return NA
  } else {
    return(NA)
  }
}




# Pattern/replacement pairs for next function. Maybe should be moved into app.R
# or deferred.R?
find_replace <- c(
  "<.*?>" = "",
  "&harr;" = "<-->",
  "&rarr;" = "-->",
  "&larr;" = "<--",
  "&alpha;" = "a",
  "&beta;"  = "b",
  "&omega;" = "o",
  "&gamma;" = "g"
)


#' cleanReactions
#'
#' @param metabTable
#'
#' @return Clean version of output table for download purposes
#'
#' @export
#'
#' Removes any HTML tags in reaction names. Also replaces arrow marks with plain
#' text version, along with Greek letters. Using above-defined named vector so
#' we can perform multiple sets of pattern-replacement in a single call to
#' str_replace_all().
#'
cleanReactions <- function(metabTable) {
  metabTable %>% mutate(
    `Reaction Name` = stringr::str_replace_all(`Reaction Name`, find_replace)
  )
}