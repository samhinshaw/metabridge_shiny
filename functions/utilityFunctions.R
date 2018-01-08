notNAs <- function(vector) {
  vector <- vector[!is.na(vector)]
  return(vector)
}

notEmpty <- function(vector) {
  vector <- vector[!grepl(x = vector, pattern = "^$")]
  return(vector)
}

# MetaCyc only supports the older format, 5 digit HMDB IDs. If we detect your
# HMDB IDs are in the newer 7 digit formar, we will trim the leading characters
# if they are zeros. If they are not zeros, we will return an error.
matchHMDB <- function(hmdbID) {
  # Make sure the ID is a character amd starts with 'HMDB' or 'hmdb'
  # Look at the syntax very carefully here, the parens are IMPORTANT
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
    # Otherwise, if the ID is in the older, 5-digit format, simply return the ID as-is.
  } else if (nchar(hmdbID) == 9) {
    # Do this **anyways** because it'll ensure we have capital letters at the start of the ID
    newID <- paste0("HMDB", str_sub(hmdbID, start = -5, end = -1))
    return(newID)
    # If there is an edge case where the ID is not 9 or 11 charactesr in length(), also return NA
  } else {
    return(NA)
  }
}
