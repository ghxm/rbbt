
#' @title Get set difference of two vectors, allowing for regex matches
#' @description Get set difference of two vectors, allowing for regex matches
#' @param x A vector
#' @param y A vector containing a regular expressiom preceded by regex_char
#' @param regex_char A character string to use as a regex indicator in y
#' @return A vector of set difference, setdiff(x, y) if no regex matches
setdiff_regex <- function(x, y, regex_char = " ") {

    u <- as.vector(x)
    v <- as.vector(y)

    # check if any in v starts with regex_char
    if (is.character(v) && any(startsWith(v, regex_char))) {

      # get indices of regex_char
      regex_indices <- which(startsWith(v, regex_char))

      # remove regex_char from v
      v <- gsub(paste0("^", regex_char), "", v)

      # get regexes
      regexes <- v[regex_indices]

      # remove regexes from v
      v <- v[-regex_indices]

      # get indices of regex matches
      regex_matches <- c(apply(sapply(regexes, function(x) grepl(x, u)), 2, which))

      # remove regex matches from u
      u <- u[-regex_matches]

    }

    # get set difference for non-regex entries
    setdiff(u, v)

}