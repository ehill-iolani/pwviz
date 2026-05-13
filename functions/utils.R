# Parses an organization string that may be stored as an R vector literal.
# e.g. c("Org A", "Org B") -> c("Org A", "Org B")
parse_orgs <- function(x) {
  if (is.null(x) || is.na(x) || x == "") return(character(0))
  x <- as.character(x)
  if (grepl('^\\s*c\\s*\\(', x)) {
    matches <- regmatches(x, gregexpr('"([^"\\\\]*(?:\\\\.[^"\\\\]*)*)"', x, perl = TRUE))
    if (length(matches) && length(matches[[1]]) > 0) {
      return(trimws(gsub('^"|"$', '', matches[[1]])))
    }
    tmp <- gsub('^\\s*c\\s*\\(|\\)\\s*$', '', x)
    tmp <- gsub('"', '', tmp)
    parts <- trimws(unlist(strsplit(tmp, '\\s*,\\s*')))
    return(parts[parts != ""])
  }
  parts <- gsub('^"|"$', '', trimws(unlist(strsplit(x, '\\s*,\\s*'))))
  parts[parts != ""]
}

# Formats an organization entry for HTML display, joining multiple orgs with <br/>.
format_org_html <- function(x) {
  orgs <- parse_orgs(x)
  if (length(orgs) == 0) return(NA_character_)
  paste(orgs, collapse = "<br/>")
}
