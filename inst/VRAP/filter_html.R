library(stringr)

FILTER.HTML.DELIM <- "#.#"
FILTER.HTML.END <- "#.#.*END"

new.html.name <- function(old.name) {

  new.name <- NULL
  if (grepl("_complete", old.name)) {
    new.name <- str_replace(old.name, "_complete","")
  }
  new.name
}


filter.html <- function(files, tags=c('notatag')) {
  
## The tags array should include all tags to keep - any others will be
## discarded.

  utags <- paste0(FILTER.HTML.DELIM,toupper(tags))

  for (infile in files) {
    outfile = new.html.name(infile)
    .flag = TRUE
    if (!is.null(outfile) && file.exists(infile)) {
      
      inf <- file(infile, "r")
      inlines <- readLines(inf)
      close(inf)

      outlines <- vector(mode="logical", length=length(inlines))
      outlines <- rep(TRUE, length(outlines))
      
      keep.flag <- TRUE

      index <- 0
      for (inline in inlines) {
        index <- index + 1
        if (startsWith(inline, FILTER.HTML.DELIM)) {
                    outlines[index] <- FALSE
          if (any(grepl(FILTER.HTML.END, inline))) {
            keep.flag <- TRUE
          } else if (!any(startsWith(inline, utags))) {
          keep.flag <- FALSE
          } 
        } else {
          outlines[index] <- keep.flag
        }
      }
      of <- file(outfile, "w")
      writeLines(inlines[outlines], of)
      close(of)
    }
  }
}
