demo.set <- function(dir="demofiles") {

  demos <- list()
  
  files <- Sys.glob(file.path(dir, '*'))

  for (thefile in files) {
    fi <- file(thefile, 'r')
    demolines <- readLines(fi, 1)
    while (length(demolines) > 0) {
      demoline <- trimws(demolines[1])
      if (nchar(demoline) > 0) {
        dname <- strsplit(demoline, ',')[[1]][1]
        dfile <- basename(thefile)
        demos[dname] = dfile
        break;
      }
      demolines <- readLines(fi, 1)
    }
    close(fi)
  }

  as.vector(demos)
}
