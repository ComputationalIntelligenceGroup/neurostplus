# some file names are wrong
files <- list.files(f)
files
files <- gsub('-.*', '', files)
files
folder <- '~/code/bbp-data/data/BBP_SWC/'
from <- paste(folder, "/",  files, ".swc", sep = "")
from <- from[file.exists(from)]
from <- unique(from)
length(from)
to <- paste("inst/extdata/swc", "/", files, ".swc", sep = "")
from
to
file.copy(from, to)
