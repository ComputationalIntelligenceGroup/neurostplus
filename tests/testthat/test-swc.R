context(" are these correct files")

test_that("compare", {
 skip("local")
 system("diff /home/bmihaljevic/code-gaba/neurostrplus/inst/extdata/swc/C010600B2.swc ~/code/bbp-data/data/BBP_SWC/C010600B2.swc")
 system("cat /home/bmihaljevic/code-gaba/neurostrplus/inst/extdata/swc/C010600B2.swc | wc -l ")
 system("cat ~/code/bbp-data/data/BBP_SWC/C010600B2.swc   | wc -l")
})


# to <- get_all_swc_files()
# to <- gsub('.*neurostrplus/', 'inst/',  to)
# from <- gsub('inst/extdata/swc', '~/code/bbp-data/data/BBP_SWC/', to, fixed = TRUE)
# file.copy(from, to, overwrite = TRUE)
#
# file <- '../bbpinterclassify/data/217-cells-final-db.csv'
# file <- '~/code/neuro-intermorpho/data/final-db.csv'
# db <- read.csv(file, row.names = 1)
# nrow(branches)
