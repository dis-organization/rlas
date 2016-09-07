f <- "http://www.liblas.org/samples/srs.las"
download.file(f, sprintf("inst/extdata/lasfiles/%s", basename(f)), mode = "wb")