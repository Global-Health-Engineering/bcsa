# install.packages("cffr")
library(cffr)

packageVersion("cffr")

# Hard code doi
doi <- "10.5281/zenodo.10878608"

# creates CFF with all author roles
mod_cff <- cff_create("DESCRIPTION",
                      dependencies = FALSE,
                      keys = list("doi" = doi,
                                  "date-released" = Sys.Date()))

# Remove the preferred-citation key
mod_cff$`preferred-citation` <- NULL

# writes the CFF file
cff_write(mod_cff)

# Now write a CITATION file from the CITATION.cff file
# Use inst/CITATION instead (the default if not provided)
path_cit <- file.path("inst/CITATION")

a_cff <- cff_read(path = "CITATION.cff")

cff_write_citation(a_cff, file = path_cit)

# By last, read the citation
cat(readLines(path_cit), sep = "\n")
