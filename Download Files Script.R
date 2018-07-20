packages <- c("RODBC")

package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE, repos = "http://cran.us.r-project.org")
    library(x, character.only = TRUE)
  }
})

source("functions/FileProcessingFunctions.R")

ch <- odbcConnect("pdfScraper")
allpdfs<-sqlFetch(ch,"dbo.vw_pdfFileProcessingStatus",as.is = c(TRUE), stringsAsFactors = FALSE)
odbcClose(ch)