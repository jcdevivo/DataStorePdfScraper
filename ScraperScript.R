source("functions/FileProcessingFunctions.R")
source("functions/DataCleanup.R")

packages <- c("RODBC", "tm", "quanteda", "readtext")

package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE, repos = "http://cran.us.r-project.org")
    library(x, character.only = TRUE)
  }
})

doclocation <- file.path("d:/texts")
load("../NPSpecies/data/dicSpecies.rda")
load("../NPSpecies/data/NPSpeciesJustSpecies.rda")

scrapeaway <- function(chunksize){
  
  start_time <- Sys.time()
  
  ch <- odbcConnect("pdfScraper")
  pdfFileProcessingStatus<-sqlFetch(ch,"dbo.vw_pdfFileProcessingStatus",as.is = c(TRUE), stringsAsFactors = FALSE)
  odbcClose(ch)
  
  FilesNotDownloaded<-subset(pdfFileProcessingStatus, Downloaded != 1)
  
  # initialize temp download status data frame
  tempDownloadStatus<-data.frame(
    DigitalFileID=integer(),
    Downloaded=integer()
  )
  
  message(paste("Downloading next batch of", chunksize, "pdf documents..."))
  
  for (i in 1:chunksize) {
    GetPdfFromDataStore(FilesNotDownloaded$DigitalFileID[i])
    tempDownloadStatus<-rbind(tempDownloadStatus,data.frame(DigitalFileID=FilesNotDownloaded$DigitalFileID[i],Downloaded=1))
    Sys.sleep(2)
  }
  
  # update downloaded table in the SQL database
  ch <- odbcConnect("pdfScraper")
  sqlSave(ch,tempDownloadStatus,"dbo.tbl_DownloadStatus",append=TRUE,rownames=FALSE)
  pdfFileProcessingStatus<-sqlFetch(ch,"dbo.vw_pdfFileProcessingStatus",as.is = c(TRUE), stringsAsFactors = FALSE)
  odbcClose(ch)
  
  # initialize temp download status data frame
  tempScrapeStatus<-data.frame(
    DigitalFileID=integer(),
    Scraped=integer()
  )
  
  # Create Corpus from Downloaded Files
  message(paste("Extacting text from current batch of pdf documents..."))
  docs<-readtext(paste0(doclocation, "/*.pdf"),
                 docvarsfrom = "filenames",
                 dvsep = "_"
  )
  
  docscorpus<-corpus(docs)
  
  # extract the docIDs and update the scraped table
  for (i in 1:nrow(docs)) {
    tempScrapeStatus<-rbind(tempScrapeStatus,data.frame(DigitalFileID=docs$docvar1[i],Scraped=1))
  }
  
  # update downloaded table in the SQL database
  ch <- odbcConnect("pdfScraper")
  sqlSave(ch,tempScrapeStatus,"dbo.tbl_ScrapeStatus",append=TRUE,rownames=FALSE)
  pdfFileProcessingStatus<-sqlFetch(ch,"dbo.vw_pdfFileProcessingStatus",as.is = c(TRUE), stringsAsFactors = FALSE)
  odbcClose(ch)
  
  # initialize temp analysis status data frame
  tempAnalysisStatus<-data.frame(
    DigitalFileID=integer(),
    TaxonomyAnalyzed=integer()
  )
  
  # Analyze Text from files in the corpus by comparing it to the species dictionary
  message(paste("Detecting taxa mentioned in the current batch of pdf documents..."))
  hits<-as.data.frame(kwic(docscorpus,phrase(dicSpecies),valuetype="fixed", include_docvars=TRUE))
  names(hits)[5]<-"SciName"
  
  # extract the doc IDs and update the analyzed table
  for (i in 1:nrow(docs)) {
    tempAnalysisStatus<-rbind(tempAnalysisStatus,data.frame(DigitalFileID=docs$docvar1[i],TaxonomyAnalyzed=1))
  }
  
  # Write data to SQL table
  message(paste(nrow(hits), "new mentions of taxa being added to the database..."))
  ch <- odbcConnect("pdfScraper")
  sqlSave(ch,hits,"dbo.tbl_TaxonomicScrapingResults",append=TRUE,rownames=FALSE)
  sqlSave(ch,tempAnalysisStatus,"dbo.tbl_TaxonomyAnalysisStatus",append=TRUE,rownames=FALSE)
  pdfFileProcessingStatus<-sqlFetch(ch,"dbo.vw_pdfFileProcessingStatus",as.is = c(TRUE), stringsAsFactors = FALSE)
  odbcClose(ch)
  message(paste("Success!", nrow(hits), "new mentions of taxa have been added!"))
  
  # Delete Files in the directory
  FilesNotDownloaded<-subset(pdfFileProcessingStatus, Downloaded != 1)
  file.remove(file.path("d:/texts", list.files("d:/texts"))) 
  
  message(paste(nrow(FilesNotDownloaded), "pdf documents to go"))
  print(Sys.time()-start_time)
  
}

runlots <- function(chunksize,chunks){
  for (i in 1:chunks) {
    scrapeaway(chunksize)
    message(paste(i, "of", chunks, "batches complete"))
    gc()
  }
}


