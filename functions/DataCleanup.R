cleandownloaded <- function() {
  
  ch <- odbcConnect("pdfScraper")
  pdfFileProcessingStatus<-sqlFetch(ch,"dbo.vw_pdfFileProcessingStatus",as.is = c(TRUE), stringsAsFactors = FALSE)
  odbcClose(ch)
  
  wackydownloads <- subset(pdfFileProcessingStatus, (Downloaded == 0 & Scraped==1 & TaxonomyAnalyzed==1))
  
  tempDownloadStatus<-data.frame(DigitalFileID=wackydownloads$DigitalFileID,Downloaded=1)

  ch <- odbcConnect("pdfScraper")
  sqlSave(ch,tempDownloadStatus,"dbo.tbl_DownloadStatus",append=TRUE,rownames=FALSE)
  pdfFileProcessingStatus<-sqlFetch(ch,"dbo.vw_pdfFileProcessingStatus",as.is = c(TRUE), stringsAsFactors = FALSE)
  odbcClose(ch)
  
}

afterdownloadfail <- function() {
  
  # Create Corpus from Downloaded Files
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
  hits<-as.data.frame(kwic(docscorpus,phrase(dicSpecies),valuetype="fixed", include_docvars=TRUE))
  names(hits)[5]<-"SciName"
  
  # extract the doc IDs and update the analyzed table
  for (i in 1:nrow(docs)) {
    tempAnalysisStatus<-rbind(tempAnalysisStatus,data.frame(DigitalFileID=docs$docvar1[i],TaxonomyAnalyzed=1))
  }
  
  # Write data to SQL table
  ch <- odbcConnect("pdfScraper")
  sqlSave(ch,hits,"dbo.tbl_TaxonomicScrapingResults",append=TRUE,rownames=FALSE)
  sqlSave(ch,tempAnalysisStatus,"dbo.tbl_TaxonomyAnalysisStatus",append=TRUE,rownames=FALSE)
  pdfFileProcessingStatus<-sqlFetch(ch,"dbo.vw_pdfFileProcessingStatus",as.is = c(TRUE), stringsAsFactors = FALSE)
  odbcClose(ch)
  
  ch <- odbcConnect("pdfScraper")
  pdfFileProcessingStatus<-sqlFetch(ch,"dbo.vw_pdfFileProcessingStatus",as.is = c(TRUE), stringsAsFactors = FALSE)
  odbcClose(ch)
  
  wackydownloads <- subset(pdfFileProcessingStatus, (Downloaded == 0 & Scraped==1 & TaxonomyAnalyzed==1))
  
  tempDownloadStatus<-data.frame(DigitalFileID=wackydownloads$DigitalFileID,Downloaded=1)
  
  ch <- odbcConnect("pdfScraper")
  sqlSave(ch,tempDownloadStatus,"dbo.tbl_DownloadStatus",append=TRUE,rownames=FALSE)
  pdfFileProcessingStatus<-sqlFetch(ch,"dbo.vw_pdfFileProcessingStatus",as.is = c(TRUE), stringsAsFactors = FALSE)
  odbcClose(ch)
  
  # Delete Files in the directory
  FilesNotDownloaded<-subset(pdfFileProcessingStatus, Downloaded != 1)
  file.remove(file.path("d:/texts", list.files("d:/texts"))) 
  
}

accessdenied <- function(FileID) {
  
  ch <- odbcConnect("pdfScraper")
  sqlSave(ch,data.frame(DigitalFileID=FileID),"dbo.vw_AddRestrictedFile",append=TRUE,rownames=FALSE)
  pdfFileProcessingStatus<-sqlFetch(ch,"dbo.vw_pdfFileProcessingStatus",as.is = c(TRUE), stringsAsFactors = FALSE)
  odbcClose(ch)
  
}