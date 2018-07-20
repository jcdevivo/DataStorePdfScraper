# Get file from NPS Data Store based on the unique holding ID number

GetPdfFromDataStore<-function(holdingID){
  RestDownladURL<-paste('https://irmaservices.nps.gov/datastore-secure/v4/rest/DownloadFile/',holdingID,sep = "")
  DestinationLocation<-paste("d:/texts/",holdingID,".pdf",sep="")
  
  out<-tryCatch(
    {
      download.file(RestDownladURL,DestinationLocation,quiet=TRUE, mode="wb")
    },
    error=function(cond) {
      message(paste("access to file" , holdingID,  "is denied; file cannot be downloaded"))
      accessdenied(holdingID)
    },
    finally={
      message(paste("file", holdingID, "processed"))
    }
  )
} 

# delete file in a directory

DeletePdfFile<-function(holdingID){
  filename<-paste("d:/texts/",holdingID,".pdf",sep="")
  file.remove(filename)
}