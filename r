pollutantmean <- function(directory, pollutant, id = 1:332){

  allFiles <- list.files(path = directory, full.names = TRUE)
  selectedData <-data.frame()
  for (i in id) {
    selectedData <- rbind(selectedData, read.csv(allFiles[i]))
  }
  if (pollutant == 'sulfate'){
    mean(selectedData$sulfate, trim=0, na.rm = TRUE)
  } else if (pollutant == 'nitrate') {
    mean(selectedData$nitrate, trim=0, na.rm = TRUE)
  }
}


complete <- function(directory,id = 1:332) {  
    full_list <- list.files(directory,full.names = TRUE)        #creates list of files
    
    df <- data.frame(id=numeric(),nobs=numeric())
    one.row <- data.frame(id=numeric(),nobs=numeric())

        for (i in id) {
            nobs <- sum(complete.cases(read.csv(full_list[i])))
            one.row <-c(i,nobs)
            df <- rbind(df,one.row) }
    names(df) <-c("id", "nobs")
    
    print(df)
  }
