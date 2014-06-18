pollutantmean <- function(directory, pollutant, id = 1:332) {
        
        setwd("C:/Users/rolande.mbatchou/Desktop/Data Science/R Programming/Programming Assignment 1/specdata")
        myFiles <- list.files(pattern="csv")
        subdata <- numeric()
        for (i in id) {
                subdata <- rbind(subdata, read.csv(myFiles[i], sep=",", header=TRUE))
        }
        
          return(mean(subdata[,pollutant], na.rm=TRUE))
}