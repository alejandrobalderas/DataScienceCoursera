
pollutantmean <- function(directory, pollutant, id = 1:332){
    tmpDir <- paste(directory,'/',id,'.csv',sep="")
    tmpData <- read.csv(tmpDir)
    
    if(pollutant == 'sulfate'){
        tmpValues <- tmpData$sulfate
    }else if(pollutant == 'nitrate'){
        tmpValues <- tmpData$nitrate
    }
    val = mean(tmpValues,na.rm = TRUE)
}




pollutantmean2 <- function(directory, pollutant, id = 1:332){
    
    tmpFun <- function(directory,pollutant,idNum){
        tmpDir <- paste(directory,'/',sprintf("%03d",idNum),'.csv',sep="")
        tmpData <- read.csv(tmpDir)
        
        if(pollutant == 'sulfate'){
            tmpValues <- tmpData$sulfate
        }else if(pollutant == 'nitrate'){
            tmpValues <- tmpData$nitrate
        }
        val <- mean(tmpValues,na.rm = TRUE)
        val
    }
    val <- mean(sapply(id, function(x) tmpFun(directory,pollutant,x)),na.rm = TRUE)
    val
}


complete <- function(directory, id = 1:332){
    
    tmpFun_na <-function(directory,idNum){
        tmpDir <- paste(directory,'/',sprintf("%03d",idNum),'.csv',sep="")
        tmpData <- read.csv(tmpDir)
        my_na <- !is.na(tmpData$sulfate)
        sum_na <- sum(my_na)
        sum_na
    }
    cnames <- c('id','nobs')
    result_na <- sapply(id, function(x) tmpFun_na(directory,x))
    result <- data.frame(id,result_na)
    colnames(result)<- cnames
    
    result
    
} 


corr2 <- function(directory , threshold = 0){
    
    file_length<-length(list.files(directory))
    na_dataframe <- complete(directory,1:file_length)
    condition_for_threshold <- (na_dataframe$nobs>=threshold)
    
    tmpFun_calcCor <- function(directory,idNum){
        tmpDir <- paste(directory,'/',sprintf("%03d",idNum),'.csv',sep="")
        tmpData <- read.csv(tmpDir)
        correl <- cor(tmpData$sulfate,tmpData$nitrate,use = "pairwise.complete.obs")
        correl
    }
    
    
    result <- sapply(1:file_length, function(x) tmpFun_calcCor(directory,x))
    result <- result * condition_for_threshold
    result
    
}

corr <- function(directory, threshold = 0){
    file_length<-length(list.files(directory))
    na_dataframe <- complete(directory,1:file_length)
    condition_for_threshold <- (na_dataframe$nobs>=threshold)
    
    tmpFun_calcCor <- function(directory,idNum){
        tmpDir <- paste(directory,'/',sprintf("%03d",idNum),'.csv',sep="")
        tmpData <- read.csv(tmpDir)
        correl <- cor(tmpData$sulfate,tmpData$nitrate,use = "pairwise.complete.obs")
        correl
    }
    
    vec <- c()
    ctr <-1
    for(num in 1:file_length){
        if(condition_for_threshold[num]){
            tmpVal <- tmpFun_calcCor(directory,num)
            vec[ctr] <- tmpVal
            ctr <- ctr + 1
        }
    }
    vec
    
}


