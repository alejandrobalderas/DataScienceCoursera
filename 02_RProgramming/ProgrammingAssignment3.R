

outcome <- read.csv('outcome-of-care-measures.csv')
class(outcome[,11])
outcome[,11] <- as.numeric(outcome[,11])
class(outcome[,11])

# Build Histogram
hist(outcome[,11])



best <- function(state,outcome){
    options(warn = -1) 
    df <- read.csv('outcome-of-care-measures.csv',colClasses = 'character')
    columnNum <- if(outcome == 'heart attack'){
        11
    }else if(outcome == 'heart failure'){
        17
    }else if(outcome == 'pneumonia'){
        23
    }else{
        stop('invalid outcome')
    }

    df[,columnNum]<- as.numeric(df[,columnNum])
    stateDF <- if(state %in% df$State){
        df[which(df$State == state),]
    }else{
        stop('invalid state')
    }
    
    pointer <- which(stateDF[,columnNum] == min(stateDF[,columnNum],na.rm = TRUE))
    name <- as.character(stateDF[pointer,2]) 
    name
    
}



rankhospital <- function(state,outcome,num = 'best'){
    options(warn = -1) 
    df <- read.csv('outcome-of-care-measures.csv',colClasses = 'character')
    columnNum <- if(outcome == 'heart attack'){
        11
    }else if(outcome == 'heart failure'){
        17
    }else if(outcome == 'pneumonia'){
        23
    }else{
        stop('invalid outcome')
    }
    
    df[,columnNum]<- as.numeric(df[,columnNum])
    stateDF <- if(state %in% df$State){
        df[which(df$State == state),]
    }else{
        stop('invalid state')
    }
    
    if(is.character(num)){
        if(num == 'best'){
            num <- 1
        }
        else if(num == 'worst'){
            num <- nrow(stateDF)-sum(is.na(stateDF[[columnNum]]))
        }
    }
    
    sortedVal <- sort(stateDF[,columnNum])
    pointer <- which(sortedVal[num]== stateDF[,columnNum])
    if(identical(pointer, integer(0))){
        result <- NA
    }else if(length(pointer) < 2){   
        result <- stateDF[pointer,2]
    }else {
        sumTmp <- sum(sortedVal < sortedVal[num])
        result <- sort(stateDF[pointer,2])
        result <- result[num-sumTmp]
    }
    result
    
}


rankall <- function(outcome , num = "best"){
    options(warn = -1) 
    df <- read.csv('outcome-of-care-measures.csv',colClasses = 'character')
    columnNum <- if(outcome == 'heart attack'){
        11
    }else if(outcome == 'heart failure'){
        17
    }else if(outcome == 'pneumonia'){
        23
    }else{
        stop('invalid outcome')
    }

    
    df[,columnNum]<- as.numeric(df[,columnNum])
    df[,7] <- as.factor(df[,7])
    stateLevels <- levels(df$State)
    
    tmpFun <- function(df,state,columnNum,num){
        stateDF <- df[which(df$State == state),]
        sortedVal <- sort(stateDF[,columnNum])
        if(is.character(num)){
            if(num == 'best'){
                numVal <- 1
            }
            else if(num == 'worst'){
                numVal <- length(sortedVal)
            }
        }else{
            numVal <- num
        }

        pointer <- which(sortedVal[numVal]== stateDF[,columnNum])
        if(identical(pointer, integer(0))){
            result <- NA
        }else if(length(pointer) < 2){   
            result <- stateDF[pointer,2]
        }else {
            sumTmp <- sum(sortedVal < sortedVal[numVal])
            result <- sort(stateDF[pointer,2])
            result <- result[numVal-sumTmp]
        }
        result
    }
    
    tmpResult <- sapply(stateLevels,function(x) tmpFun(df,x,columnNum,num))
    
    result <- data.frame(tmpResult,stateLevels)
    colnames(result) <- c('Hospital', 'State')
    result
    
    

}

