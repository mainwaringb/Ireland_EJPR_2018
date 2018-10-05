rm(list = ls())
library(foreign)
library(BBmisc) #Package to convert data frame rows to list(convertRowstoList)
library(survey)
library(rstudioapi)

setwd(dirname(getActiveDocumentContext()$path))

##====================A: Reshape data to wide format=================
#Read in data
tdsurvey <- read.dta("TD Survey 2007-16 fake data.dta")
tdsurvey$Party <- factor(tdsurvey$Party, levels = c("FF","FG","Lab", "SF","Gr","Left","Ind","PD"))


#index of variables to include and exclude from analysis
long.timevar <- "Wave"
long.idvar <- "Name"
long.varying <- c("Party", "Constituency_coded", "Age",
                  "Q21A", "Q21B", "Q22A", "Q22B", "Q23A", "Q23B",
                  "Q24A", "Q24B", "Q25A", "Q25B", "Q26A", "Q26B", "Q27A", "Q27B")
                 
#Set the "Waves" variable to be a factor - in other words, to take ordered values from 1 to n
#This is required to correctly record the dyads that need to be computed
tdsurvey$Wave <- as.factor(tdsurvey$Wave)

#Reshape data, excluding unneeded variables
tdsurvey.wide <- reshape(data = tdsurvey, 
                         timevar = long.timevar, idvar = long.idvar,
                         v.names = long.varying, drop = names(tdsurvey[,(names(tdsurvey) %in% c(long.varying, long.idvar, long.timevar)) == FALSE]),
                         direction = "wide")

nwaves <- length(levels(tdsurvey$Wave))
wide.nvar <- length(tdsurvey.wide)
wide.nfixed <- wide.nvar - (nwaves * length(long.varying))

#The reshape command doesn't order columns appropriately
#2016 is listed first, followed by 2007, followed by 2011
#The below is a quick and dirty fix
tdsurvey.wide <- tdsurvey.wide[,c("Name", sort(names(tdsurvey.wide[,(wide.nfixed+1):wide.nvar])))]

#Create an index to remember which variables in the long-form dataset are from which waves
#This assumes that, as above, variables are sorted so that we first have time-fixed variables, then time-varying variables
wide.varying <- vector(mode = "list", length = nwaves)
names(wide.varying) <- sort(levels(tdsurvey$Wave))
for(i in 1:nwaves){
    wide.varying[[i]] <- seq(from = wide.nfixed + i, by = nwaves, to = wide.nvar)
}
wide.fixed <- 1:wide.nfixed


##=========B: Create a list of which waves each respondent completed=========

#function "listwaves" takes in a long-format dataset, where each respondent X wave is a separate row
#It outputs a wide-format variable, containing a list of which waves each respondent completed

#INPUT: "longids" - a variable with respondent IDs from the long-formatted dataset
#"longwaves" - a variable indicating which wave each observation comes from, from the long-formatted dataset
#OUTPUT: a list with n items, one for each respondent. Each item in the list is a vector, indicating waves completed
listwaves <- function(longids, longwaves){
    #Get unqique respondent IDs
    uniqueids <- unique(longids)
    
    #For each unique respondent ID, get the corresponding waves
    widewaves <- lapply(uniqueids, function(x) {
        sort(longwaves[longids == x]) 
    })
    
    return(widewaves)
}

tdsurvey.wide.wavescomplete <- listwaves(longids = tdsurvey$Name, longwaves = tdsurvey$Wave)

##=========C: Define a function to determine which dyads to compute=========

#Function "adjacentdyads" creates a list of adjacent dyads for each survey respondent
#INPUT: "records" is a list of which waves have been completed by each survey respondent
#"outformat" determines whether the output type should be a matrix or data frame
#Output: a matrix indicating which dyad should be computed, or a data frame containing dyads

adjacentDyads <- function(records, outformat = "matrix"){
    #Check for a valid value of outformat
    outformat <- tolower(outformat)
    if(outformat != "matrix" & outformat != "data.frame") stop ("Invalid output format")
    
    #Look at the first record, and use it to determine the names of waves
    #If records aren't in factor format, this fails and returns an error
    if(is.list(records) == FALSE) stop("Invalid format for records - must be in list format")
    waves <- levels(records[[1]])
    nwaves <- length(waves)
    
    #Function to produce a matrix-formatted output
    #INPUT: "record" a list, in factor format, of which survey waves a given TD completed
    #OUTPUT: a matrix, "dyad" indicating which dyads should be computed for the given TD
    #Each row and column in the matrix indicates a different wave
    if(outformat == "matrix") adjacentdyad <- function(record){
        #Compute number of waves and observations
        nobs <- length(record)
        
        #Initialize matrix to store which dyads to compute
        dyadmatrix <- matrix(data = FALSE, nrow = nwaves, ncol = nwaves)
        rownames(dyadmatrix) <- waves
        colnames(dyadmatrix) <- waves
        
        #If there is only one record (or NA data)for a given case, return an NA matrix of specified size
        if(length(record) <= 1 | is.na(length(record))) return(dyadmatrix)
        
        #Loop through record. For each record, write a dyad for that record and the next one
        #Note that the waves MUST be recored as factors for this to work
        for(j in 1:(nobs - 1)){
            a <- as.numeric(record[j])
            b <- as.numeric(record[j+1])
            
            dyadmatrix[a,b] <- TRUE
        }
        
        #Return dyadmatrix
        return(dyadmatrix)
    }
    
    #Function to produce a data-frame formatted output
    #INPUT: "record" a list, in factor format, of which survey waves a given TD completed
    #OUTPUT: a data frame, "dyad" indicating which dyads should be computed for the given TD
    #Each row in the data frame indicates a pair of years that should be computed
    
    if(outformat == "data.frame") adjacentdyad <- function(record){
        #Compute number of waves and observations
        nobs <- length(record)
        
        #Initialize data frame to store
        dyadframe <- data.frame(A = factor(x = numeric(0), levels = c(2007, 2011, 2016)), B = factor(x = numeric(0), levels = c(2007, 2011, 2016)))
        
        #If there is only one record (or NA data)for a given case, return an NA
        if(length(record) <= 1 | is.na(length(record))) return(dyadframe)

        #Loop through record. For each record, write a dyad for that record and the next one
        #Note that the waves MUST be recored as factors for this to work
        for(j in 1:(nobs - 1)){
            dyadframe[j,1] <- record[j]
            dyadframe[j,2] <- record[j+1]
        }
        
        #return dyadframe
        return(dyadframe)
    }
        
    #Compute dyads for every respondent in the data frame
    dyads <- lapply(records, function(x) adjacentdyad (record = x))
}

##=========D: Calculate which dyads to compute, for all TDs=========

tdsurvey.wide.dyads <- adjacentDyads(records = tdsurvey.wide.wavescomplete, outformat = "matrix")
tdsurvey.wide.dyadlist <- adjacentDyads(records = tdsurvey.wide.wavescomplete, outformat = "data.frame")

##=========E: Reshape data so each dyad is a row=========

#Function "shapeDyadFromWide" to take one row of wide-format data, and split it into one row for each dyad
#INPUTS:
#casedata - a data frame or list containing the dataset in wide format
#dyadlists - a list of dyads to be computed for each respondent
#wavevars - a list of vector, where each item gives the location of variables used in a wave
#fixedvars - a vector index giving the locations of variables that are fixed across waves
#wavevarnames - names of variables that change across waves, in the order they are listed in casedata
#fixedvarnames - names of variables that are fixed across waves, in the order they are listed in casedata
#OUTPUTS: one row of data for each dyad in the dataset

#I could extend this function substantially with a "format" parameter that lets met compute dyads from long-format data

shapeDyadsFromWide <- function(dataset.wide, dyadlists, wavevars, fixedvars, wavevarnames, fixedvarnames){
    #Check for invalid dyad lists format
    if(sum(sapply(dyadlists, function(x) is.data.frame(x) == FALSE)) > 0) stop("Invalid dyad lists - not all entries are in data frame format")
    if(sum(sapply(dyadlists, ncol) != 2) > 0) stop("Invalid dyad list - not all entries have correct number of data columns")

    #Check whether all the variables indexed in fixedvars and wavevars exists
    #For a list-formatted dataset.wide, check that ncols is the same for all entries
    if(is.data.frame(dataset.wide)){
        ncols <- ncol(dataset.wide) 
    }else if(is.list(dataset.wide)){
        ncols <- length(dataset.wide[[1]])
        if(sum(sapply(dataset.wide, function(x) length(x) != ncols)) > 0) stop("Not all rows of dataset.wide have equal number of cases")
    }
    if(sum(c(max(fixedvars), sapply(wavevars, max)) > ncols) > 0) stop ("Not all variables indexed in wavevars and fixedvars can be found in dataset.wide")
    
    #Convert TD survey data frame to one list object for each respondent, needed to use mapply
    if(is.data.frame(dataset.wide)){
        require(BBmisc)
        dataset.wide <- convertRowsToList(dataset.wide, name.vector = TRUE, factors.as.char = FALSE)
    }

    #If wave variable names are not provided, strip the wave suffix from the names of the first wave variables
    #Get the wave suffix from the first instance of levels(dyadlists)
    #IF wave varaible names are provided, check that the vector is the right length
    if(missing(wavevarnames)){
        wavevarnames <- gsub(pattern = paste(".", levels(dyadlists[[1]]$A)[1], sep = ""),
                             replacement = "",
                             x = names(casedata[,wavevars[[1]]]))
    } else{
        if(sum(sapply(wavevars, length) != length(wavevarnames)) > 0) stop("Length of wavevarnames is not equal to length of wavevars")
    }
    
    #If fixed variable names are not provided, get from dataset.wide and index of fixed variables
    if(missing(fixedvarnames)){
        fixedvarnames <- names(dataset.wide[[1]][fixedvars])
    }else{
        if(length(fixedvarnames) != length (fixedvars)) stop ("Length of fixedvarnames is not equal to length of fixedvars")
    }
          
    varnames <- c(fixedvarnames, paste(wavevarnames, "WaveA", sep = "."), paste(wavevarnames, "WaveB", sep = "."), "Wave A", "Wave B", "dyad")
                                      
    #Function to convert one row of wide-format data into several rows of dyad data
    shapeDyadFromWide <- function(casedata, dyadlist){
        if(nrow(dyadlist) < 1){
            warning("Invalid dyad list - given case includes one or fewer waves")
            return(NULL)
        }
    
        #Function to compute a row of dyad data
        dyadRow <- function(dyad){
            #Get the index of variables for each of two points in time needed for the dyad
            varsA <- wavevars[[dyad[1]]]
            varsB <- wavevars[[dyad[2]]]
            
            #Subset the data to include variables from the two points in time, plus fixed variables
            datarow <- casedata[c(fixedvars, varsA, varsB)]
            #Add a variable specifying dyad name
            dyadname <- paste(dyad[1], dyad[2], sep = ",")
            
            #Convert to data frame
            datarow <- data.frame(datarow, dyad[1], dyad[2], dyadname, stringsAsFactors = FALSE)
            #Add variable names that will be consistent across dyads
            names(datarow) <- varnames
            
            return(datarow)
        }
        
        #Apply the function to create one of dyad-format data, over every dyad to be computed for a given case
        #Return the case reshaped in dyadic form
        reshapedcase <- do.call(rbind, apply(dyadlist, 1, function(x) dyadRow(dyad = x)))
        return(reshapedcase)
    }
    
    #Apply function to reshape a case, over all cases in the dataset
    dataset.dyadic <- do.call(rbind, mapply(
        function(x, y) shapeDyadFromWide(casedata = x, dyadlist = y),
        dataset.wide, dyadlists))
    
    return(dataset.dyadic)
}
    
#Specify needed variables for function call
wavevarnames <- sort(names(tdsurvey[,long.varying]))
fixedvarnames <- names(tdsurvey.wide)[wide.fixed]

#Call function
tdsurvey.dyadic <- shapeDyadsFromWide(dataset.wide = tdsurvey.wide, dyadlists = tdsurvey.wide.dyadlist, 
                                      wavevars = wide.varying, fixedvars = wide.fixed, wavevarnames = wavevarnames, fixedvarnames = fixedvarnames)

##=========F: Compute deltas=========
#Alernatively, I could have shapeDyadFromWide return a vector that says which variable indices are Wave A and Wave B

#Function "dyadDeltas" computes deltas for each row of a dyad-formatted dataset
#INPUT: dataset.dyadic - a dyad-formatted dataset
#waveA - an index indicating which variables are from Wave A
#waveB - an index indicating which variables are from Wave B
#OUTPUT: a dataframe of deltas
dyadDeltas <- function(dataset.dyadic, waveA, waveB, varnames)
{
    #Make sure indexed variables are valid
    if(max(c(waveA, waveB)) > ncol(dataset.dyadic)) stop("Not all variables indexed in waveA and waveB can be found in dataset.dyadic")
    if(length(waveA) != length(waveB)) stop("Number of Wave A variables is different than number of Wave B variables")
    
    #derive varnames from names of wave A variables
    if(missing(varnames))
    {
        varnames <- names(dataset.dyadic[,waveA])
        varnames <- gsub(pattern = ".WaveA", replacement = ".Delta", x = varnames)
    }
    
    #Coerce character variables in waveA and waveB to numeric
    #First create an index of which variables are character formatted AND part of Wave A + Wave B
    charvars <- which(sapply(dataset.dyadic, function(x) class(x) == "character")) 
    charvars <- charvars[charvars %in% c(waveAvars, waveBvars)]
    if(length(charvars) > 0){
        warning("Character variables coerced to numeric")
        dataset.dyadic[,charvars] <- apply(dataset.dyadic[,charvars], 2, as.numeric)
    }
    #compute
    deltas <- dataset.dyadic[,waveB] - dataset.dyadic[,waveA]
    names(deltas) <- varnames
    return(deltas)
}

#Define continuous/numerical Wave A and Wave B variables in TDsurvey, in order to calculate change
waveAvars <- which(grepl("WaveA", names(tdsurvey.dyadic)) & (sapply(tdsurvey.dyadic, class) == "numeric"))
waveBvars <- which(grepl("WaveB", names(tdsurvey.dyadic)) & (sapply(tdsurvey.dyadic, class) == "numeric"))

#Append deltas to data file with dyadic information
tdsurvey.dyadic <- cbind(tdsurvey.dyadic, dyadDeltas(dataset.dyadic = tdsurvey.dyadic, waveA = waveAvars, waveB = waveBvars))
    
##=========G: Write tables and conduct analysis=========

#--------Basic exploratory data---------

tdsurvey.dyadic$Party.WaveA == tdsurvey.dyadic$Party.WaveB

#Look at numbrer of repeat compeltes
tdsurvey.wide.nwavescomplete <- sapply(tdsurvey.wide.wavescomplete, length)
sum(tdsurvey.wide.nwavescomplete == 1)
sum(tdsurvey.wide.nwavescomplete == 2)

#Repeated compeltes by party
summary(tdsurvey.dyadic$Party.WaveA)
tdsurvey.dyadic$dyad <- as.factor(tdsurvey.dyadic$dyad)
summary(tdsurvey.dyadic$dyad)

#--------Compute summary statistics---------

#Compute total of squared shifts per TD
dyad.deltavars <- c("Q21A.Delta", "Q22A.Delta", "Q23A.Delta", "Q24A.Delta", "Q25A.Delta", "Q26A.Delta")
tdsurvey.dyadic$avgdev <- apply(tdsurvey.dyadic[, dyad.deltavars], 1, function(x) sum(x^2, na.rm = TRUE) / sum(!is.na(x)))

#Get average (absolute) shift by party
tdsurvey.dyadic$avg.delta <-  apply(tdsurvey.dyadic[, dyad.deltavars], 1, function(x) sum(abs(x), na.rm = TRUE) / sum(!is.na(x)))
mytable <- as.vector(by(tdsurvey.dyadic$avg.delta, tdsurvey.dyadic$Party.WaveA, function(x) mean(x, na.rm=TRUE)))
names(mytable) <- levels(tdsurvey.dyadic$Party.WaveA)

#Get (absolute) shift by issue
mytable <- colMeans(abs(tdsurvey.dyadic[,dyad.deltavars]), na.rm=TRUE)

#===========Table 7 (Individual-level deviation between waves)=========

#For now, I am using the proportion of TDs interviewed (313/490) as the FPC
#An alternate approach would be to compute how many incumbent TDs in each year dyad we didn't interview

tdsurvey.dyadic$fpc <- rep(313/490, nrow(tdsurvey.dyadic))
svy.dyadic <- svydesign(ids = ~Name, fpc = ~fpc, data = tdsurvey.dyadic)

table7 <- matrix(NA, nrow = 7, ncol = 8)
rownames(table7) <- c("Avg", "Q22", "Q21", "Q26", "Q24", "Q23", "Q25")
colnames(table7) <- levels(tdsurvey.dyadic$Party.WaveA)

myvars <- tdsurvey.dyadic[,c("Q22A.Delta", "Q21A.Delta", "Q26A.Delta", "Q24A.Delta", "Q23A.Delta", "Q25A.Delta")]
table7[2:7,] <- t(apply(myvars, 2, function(y) tapply(y, tdsurvey.dyadic$Party.WaveA, function(x) sqrt(mean(x ^ 2, na.rm = TRUE)))))
table7[1,] <- colMeans(table7[2:7,], na.rm=TRUE)

table7.append <- matrix(NA, nrow = 7, ncol = 1)
colnames(table7.append) <- "Total"
table7.append[2:7] <- apply(myvars, 2, function(x) sqrt(mean(x ^ 2, na.rm = TRUE)))
table7.append[1] <- mean(table7.append[2:7])
table7 <- cbind(table7.append, table7)

write.csv(table7, "Table 7 - Individual-level shifts.csv")

#--------Run sig tests on summary statistics---------

myvars <- tdsurvey.dyadic[,c("Q22A.Delta", "Q21A.Delta", "Q26A.Delta", "Q24A.Delta", "Q23A.Delta", "Q25A.Delta")]
tdsurvey.dyadic_sq <- apply(myvars, 2, function(x) x ^ 2)
colnames(tdsurvey.dyadic_sq) <- gsub(".Delta", ".Delta_sq", colnames(tdsurvey.dyadic_sq))
tdsurvey.dyadic <- cbind(tdsurvey.dyadic, tdsurvey.dyadic_sq)
tdsurvey.dyadic$avg.Delta_sq <- apply(tdsurvey.dyadic[,c("Q21A.Delta_sq", "Q22A.Delta_sq", "Q23A.Delta_sq", "Q24A.Delta_sq", "Q25A.Delta_sq", "Q26A.Delta_sq")], 1, function(x) mean(x,na.rm=TRUE))

#Using "svymean" with multiple variables in one formula doesn't work, because a case is omitted for ALL variables if it has a missign value for even one variable in the formula
#Instead, I need to call svymean multiple times, each for a single variable
#The lapply statement below does this
myvars <- c("avg.Delta_sq", "Q22A.Delta_sq", "Q21A.Delta_sq", "Q26A.Delta_sq", "Q24A.Delta_sq", "Q23A.Delta_sq", "Q25A.Delta_sq")
svy.dyadic <- svydesign(ids = ~Name, fpc = ~fpc, data = tdsurvey.dyadic)

vartable <- function(indvars, depvar, design){
    if(!missing(depvar)){
        if(is.character(depvar)) depvar <- make.formula(depvar)
        table.list <- lapply(indvars, function(x) svyby(make.formula(x), depvar, svymean, design = design, na.rm = TRUE)[,-1])
    }else{
        table.list <- lapply(indvars, function(x) svymean(make.formula(x), design = design, na.rm = TRUE))
    }

    table.df <- data.frame(table.list)
    table.t <- t(table.df)
    
    table <- sqrt(table.t)
    return(table)
}

table7.var <- vartable(indvars = myvars, depvar = ~Party.WaveA, design = svy.dyadic)
myrownames <- rownames(table7.var)
table7.var.append <- vartable(indvars = myvars, design = svy.dyadic)
table7.var <- cbind(table7.var.append, table7.var)
colnames(table7.var)[1] <- "Total"
rownames(table7.var) <- myrownames

write.csv(table7.var, "Table 7 Variances.csv")

test <- svyglm(avg.delta ~ Party.WaveA, design = svy.dyadic)
summary(test)

test <- svyglm(avg.delta ~ relevel(Party.WaveA, 2), design = svy.dyadic)
summary(test)

