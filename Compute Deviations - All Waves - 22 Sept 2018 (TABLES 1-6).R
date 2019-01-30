#Note that this is a slightly cleaned and enhanced version of the syntax used in the article
#I've tried to shorten and simplify the code, and ensure that all syntax files run from the same datafile
#Original syntax is available on request

##=======Initial Prep Work=========

#Clear environment
rm(list = ls())

library("survey")
library("foreign")
library("rstudioapi")

#Set working directory to file locations
setwd(dirname(getActiveDocumentContext()$path))

#Load files
#"tdsurvey" contains data on the 313 survey responses we received
#"alltds" contains data on all 491 members of the Dail across the three sessions
tdsurvey <- read.dta("TD Survey 2007-16 fake data.dta")
alltds <- read.dta("All TDs fake data.dta")

#Ensure that party variables are set appropriately
alltds$PartySimple <- factor(alltds$PartySimple, levels = c("FF","FG","Lab", "SF","Gr","Left","Ind","PD"))
tdsurvey$PartySimple <- factor(tdsurvey$PartySimple, levels = c("FF","FG","Lab", "SF","Gr","Left","Ind","PD"))

#Define survey design object
alltds.svy <- svydesign(ids = ~Name_of_TDs, fpc = ~fpc, data = alltds)

#Define questions used for most of the analysis
questionlist <- c("Q22", "Q21", "Q26", "Q24", "Q23", "Q25")

##=======Define function for creating tables with variance=========

#the "vartable" function produces a table of the deviations, including variances

#Function input: indvars, a character vector naming independent variables to produce the table for
#design: an svydesign object, containing the variables named above
#depvar: an optional dependent variable, IE a variable to crosstabulate the table by
#total: an optional dummy, specifying whether a "total" column should be produced in addition to any dependent variables

#Function output: a table generated via svymean or svyby, with formatting modifications

vartable <- function(indvars, depvar, design, totals = FALSE){
    #Vartable has several different behaviours, depending on whether:
    #A) there is a dependent variable or not (IE, whether we are computing a statistic for the total sample, or disaggregated to several subsamples)
    #B) if there is a dependent variable, whether we also want the total
    #I've structured this in the way that should run ever-so-slightly faster, rather than the way that produces shortest and most readable code
    
    ##--Step 1 - generate the core data for the table--
    #If there is a dependent variable
    if(!missing(depvar)){
        #Convert dependent variable from character to formula
        if(is.character(depvar)) depvar <- make.formula(depvar)
        
        #If we want to generate a table row for sample totals as well as subsamples
        if(totals == TRUE){
            table_wtotals <- function(x){
                rows.by <- svyby(make.formula(x), depvar, svymean, design = design, na.rm = TRUE)[,-1]
                rows.total <- svymean(make.formula(x), design = design, na.rm = TRUE)
                rows.all <- rbind(c(rows.total, SE(rows.total)), rows.by)
                rownames(rows.all)[1] <- "Total"
                return(rows.all)
            }
            table.list <- lapply(indvars, function(x) table_wtotals(x))
        
        #If we want to generate rows only for subsamples, not for sample totals
        }else{
            table.list <- lapply(indvars, function(x) svyby(make.formula(x), depvar, svymean, design = design, na.rm = TRUE)[,-1])
        }
        
    #If there is no dependent variable
    }else{
        table.list <- lapply(indvars, function(x) svymean(make.formula(x), design = design, na.rm = TRUE))
    }
    
    ##--Step 2 - reformat the table and transform before returning
    table.df <- data.frame(table.list)
    table.t <- t(table.df)
    
    table <- sqrt(table.t)
    return(table)
}


##=======Table 1 - TDs' mean position by party (all years)=========

#--Original Table--

#Grab data needed for table 1 - question A (self-placement)
t1.vars <- paste0(questionlist, "A")

#Calculate data by party
table1 <- t(sapply(alltds[t1.vars], function(x) tapply(x, alltds$PartySimple, mean, na.rm=TRUE)))


#Calculate data for "total" column and merge 
table1.append <- sapply(alltds[t1.vars], function(x) mean(x, na.rm = TRUE))
table1 <- cbind(table1.append, table1)
colnames(table1)[1] <- "Total"

#Write main table
write.csv(table1, "Table 1 - TD means all years.csv")

#--Table with variance estimates--
#Note that running tables through the svy package (with fpc and clustering) produces slightly different scores
#Alternative way to run the tables above, which includes standard errors
table1.alt.form <- as.formula(paste0("~",paste(t1.vars, collapse = " + ")))
table1.alt <- t(svyby(table1.alt.form, ~PartySimple, design = alltds.svy, svymean, na.rm=TRUE)[,-1])
table1.alt.append <- svymean(table1.alt.form, design = alltds.svy, na.rm = TRUE)
table1.alt <- cbind(c(table1.alt.append, SE(table1.alt.append)), table1.alt)
colnames(table1.alt)[1] <- "Total"

write.csv(table1.alt, "Table 1 (Alt Calc) - TD means all years.csv")

##========Table 2 (TD-parliamentary center deviation across years)=========
#Table computed on data from all years

##--Original table---

#Function to calculate how each observation of a scale variable differs from the mean observation in that group
#(where scale variable are policy questions, and the groups are year)
#Input: two columns of our TD dataset (assuming the same length
#"policyvar" contains the scale data;  "yearvar" can be categorical, and is used for grouping
#Output: a column of data, with the same length as the input variables, shwoing how each observation differs from the group's mean

yearmean_dist <- function(policyvar, yearvar){
    #Calculate the mean policy position for each of the three years
    yearmeans <- tapply(policyvar, yearvar, mean, na.rm=TRUE)
    
    #For each respondent in the dataset, calculate difference from the year-specific mean
    meandiff <- mapply(function(position, year) position - yearmeans[year], policyvar, yearvar)
}

#Compute individual-level difference between TD and year-specific average
t2.vars.1 <- paste0(questionlist, "_meandiff")
yearmeans <- sapply(alltds[paste0(questionlist, "A")], function(x) yearmean_dist(x, alltds$year))
colnames(yearmeans) <- t2.vars.1
alltds <- cbind(alltds, yearmeans)

#Compute deviation from mean differences
table2 <- t(sapply(alltds[t2.vars.1], function(y) tapply(y, alltds$PartySimple, function(x) sqrt(sum(x ^ 2, na.rm = TRUE)/ sum(!is.na(x))))))
table2 <- rbind(colMeans(table2, na.rm=TRUE), table2)
rownames(table2)[1] <- "Average"

#Get "total" column
table2.append <- sapply(alltds[t2.vars.1], function(x) sqrt(sum(x ^ 2, na.rm = TRUE) / sum(!is.na(x))))
table2.append <- c(mean(table2.append), table2.append)
names(table2.append)[1] <- "Avg"

table2 <- cbind(table2.append, table2)
colnames(table2)[1] <- "Total"

#Write to table
write.csv(table2, "Table 2 - TD-Party Deviation all years.csv")

##--Significance tests---

#We want to test significance on the *squared* difference between a TD and the center of parliament
#So start by calculating this
t2.vars.2 <- paste0(questionlist, "_meandiff_sq")
meandiff_sq <- sapply(alltds[t2.vars.1], function(x) x ^ 2)
colnames(meandiff_sq) <- t2.vars.2
alltds <- cbind(alltds, meandiff_sq)
alltds$avg_meandiff_sq <- apply(alltds[,t2.vars.2], 1, mean, na.rm = TRUE)

#Update survey design object with new variables
alltds.svy <- svydesign(ids = ~Name_of_TDs, fpc = ~fpc, data = alltds)

#Bivariate tests
svyttest(avg_meandiff_sq ~ PartySimple, design = subset(alltds.svy, PartySimple == "FF" | PartySimple == "SF"))
svyttest(avg_meandiff_sq ~ PartySimple, design = subset(alltds.svy, PartySimple == "FF" | PartySimple == "Left"))
svyttest(avg_meandiff_sq ~ PartySimple, design = subset(alltds.svy, PartySimple == "FG" | PartySimple == "SF"))
svyttest(avg_meandiff_sq ~ PartySimple, design = subset(alltds.svy, PartySimple == "FG" | PartySimple == "Left"))

#Multivariate tests
base_ff <- svyglm(avg_meandiff_sq ~ PartySimple + ipc, design = alltds.svy)
summary(base_ff)$coefficients
base_fg <- svyglm(avg_meandiff_sq ~ relevel(PartySimple, ref = "FG") + ipc, design = alltds.svy)
summary(base_fg)$coefficients

#--Table with variance estimates--

#Alternative way to run the tables above, which includes standard errors

t2.vars.3 <- c("avg_meandiff_sq", t2.vars.2)
table2.var <- vartable(indvars = t2.vars.3, depvar = ~PartySimple, design = alltds.svy, totals = TRUE)
write.csv(table2.var, "Table 2 Variances.csv")

##========Table 3 (TD-constituency deviation 2016)=========
#Table computed from 2016 survey only

##--Original table---

#Compute differences
t3.vars.1 <- paste0(questionlist, "_addiff")
tdsurvey[t3.vars.1] <- mapply(function(x, y) x - y,
                                                    tdsurvey[paste0(questionlist, "A")], tdsurvey[paste0(questionlist, "D")])

#Compute deviation from differences
table3 <- t(sapply(tdsurvey[t3.vars.1], function(y) tapply(y, tdsurvey$PartySimple, function(x) sqrt(sum(x ^ 2, na.rm = TRUE)/ sum(!is.na(x))))))

#Compute average deviation
table3 <- rbind(colMeans(table3, na.rm=TRUE), table3)
rownames(table3)[1] <- "Avg"

#Get "total" column
table3.append <- sapply(tdsurvey[t3.vars.1], function(x) sqrt(sum(x ^ 2, na.rm = TRUE) / sum(!is.na(x))))
table3.append <- c(mean(table3.append), table3.append)
names(table3.append)[1] <- "Avg"
table3 <- cbind(table3.append, table3)
colnames(table3)[1] <- "Total"

#Write to table
write.csv(table3, "Table 3 - TD-Constituent Deviation.csv")

##--Significance tests---

#As above, want to test significance on the *squared* difference between a TD and the center of parliament
#So start by calculating this
t3.vars.2 <- paste0(questionlist, "_addiff_sq")
addiff_sq <- sapply(tdsurvey[t3.vars.1], function(x) x ^ 2)
colnames(addiff_sq) <-t3.vars.2
tdsurvey <- cbind(tdsurvey, addiff_sq)

#Get average of differences - for each case, average non-missing values
tdsurvey$avg_addiff_sq <- apply(tdsurvey[,t3.vars.2], 1, mean, na.rm = TRUE)
 
#Update survey design object
tdsurvey.svy <- svydesign(ids = ~Name, fpc = ~fpc, data = tdsurvey)

#Bivariate tests
svyttest(avg_addiff_sq ~ PartySimple, subset(tdsurvey.svy, PartySimple == "FF" | PartySimple == "SF"))
svyttest(avg_addiff_sq ~ PartySimple, subset(tdsurvey.svy, PartySimple == "FF" | PartySimple == "Left"))
svyttest(avg_addiff_sq ~ PartySimple, subset(tdsurvey.svy, PartySimple == "FG" | PartySimple == "SF"))
svyttest(avg_addiff_sq ~ PartySimple, subset(tdsurvey.svy, PartySimple == "FG" | PartySimple == "Left"))

#Multivariate
base_ff <- svyglm(avg_addiff_sq ~ PartySimple + ipc, design = tdsurvey.svy)
summary(base_ff)$coefficients
base_fg <- svyglm(avg_addiff_sq ~ relevel(PartySimple, ref = "FG") + ipc, design = tdsurvey.svy)
summary(base_fg)$coefficients

#--Table with variance estimates--

#Alternative way to run the tables above, which includes standard errors

t3.vars.3 <- c("avg_addiff_sq", t3.vars.2)
table3.var <- vartable(indvars = t3.vars.3, depvar = ~PartySimple, design = tdsurvey.svy, totals =  TRUE)
write.csv(table3.var, "Table 3 Variances.csv")

##========Table 4 (TD-party disagreement)=========

#Identify instances of a >= 2pt disagreement
#Omit independents, beacuse we can't really calculate the distance between an independent and his/her party
self <- alltds[alltds$PartySimple != "Ind", paste0(questionlist, "A")]
party <- alltds[alltds$PartySimple != "Ind", paste0(questionlist, "B")]
bigdiff <- abs(self - party) >= 2

#Compute disagreement rate, by party
table4 <- t(apply(bigdiff, 2, function(y)  tapply(y, alltds$PartySimple[alltds$PartySimple != "Ind"], function(x) sum(x, na.rm=TRUE) / sum(!is.na(x)))))
table4.supprow <- tapply(rowSums(bigdiff, na.rm=TRUE), alltds$PartySimple[alltds$PartySimple != "Ind"], function(x) sum(x, na.rm=TRUE)) / 
    tapply(rowSums(!is.na(bigdiff)), alltds$PartySimple[alltds$PartySimple != "Ind"], function(x) sum(x, na.rm=TRUE))
table4 <- rbind(table4.supprow, table4)
rownames(table4)[1] <- "Avg"

#Compute total disagreement rate
table4.append <- apply(bigdiff, 2, function(x) sum(x, na.rm = TRUE) / sum(!is.na(x)))
table4.append <- c(sum(bigdiff, na.rm = TRUE) / sum(!is.na(bigdiff)), table4.append)
names(table4.append)[1] <- "Avg"
table4 <- cbind(table4.append, table4)
colnames(table4)[1] <- "Total"
table4 <- table4[,1:7]

write.csv(table4, "Table 4 - TD-Party Disagreement.csv")

##========Table 5 (TD-party deviation)=========
#All waves

#Compute differences
#For this question, we want to exclude independents - so we will subset the data to do this
t5.vars.1 <- paste0(questionlist, "_abdiff")
alltds[t5.vars.1] <- mapply(function(x, y) x - y,
                                                    alltds[paste0(questionlist, "A")], alltds[paste0(questionlist, "B")])
t5.subset <- alltds[alltds$PartySimple != "Ind", c(t5.vars.1, "PartySimple")]

#Compute deviation from differences
#myvars <- alltds[alltds$PartySimple != "Ind", t5.vars.1]
table5 <- t(
    sapply(t5.subset[t5.vars.1], function(y) tapply(y, t5.subset$PartySimple, function(x) sqrt(sum(x ^ 2, na.rm = TRUE)/ sum(!is.na(x)))))
)

#Compute average deviation
table5 <- rbind(colMeans(table5, na.rm=TRUE), table5)
rownames(table5)[1] <- "Avg"

#Get "total" column
table5.append <- sapply(t5.subset[t5.vars.1], function(x) sqrt(sum(x ^ 2, na.rm = TRUE) / sum(!is.na(x))))
table5.append <- c(mean(table5.append), table5.append)
names(table5.append)[1] <- "Avg"
table5 <- cbind(table5.append, table5)
colnames(table5)[1] <- "Total"

#Write to table
table5 <- table5[,c(1:7)]
write.csv(table5, "Table 5 - TD-Party Self-Placement Deviation.csv")

##--Significance Tests

#Compute deviations from differences
t5.vars.2 <- paste0(questionlist, "_abdiff_sq")
abdiff_sq <- sapply(alltds[t5.vars.1], function(x) x ^ 2)
colnames(abdiff_sq) <- t5.vars.2
alltds <- cbind(alltds, abdiff_sq)

#Get average of differences - for each case, average non-missing values
alltds$avg_abdiff_sq <- apply(alltds[t5.vars.2], 1, mean, na.rm = TRUE)

#Update survey design object
alltds.svy <- svydesign(ids = ~Name_of_TDs, fpc = ~fpc, data = alltds)

#Bivariate tests
#Because the left isn't really a "party" we don't test FF-left or FG-left differences
svyttest(avg_abdiff_sq ~ PartySimple, design = subset(alltds.svy, PartySimple == "FF" | PartySimple == "SF"))
svyttest(avg_abdiff_sq ~ PartySimple, design = subset(alltds.svy, PartySimple == "FG" | PartySimple == "SF"))

#Multivariate tests
#Effect of intra-party competition
base_ff <- svyglm(avg_abdiff_sq ~ PartySimple + ipc, design = alltds.svy)
summary(base_ff)$coefficients

#--Table with variance estimates--

t5.vars.3 <- c("avg_abdiff_sq", t5.vars.2)
table5.var <- vartable(indvars = t5.vars.3, depvar = ~PartySimple, design = alltds.svy, totals =  TRUE)
write.csv(table5.var, "Table 5 Variances.csv")

##========Table 6 (TD-constituency difference)=========
#Table computed from 2016 data only

##--Original table---

t6.vars.1 <- paste0(questionlist, "_addiff")
tdsurvey$avg_addiff <- apply(tdsurvey[t6.vars.1], 1, mean, na.rm = TRUE)
t6.vars.2 <- c("avg_addiff", t6.vars.1)
table6 <- t(sapply(tdsurvey[t6.vars.2], function(y) tapply(y, tdsurvey$PartySimple, function(x) mean(x, na.rm = TRUE))))
rownames(table6)[1] <- "Avg"

#Get "total" column
table6.append <- sapply(tdsurvey[t6.vars.2], function(x) mean(x))
table6 <- cbind(table6.append, table6)
colnames(table6)[1] <- "Total"

#Write to table
write.csv(table6, "Table 6 - TD-Constituency Difference.csv")

##--Significance tests---

#Update survey design object
tdsurvey.svy <- svydesign(ids = ~Name, fpc = ~fpc, data = tdsurvey)

#Bivariate tests
svyttest(avg_addiff ~ PartySimple, subset(tdsurvey.svy, PartySimple == "FF" | PartySimple == "SF"))
svyttest(avg_addiff ~ PartySimple, subset(tdsurvey.svy, PartySimple == "FF" | PartySimple == "Left"))
svyttest(avg_addiff ~ PartySimple, subset(tdsurvey.svy, PartySimple == "FG" | PartySimple == "SF"))
svyttest(avg_addiff ~ PartySimple, subset(tdsurvey.svy, PartySimple == "FG" | PartySimple == "Left"))

#Effect of intra-party competition
#Need to add ipc variable to simulated data
base_ff <- svyglm(avg_addiff ~ PartySimple + ipc, design = tdsurvey.svy)
summary(base_ff)$coefficients

#--Table with variance estimates--

#Alternative way to run the tables above, which includes standard errors
table6.alt.form <- as.formula(paste0("~", paste(t6.vars.2, collapse = " + ")))
table6.alt <- t(svyby(table6.alt.form, ~PartySimple, design = tdsurvey.svy, svymean, na.rm=TRUE)[,-1])
table6.alt.append <- svymean(table6.alt.form, design = tdsurvey.svy, na.rm = TRUE)
table6.alt <- cbind(c(table6.alt.append, SE(table6.alt.append)), table6.alt)
colnames(table6.alt)[1] <- "Total"

write.csv(table6.alt, "Table 6 (Alt Calc) - TD-Constituency Differences")
