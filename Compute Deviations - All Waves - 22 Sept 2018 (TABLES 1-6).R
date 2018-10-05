library("survey")
library("foreign")
library("rstudioapi")

#Note that this is a slightly cleaned and enhanced version of the syntax used in the article
#I've tried to shorten and simplify the code, and ensure that all syntax files run from the same datafile
#Original syntax is available on request

rm(list = ls())
setwd(dirname(getActiveDocumentContext()$path))

#"tdsurvey" contains data on the 313 survey responses we received
#"alltds" contains data on all 491 members of the Dail across the three sessions
tdsurvey <- read.dta("TD Survey 2007-16 fake data.dta")
alltds <- read.dta("All TDs fake data.dta")

alltds$PartySimple <- factor(alltds$PartySimple, levels = c("FF","FG","Lab", "SF","Gr","Left","Ind","PD"))
tdsurvey$PartySimple <- factor(tdsurvey$PartySimple, levels = c("FF","FG","Lab", "SF","Gr","Left","Ind","PD"))


##=======Define function for creating tables with variance=========

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


##=======Table 1 - TDs' mean position by party (all years)=========
#All years

myvars <- alltds[,c("Q22A", "Q21A", "Q26A", "Q24A", "Q23A", "Q25A")]
table1 <- t(apply(myvars, 2, function(x) tapply(x, alltds$PartySimple, mean, na.rm=TRUE)))


#--Means--
#Data for "total" column
table1.append <- apply(myvars, 2, function(x) mean(x, na.rm = TRUE))

table1 <- cbind(table1.append, table1)
colnames(table1)[1] <- "Total"

#Write main table
write.csv(table1, "Table 1 - TD means all years.csv")

#--Variance--
#Note that running tables through the svy package (with fpc and clustering) produces slightly different scores
alltds.svy <- svydesign(ids = ~Name_of_TDs, fpc = ~fpc, data = alltds)

#Alternative way to run the tables above, which includes standard errors
table1.alt <- t(svyby(~Q22A + Q21A + Q26A + Q24A + Q23A + Q25A , ~PartySimple, design = alltds.svy, svymean, na.rm=TRUE)[,-1])

table1.alt.append <- svymean(~Q22A + Q21A + Q26A + Q24A + Q23A + Q25A, design = alltds.svy, na.rm = TRUE)
table1.alt <- cbind(c(table1.alt.append, SE(table1.alt.append)), table1.alt)
colnames(table1.alt)[1] <- "Total"

write.csv(table1.alt, "Table 1 (Alt Calc) - TD means all years.csv")


##========Table 2 (TD-parliamentary center deviation across years)=========
#All years

#Create matrix to write to CSV
table2 <- matrix(NA, nrow = 7, ncol = 8)
rownames(table2) <- c("Avg", "Q22", "Q21", "Q26", "Q24", "Q23", "Q25")
colnames(table2) <- levels(alltds$PartySimple)

##---Means---

yearmean_dist <- function(policyvar, yearvar){
    #Calculate the mean polciy position for each of the three years
    yearmeans <- tapply(policyvar, yearvar, mean, na.rm=TRUE)
    
    #For each respondent in the dataset, calculate difference from the year-specific mean
    meandiff <- mapply(function(position, year) position - yearmeans[year], policyvar, yearvar)
}

#Compute individual-level difference between TD and year-specific average
myvars <- alltds[,c("Q22A", "Q21A", "Q26A", "Q24A", "Q23A", "Q25A")]
yearmeans <- apply(myvars, 2, function(x) yearmean_dist(x, alltds$year))
colnames(yearmeans) <- gsub("A", "_meandiff", colnames(yearmeans))
alltds <- cbind(alltds, yearmeans)

#Compute deviation from mean differences
myvars <- alltds[,c("Q22_meandiff", "Q21_meandiff", "Q26_meandiff", "Q24_meandiff", "Q23_meandiff", "Q25_meandiff")]
table2 <- t(apply(myvars, 2, function(y) tapply(y, alltds$PartySimple, function(x) sqrt(sum(x ^ 2, na.rm = TRUE)/ sum(!is.na(x))))))

#Compute average deviation
table2 <- rbind(colMeans(table2, na.rm=TRUE), table2)
rownames(table2)[1] <- "Average"

#Get "total" column
table2.append <- apply(myvars, 2, function(x) sqrt(sum(x ^ 2, na.rm = TRUE) / sum(!is.na(x))))
table2.append <- c(mean(table2.append), table2.append)
names(table2.append)[1] <- "Avg"

table2 <- cbind(table2.append, table2)
colnames(table2)[1] <- "Total"

#Write to table
write.csv(table2, "Table 2 - TD-Party Deviation all years.csv")

#Sig tests
#Compute differences
myvars <- alltds[,c("Q22_meandiff", "Q21_meandiff", "Q26_meandiff", "Q24_meandiff", "Q23_meandiff", "Q25_meandiff")]
meandiff_sq <- apply(myvars, 2, function(x) x ^ 2)
colnames(meandiff_sq) <- gsub("_meandiff", "_meandiff_sq", colnames(meandiff_sq))
alltds <- cbind(alltds, meandiff_sq)

#Get average of differences - for each case, average non-missing values
alltds$avg_meandiff_sq <- apply(alltds[,c("Q21_meandiff_sq", "Q22_meandiff_sq", "Q23_meandiff_sq", "Q24_meandiff_sq", "Q25_meandiff_sq", "Q26_meandiff_sq")], 1, mean, na.rm = TRUE)

##---Bivariate tests---
#Could probably function-ise this
#But since I'm interested in specific combinations of tests it doesn't seem worth it
#FF-SF
alltds.svy <- svydesign(ids = ~Name_of_TDs, fpc = ~fpc, data = alltds[(alltds$PartySimple == "FF" | alltds$PartySimple == "SF") & !is.na(alltds$avg_meandiff_sq),])
svyttest(avg_meandiff_sq ~ PartySimple, alltds.svy)
#FF-Left
alltds.svy <- svydesign(ids = ~Name_of_TDs, fpc = ~fpc, data = alltds[(alltds$PartySimple == "FF" | alltds$PartySimple == "Left") & !is.na(alltds$avg_meandiff_sq),])
svyttest(avg_meandiff_sq ~ PartySimple, alltds.svy)
#FG-SF
alltds.svy <- svydesign(ids = ~Name_of_TDs, fpc = ~fpc, data = alltds[(alltds$PartySimple == "FG" | alltds$PartySimple == "SF") & !is.na(alltds$avg_meandiff_sq),])
svyttest(avg_meandiff_sq ~ PartySimple, alltds.svy)
#FG-Leftist
alltds.svy <- svydesign(ids = ~Name_of_TDs, fpc = ~fpc, data = alltds[(alltds$PartySimple == "FG" | alltds$PartySimple == "Left") & !is.na(alltds$avg_meandiff_sq),])
svyttest(avg_meandiff_sq ~ PartySimple, alltds.svy)

##---Multivariate tests---
alltds.svy <- svydesign(ids = ~Name_of_TDs, fpc = ~fpc, data = alltds[!is.na(alltds$avg_meandiff_sq),])
base_ff <- svyglm(avg_meandiff_sq ~ PartySimple + ipc, design = alltds.svy)
summary(base_ff)$coefficients

base_ff <- svyglm(avg_meandiff_sq ~ PartySimple + ipc, design = alltds.svy)
summary(base_ff)$coefficients

base_fg <- svyglm(avg_meandiff_sq ~ relevel(PartySimple, ref = "FG") + ipc, design = alltds.svy)
summary(base_fg)$coefficients

##---Variance---
alltds.svy <- svydesign(ids = ~Name_of_TDs, fpc = ~fpc, data = alltds)

#Alternative way to run the tables above, which includes standard errors

myvars <- c("avg_meandiff_sq", "Q22_meandiff_sq", "Q21_meandiff_sq", "Q26_meandiff_sq", "Q24_meandiff_sq", "Q23_meandiff_sq", "Q25_meandiff_sq")
table2.var <- vartable(indvars = myvars, depvar = ~PartySimple, design = alltds.svy)
myrownames <- rownames(table2.var)
table2.var.append <- vartable(indvars = myvars, design = alltds.svy)
table2.var <- cbind(table2.var.append, table2.var)
colnames(table2.var)[1] <- "Total"
rownames(table2.var) <- myrownames

write.csv(table2.var, "Table 2 Variances.csv")


##========Table 3 (TD-constituency deviation 2016)=========
#2016 survey only

#Compute differences
#Could mapply over these questions, but probably not worth it
questionlist <- paste0("Q", 21:26)
tdsurvey[paste0(questionlist, "_addiff")] <- mapply(function(x, y) x - y,
                                                    tdsurvey[paste0(questionlist, "A")], tdsurvey[paste0(questionlist, "D")])

#Create matrix to write to CSV
#Compute deviation from differences
myvars <- tdsurvey[,c("Q22_addiff", "Q21_addiff", "Q26_addiff", "Q24_addiff", "Q23_addiff", "Q25_addiff")]
table3 <- t(apply(myvars, 2, function(y) tapply(y, tdsurvey$PartySimple, function(x) sqrt(sum(x ^ 2, na.rm = TRUE)/ sum(!is.na(x))))))

#Compute average deviation
table3 <- rbind(colMeans(table3, na.rm=TRUE), table3)
rownames(table3)[1] <- "Avg"

#Get "total" column
table3.append <- apply(myvars, 2, function(x) sqrt(sum(x ^ 2, na.rm = TRUE) / sum(!is.na(x))))
table3.append <- c(mean(table3.append), table3.append)
names(table3.append)[1] <- "Avg"
table3 <- cbind(table3.append, table3)
colnames(table3)[1] <- "Total"

#Write to table
write.csv(table3, "Table 3 - TD-Constituent Deviation.csv")

#Sig tests
#Compute differences
myvars <- tdsurvey[,c("Q22_addiff", "Q21_addiff", "Q26_addiff", "Q24_addiff", "Q23_addiff", "Q25_addiff")]
addiff_sq <- apply(myvars, 2, function(x) x ^ 2)
colnames(addiff_sq) <- gsub("_addiff", "_addiff_sq", colnames(addiff_sq))
tdsurvey <- cbind(tdsurvey, addiff_sq)

#Get average of differences - for each case, average non-missing values
tdsurvey$avg_addiff_sq <- apply(tdsurvey[,c("Q21_addiff_sq", "Q22_addiff_sq", "Q23_addiff_sq", "Q24_addiff_sq", "Q25_addiff_sq", "Q26_addiff_sq")], 1, mean, na.rm = TRUE)
 
##---Bivariate---
#FF-SF
tdsurvey.svy <- svydesign(ids = ~Name, fpc = ~fpc, data = tdsurvey[tdsurvey$PartySimple == "FF" | tdsurvey$PartySimple == "SF",])
svyttest(avg_addiff_sq ~ PartySimple, tdsurvey.svy)
#FF-Left
tdsurvey.svy <- svydesign(ids = ~Name, fpc = ~fpc, data = tdsurvey[tdsurvey$PartySimple == "FF" | tdsurvey$PartySimple == "Left",])
svyttest(avg_addiff_sq ~ PartySimple, tdsurvey.svy)
#FG-SF
tdsurvey.svy <- svydesign(ids = ~Name, fpc = ~fpc, data = tdsurvey[tdsurvey$PartySimple == "FG" | tdsurvey$PartySimple == "SF",])
svyttest(avg_addiff_sq ~ PartySimple, tdsurvey.svy)
#FG-Leftist
tdsurvey.svy <- svydesign(ids = ~Name, fpc = ~fpc, data = tdsurvey[tdsurvey$PartySimple == "FG" | tdsurvey$PartySimple == "Left",])
svyttest(avg_addiff_sq ~ PartySimple, tdsurvey.svy)

##---Multivariate---
tdsurvey.svy <- svydesign(ids = ~Name, fpc = ~fpc, data = tdsurvey)

base_ff <- svyglm(avg_addiff_sq ~ PartySimple + ipc, design = tdsurvey.svy)
summary(base_ff)$coefficients

base_fg <- svyglm(avg_addiff_sq ~ relevel(PartySimple, ref = "FG") + ipc, design = tdsurvey.svy)
summary(base_fg)$coefficients

##---Variance---
tdsurvey.svy <- svydesign(ids = ~Name, fpc = ~fpc, data = tdsurvey)

myvars <- c("avg_addiff_sq", "Q22_addiff_sq", "Q21_addiff_sq", "Q26_addiff_sq", "Q24_addiff_sq", "Q23_addiff_sq", "Q25_addiff_sq")
table3.var <- vartable(indvars = myvars, depvar = ~PartySimple, design = tdsurvey.svy)
myrownames <- rownames(table3.var)
table3.var.append <- vartable(indvars = myvars, design = tdsurvey.svy)
table3.var <- cbind(table3.var.append, table3.var)
colnames(table3.var)[1] <- "Total"
rownames(table3.var) <- myrownames

write.csv(table3.var, "Table 3 Variances.csv")

##========Table 4 (TD-party disagreement)=========

#Identify instances of a >= 2pt disagreement
#Omit independents, beacuse we can't really calculate the distance between an independent and his/her party
self <- alltds[alltds$PartySimple != "Ind" ,c("Q22A", "Q21A", "Q26A", "Q24A", "Q23A", "Q25A")]
party <- alltds[alltds$PartySimple != "Ind", c("Q22B", "Q21B", "Q26B", "Q24B", "Q23B", "Q25B")]
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
#For this question, we want to exclude independents
questionlist <- paste0("Q", 21:26)
alltds[paste0(questionlist, "_abdiff")] <- mapply(function(x, y) x - y,
                                                    alltds[paste0(questionlist, "A")], alltds[paste0(questionlist, "B")])


#Compute deviation from differences
myvars <- alltds[alltds$PartySimple != "Ind", c("Q22_abdiff", "Q21_abdiff", "Q26_abdiff", "Q24_abdiff", "Q23_abdiff", "Q25_abdiff")]
table5 <- t(apply(myvars, 2, function(y) tapply(y, alltds$PartySimple[alltds$PartySimple != "Ind"], function(x) sqrt(sum(x ^ 2, na.rm = TRUE)/ sum(!is.na(x))))))

#Compute average deviation
table5 <- rbind(colMeans(table5, na.rm=TRUE), table5)
rownames(table5)[1] <- "Avg"

#Get "total" column
table5.append <- apply(myvars, 2, function(x) sqrt(sum(x ^ 2, na.rm = TRUE) / sum(!is.na(x))))
table5.append <- c(mean(table5.append), table5.append)
names(table5.append)[1] <- "Avg"
table5 <- cbind(table5.append, table5)
colnames(table5)[1] <- "Total"

#Write to table
table5 <- table5[,c(1:7)]
write.csv(table5, "Table 5 - TD-Party Self-Placement Deviation.csv")

#Sig tests
#Compute deviations from differences
myvars <- alltds[,c("Q22_abdiff", "Q21_abdiff", "Q26_abdiff", "Q24_abdiff", "Q23_abdiff", "Q25_abdiff")]
abdiff_sq <- apply(myvars, 2, function(x) x ^ 2)
colnames(abdiff_sq) <- gsub("_abdiff", "_abdiff_sq", colnames(abdiff_sq))
alltds <- cbind(alltds, abdiff_sq)

#Get average of differences - for each case, average non-missing values
alltds$avg_abdiff_sq <- apply(alltds[,c("Q21_abdiff_sq", "Q22_abdiff_sq", "Q23_abdiff_sq", "Q24_abdiff_sq", "Q25_abdiff_sq", "Q26_abdiff_sq")], 1, mean, na.rm = TRUE)

##---Bivariate---
#FF-SF
alltds.svy <- svydesign(ids = ~Name_of_TDs, fpc = ~fpc, data = alltds[alltds$PartySimple == "FF" | alltds$PartySimple == "SF",])
svyttest(avg_abdiff_sq ~ PartySimple, alltds.svy)
#FG-SF
alltds.svy <- svydesign(ids = ~Name_of_TDs, fpc = ~fpc, data = alltds[alltds$PartySimple == "FG" | alltds$PartySimple == "SF",])
svyttest(avg_abdiff_sq ~ PartySimple, alltds.svy)
#Because the left isn't really a "party" we don't test FF-left or FG-left differences

#---Multivariate---
#Effect of intra-party competition
alltds.svy <- svydesign(ids = ~Name_of_TDs, fpc = ~fpc, data = alltds)
base_ff <- svyglm(avg_abdiff_sq ~ PartySimple + ipc, design = alltds.svy)
summary(base_ff)$coefficients

#IPC is not significant (t = -1.34, p = .18)

##---Variance---
alltds.svy <- svydesign(ids = ~Name_of_TDs, fpc = ~fpc, data = alltds)

myvars <- c("avg_abdiff_sq", "Q22_abdiff_sq", "Q21_abdiff_sq", "Q26_abdiff_sq", "Q24_abdiff_sq", "Q23_abdiff_sq", "Q25_abdiff_sq")
table5.var <- vartable(indvars = myvars, depvar = ~PartySimple, design = alltds.svy)
myrownames <- rownames(table5.var)
table5.var.append <- vartable(indvars = myvars, design = alltds.svy)
table5.var <- cbind(table5.var.append, table5.var)
colnames(table5.var)[1] <- "Total"
rownames(table5.var) <- myrownames

write.csv(table5.var, "Table 5 Variances.csv")


##========Table 6 (TD-constituency difference)=========
#2016 data only

table6 <- matrix(NA, nrow = 7, ncol = 8)
rownames(table6) <- c("Avg", "Q22", "Q21", "Q26", "Q24", "Q23", "Q25")
colnames(table6) <- levels(alltds$PartySimple)

#issue by issue, by party
table6[2,] <- by(tdsurvey$Q22A - tdsurvey$Q22D, tdsurvey$PartySimple, function(x) mean(x, na.rm=TRUE))
table6[3,] <- by(tdsurvey$Q21A - tdsurvey$Q21D, tdsurvey$PartySimple, function(x) mean(x, na.rm=TRUE))
table6[4,]  <- by(tdsurvey$Q26A - tdsurvey$Q26D, tdsurvey$PartySimple, function(x) mean(x, na.rm=TRUE))
table6[5,] <- by(tdsurvey$Q24A - tdsurvey$Q24D, tdsurvey$PartySimple, function(x) mean(x, na.rm=TRUE))
table6[6,]  <- by(tdsurvey$Q23A - tdsurvey$Q23D, tdsurvey$PartySimple, function(x) mean(x, na.rm=TRUE))
table6[7,]  <- by(tdsurvey$Q25A - tdsurvey$Q25D, tdsurvey$PartySimple, function(x) mean(x, na.rm=TRUE))
table6[1,] <- colMeans(table6[2:7,], na.rm=TRUE)

#total
table6.append <- matrix(NA, nrow = 7, ncol = 1)
colnames(table6.append) <- "Total"

table6.append[2] <- mean(tdsurvey$Q22A - tdsurvey$Q22D, na.rm=TRUE)
table6.append[3] <- mean(tdsurvey$Q21A - tdsurvey$Q21D, na.rm=TRUE)
table6.append[4] <- mean(tdsurvey$Q26A - tdsurvey$Q26D, na.rm=TRUE)
table6.append[5] <- mean(tdsurvey$Q24A - tdsurvey$Q24D, na.rm=TRUE)
table6.append[6] <- mean(tdsurvey$Q23A - tdsurvey$Q23D, na.rm=TRUE)
table6.append[7] <- mean(tdsurvey$Q25A - tdsurvey$Q25D, na.rm=TRUE)

table6.append[1] <- mean(table6.append[2:7])
table6 <- cbind(table6.append, table6)

write.csv(table6, "Table 6 - TD-Constituency Difference.csv")

#Sig tests
#Compute differences
tdsurvey$Q21_addiff_directional <- tdsurvey$Q21A - tdsurvey$Q21D
tdsurvey$Q22_addiff_directional <- tdsurvey$Q22A - tdsurvey$Q22D
tdsurvey$Q23_addiff_directional <- tdsurvey$Q23A - tdsurvey$Q23D
tdsurvey$Q24_addiff_directional <- tdsurvey$Q24A - tdsurvey$Q24D
tdsurvey$Q25_addiff_directional <- tdsurvey$Q25A - tdsurvey$Q25D
tdsurvey$Q26_addiff_directional <- tdsurvey$Q26A - tdsurvey$Q26D
#Get average of differences - for each case, average non-missing values
tdsurvey$avg_addiff_directional <- apply(tdsurvey[,c("Q21_addiff_directional", "Q22_addiff_directional", "Q23_addiff_directional", "Q24_addiff_directional", "Q25_addiff_directional", "Q26_addiff_directional")], 1, mean, na.rm = TRUE)

##---Bivariate---
#FF-SF
tdsurvey.svy <- svydesign(ids = ~Name, fpc = ~fpc, data = tdsurvey[tdsurvey$PartySimple == "FF" | tdsurvey$PartySimple == "SF",])
svyttest(avg_addiff_directional ~ PartySimple, tdsurvey.svy)
#FF-Left
tdsurvey.svy <- svydesign(ids = ~Name, fpc = ~fpc, data = tdsurvey[tdsurvey$PartySimple == "FF" | tdsurvey$PartySimple == "Left",])
svyttest(avg_addiff_directional ~ PartySimple, tdsurvey.svy)
#FG-SF
tdsurvey.svy <- svydesign(ids = ~Name, fpc = ~fpc, data = tdsurvey[tdsurvey$PartySimple == "FG" | tdsurvey$PartySimple == "SF",])
svyttest(avg_addiff_directional ~ PartySimple, tdsurvey.svy)
#FG-Leftist
tdsurvey.svy <- svydesign(ids = ~Name, fpc = ~fpc, data = tdsurvey[tdsurvey$PartySimple == "FG" | tdsurvey$PartySimple == "Left",])
svyttest(avg_addiff_directional ~ PartySimple, tdsurvey.svy)

#Effect of intra-party competition
tdsurvey.svy <- svydesign(ids = ~Name, fpc = ~fpc, data = tdsurvey)
base_ff <- svyglm(avg_addiff_directional ~ PartySimple + ipc, design = tdsurvey.svy)
summary(base_ff)$coefficients

###---Variance---
tdsurvey.svy <- svydesign(ids = ~Name, fpc = ~fpc, data = tdsurvey)

#Alternative way to run the tables above, which includes standard errors
table6.var <- t(svyby(~avg_addiff_directional + Q22_addiff_directional + Q21_addiff_directional + Q26_addiff_directional + Q24_addiff_directional + Q23_addiff_directional + Q25_addiff_directional, ~PartySimple, design = tdsurvey.svy, svymean, na.rm=TRUE)[,-1])
table6.var.append <- svymean(~avg_addiff_directional + Q22_addiff_directional + Q21_addiff_directional + Q26_addiff_directional + Q24_addiff_directional + Q23_addiff_directional + Q25_addiff_directional, design = tdsurvey.svy, na.rm = TRUE)
table6.var <- cbind(c(table6.var.append, SE(table6.var.append)), table6.var)
colnames(table6.var)[1] <- "Total"

write.csv(table6.var[8:14,], "Table 6 Variances.csv")

