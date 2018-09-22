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
tdsurvey <- read.dta("TD Survey 2007-16 - 13 Jan 2018.dta")
alltds <- read.dta("All TDs data - 13 Jan 2018.dta")

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

#Create matrix to write to CSV
table1 <- matrix(NA, nrow = 6, ncol = 8)
rownames(table1) <- c("Q22", "Q21", "Q26", "Q24", "Q23", "Q25")
colnames(table1) <- levels(alltds$PartySimple)

myvars <- alltds[,c("Q22A", "Q21A", "Q26A", "Q24A", "Q23A", "Q25A")]
table1[1:6,] <- t(apply(myvars, 2, function(x) tapply(x, alltds$PartySimple, mean, na.rm=TRUE)))

#--Means--
#Data for "total" column
table1.append <- matrix(NA, nrow = 6, ncol = 1)
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
    #Calcualte the mean polciy position for each of the three years
    yearmeans <- tapply(policyvar, yearvar, mean, na.rm=TRUE)
    
    #For each respondent in the dataset, calculate difference from the year-specific mean
    meandiff <- mapply(function(position, year) position - yearmeans[year], policyvar, yearvar)
}

#Compute individual-level difference between TD and year-specific average
myvars <- alltds[,c("Q22A", "Q21A", "Q26A", "Q24A", "Q23A", "Q25A")]
yearmeans <- apply(myvars, 2, function(x) yearmean_dist(x, alltds$year))
colnames(yearmeans) <- gsub("A", "_meandiff", colnames(yearmeans))
alltds <- cbind(alltds, yearmeans)

#Compute deviation from _meandifferences
myvars <- alltds[,c("Q22_meandiff", "Q21_meandiff", "Q26_meandiff", "Q24_meandiff", "Q23_meandiff", "Q25_meandiff")]
table2[2:7,] <- t(apply(myvars, 2, function(y) tapply(y, alltds$PartySimple, function(x) sqrt(sum(x ^ 2, na.rm = TRUE)/ sum(!is.na(x))))))

#Compute average deviation
table2[1,] <- colMeans(table2[2:7,], na.rm=TRUE)

#Get "total" column
table2.append <- matrix(NA, nrow = 7, ncol = 1)
colnames(table2.append) <- "Total"
table2.append[2:7] <- apply(myvars, 2, function(x) sqrt(sum(x ^ 2, na.rm = TRUE) / sum(!is.na(x))))
table2.append[1] <- mean(table2.append[2:7])
table2 <- cbind(table2.append, table2)

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

#SF (t = 9.89, p < .001) and left (t = 8.29, p < .001) are sig different than FF
base_ff <- svyglm(avg_meandiff_sq ~ PartySimple + ipc, design = alltds.svy)
summary(base_ff)$coefficients

#SF (t = 6.27, p < .001), and left (t = 7.26, p < .001) are sig different than FG
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
tdsurvey$Q21_addiff <- tdsurvey$Q21A - tdsurvey$Q21D
tdsurvey$Q22_addiff <- tdsurvey$Q22A - tdsurvey$Q22D
tdsurvey$Q23_addiff <- tdsurvey$Q23A - tdsurvey$Q23D
tdsurvey$Q24_addiff <- tdsurvey$Q24A - tdsurvey$Q24D
tdsurvey$Q25_addiff <- tdsurvey$Q25A - tdsurvey$Q25D
tdsurvey$Q26_addiff <- tdsurvey$Q26A - tdsurvey$Q26D

#Create matrix to write to CSV
table3 <- matrix(NA, nrow = 7, ncol = 8)
rownames(table3) <- c("Avg", "Q22", "Q21", "Q26", "Q24", "Q23", "Q25")
colnames(table3) <- levels(tdsurvey$PartySimple)

#Compute deviation from differences
table3[2,] <- tapply(tdsurvey$Q22_addiff, tdsurvey$PartySimple, function(x) sqrt(sum(x ^ 2, na.rm = TRUE)/ sum(!is.na(x))))
table3[3,] <- tapply(tdsurvey$Q21_addiff, tdsurvey$PartySimple, function(x) sqrt(sum(x ^ 2, na.rm = TRUE)/ sum(!is.na(x))))
table3[4,] <- tapply(tdsurvey$Q26_addiff, tdsurvey$PartySimple, function(x) sqrt(sum(x ^ 2, na.rm = TRUE)/ sum(!is.na(x))))
table3[5,] <- tapply(tdsurvey$Q24_addiff, tdsurvey$PartySimple, function(x) sqrt(sum(x ^ 2, na.rm = TRUE)/ sum(!is.na(x))))
table3[6,] <- tapply(tdsurvey$Q23_addiff, tdsurvey$PartySimple, function(x) sqrt(sum(x ^ 2, na.rm = TRUE)/ sum(!is.na(x))))
table3[7,] <- tapply(tdsurvey$Q25_addiff, tdsurvey$PartySimple, function(x) sqrt(sum(x ^ 2, na.rm = TRUE)/ sum(!is.na(x))))

#Compute average deviation
table3[1,] <- colMeans(table3[2:7,], na.rm=TRUE)

#Get "total" column
table3.append <- matrix(NA, nrow = 7, ncol = 1)
colnames(table3.append) <- "Total"
table3.append[2] <- sqrt(sum(tdsurvey$Q22_addiff ^ 2, na.rm = TRUE) / sum(!is.na(tdsurvey$Q22_addiff)))
table3.append[3] <- sqrt(sum(tdsurvey$Q21_addiff ^ 2, na.rm = TRUE) / sum(!is.na(tdsurvey$Q21_addiff)))
table3.append[4] <- sqrt(sum(tdsurvey$Q26_addiff ^ 2, na.rm = TRUE) / sum(!is.na(tdsurvey$Q26_addiff)))
table3.append[5] <- sqrt(sum(tdsurvey$Q24_addiff ^ 2, na.rm = TRUE) / sum(!is.na(tdsurvey$Q24_addiff)))
table3.append[6] <- sqrt(sum(tdsurvey$Q23_addiff ^ 2, na.rm = TRUE) / sum(!is.na(tdsurvey$Q23_addiff)))
table3.append[7] <- sqrt(sum(tdsurvey$Q25_addiff ^ 2, na.rm = TRUE) / sum(!is.na(tdsurvey$Q25_addiff)))
table3.append[1] <- mean(table3.append[2:7])
table3 <- cbind(table3.append, table3)

#Write to table
write.csv(table3, "Table 3 - TD-Constituent Deviation.csv")

#Sig tests
#Compute differences
tdsurvey$Q21_addiff_sq <- (tdsurvey$Q21A - tdsurvey$Q21D) ^ 2
tdsurvey$Q22_addiff_sq <- (tdsurvey$Q22A - tdsurvey$Q22D) ^ 2
tdsurvey$Q23_addiff_sq <- (tdsurvey$Q23A - tdsurvey$Q23D) ^ 2
tdsurvey$Q24_addiff_sq <- (tdsurvey$Q24A - tdsurvey$Q24D) ^ 2
tdsurvey$Q25_addiff_sq <- (tdsurvey$Q25A - tdsurvey$Q25D) ^ 2
tdsurvey$Q26_addiff_sq <- (tdsurvey$Q26A - tdsurvey$Q26D) ^ 2
#Get average of differenes - for each case, average non-missing values
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
#Effect of intra-party competition
tdsurvey.svy <- svydesign(ids = ~Name, fpc = ~fpc, data = tdsurvey)

#SF-FF: t-value 3.18, p-value .002; Left-FF: t-value 2.26, p-value .026)
base_ff <- svyglm(avg_addiff_sq ~ PartySimple + ipc, design = tdsurvey.svy)
summary(base_ff)$coefficients

#SF-FG: t-value 4.09, p-value <.001; Left-FG: t-value 2.77, p-value .007
base_fg <- svyglm(avg_addiff_sq ~ relevel(PartySimple, ref = "FG") + ipc, design = tdsurvey.svy)
summary(base_fg)$coefficients

#TDs without an intra-party competitor deviate from their districts by 2.16 squared points more

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
table4 <- matrix(NA, nrow = 7, ncol = 8)
rownames(table4) <- c("Avg", "Q22", "Q21", "Q26", "Q24", "Q23", "Q25")
colnames(table4) <- levels(alltds$PartySimple)[1:8]

#Identify instances of a >= 2pt disagreement
#Omit independnets, beacuse we can't really calculate the distance between an independent and his/her party
self <- alltds[alltds$PartySimple != "Ind" ,c("Q22A", "Q21A", "Q26A", "Q24A", "Q23A", "Q25A")]
party <- alltds[alltds$PartySimple != "Ind", c("Q22B", "Q21B", "Q26B", "Q24B", "Q23B", "Q25B")]
bigdiff <- abs(self - party) >= 2

#Compute disagreement rate, by party
table4[2,] <- tapply(bigdiff[,1], alltds$PartySimple[alltds$PartySimple != "Ind"], function(x) sum(x, na.rm=TRUE) / sum(!is.na(x)))
table4[3,] <- tapply(bigdiff[,2], alltds$PartySimple[alltds$PartySimple != "Ind"], function(x) sum(x, na.rm=TRUE) / sum(!is.na(x)))
table4[4,] <- tapply(bigdiff[,3], alltds$PartySimple[alltds$PartySimple != "Ind"], function(x) sum(x, na.rm=TRUE) / sum(!is.na(x)))
table4[5,] <- tapply(bigdiff[,4], alltds$PartySimple[alltds$PartySimple != "Ind"], function(x) sum(x, na.rm=TRUE) / sum(!is.na(x)))
table4[6,] <- tapply(bigdiff[,5], alltds$PartySimple[alltds$PartySimple != "Ind"], function(x) sum(x, na.rm=TRUE) / sum(!is.na(x)))
table4[7,] <- tapply(bigdiff[,6], alltds$PartySimple[alltds$PartySimple != "Ind"], function(x) sum(x, na.rm=TRUE) / sum(!is.na(x)))
table4[1,] <- tapply(rowSums(bigdiff, na.rm=TRUE), alltds$PartySimple[alltds$PartySimple != "Ind"], function(x) sum(x, na.rm=TRUE)) / 
    tapply(rowSums(!is.na(bigdiff)), alltds$PartySimple[alltds$PartySimple != "Ind"], function(x) sum(x, na.rm=TRUE))

#Compute total disagreement rate
table4.append <- matrix(NA, nrow = 7, ncol = 1)
colnames(table4.append) <- "Total"
table4.append[2,] <- sum(bigdiff[,1], na.rm = TRUE) / sum(!is.na(bigdiff[,1]))
table4.append[3,] <- sum(bigdiff[,2], na.rm = TRUE) / sum(!is.na(bigdiff[,2]))
table4.append[4,] <- sum(bigdiff[,3], na.rm = TRUE) / sum(!is.na(bigdiff[,3]))
table4.append[5,] <- sum(bigdiff[,4], na.rm = TRUE) / sum(!is.na(bigdiff[,4]))
table4.append[6,] <- sum(bigdiff[,5], na.rm = TRUE) / sum(!is.na(bigdiff[,5]))
table4.append[7,] <- sum(bigdiff[,6], na.rm = TRUE) / sum(!is.na(bigdiff[,6]))
table4.append[1,] <- sum(bigdiff, na.rm = TRUE) / sum(!is.na(bigdiff))
table4 <- cbind(table4.append, table4)

write.csv(table4, "Table 4 - TD-Party Disagreement.csv")

##========Table 5 (TD-party deviation)=========
#All waves

table5 <- matrix(NA, nrow = 7, ncol = 8)
rownames(table5) <- c("Avg", "Q22", "Q21", "Q26", "Q24", "Q23", "Q25")
colnames(table5) <- levels(alltds$PartySimple)

#Compute differences
alltds$Q21_abdiff <- alltds$Q21A - alltds$Q21B
alltds$Q22_abdiff <- alltds$Q22A - alltds$Q22B
alltds$Q23_abdiff <- alltds$Q23A - alltds$Q23B
alltds$Q24_abdiff <- alltds$Q24A - alltds$Q24B
alltds$Q25_abdiff <- alltds$Q25A - alltds$Q25B
alltds$Q26_abdiff <- alltds$Q26A - alltds$Q26B

#Compute deviation from differences
table5[2,] <- tapply(alltds$Q22_abdiff[alltds$PartySimple != "Ind"], alltds$PartySimple[alltds$PartySimple != "Ind"], function(x) sqrt(sum(x ^ 2, na.rm = TRUE)/ sum(!is.na(x))))
table5[3,] <- tapply(alltds$Q21_abdiff[alltds$PartySimple != "Ind"], alltds$PartySimple[alltds$PartySimple != "Ind"], function(x) sqrt(sum(x ^ 2, na.rm = TRUE)/ sum(!is.na(x))))
table5[4,] <- tapply(alltds$Q26_abdiff[alltds$PartySimple != "Ind"], alltds$PartySimple[alltds$PartySimple != "Ind"], function(x) sqrt(sum(x ^ 2, na.rm = TRUE)/ sum(!is.na(x))))
table5[5,] <- tapply(alltds$Q24_abdiff[alltds$PartySimple != "Ind"], alltds$PartySimple[alltds$PartySimple != "Ind"], function(x) sqrt(sum(x ^ 2, na.rm = TRUE)/ sum(!is.na(x))))
table5[6,] <- tapply(alltds$Q23_abdiff[alltds$PartySimple != "Ind"], alltds$PartySimple[alltds$PartySimple != "Ind"], function(x) sqrt(sum(x ^ 2, na.rm = TRUE)/ sum(!is.na(x))))
table5[7,] <- tapply(alltds$Q25_abdiff[alltds$PartySimple != "Ind"], alltds$PartySimple[alltds$PartySimple != "Ind"], function(x) sqrt(sum(x ^ 2, na.rm = TRUE)/ sum(!is.na(x))))
#Compute average deviation
table5[1,] <- colMeans(table5[2:7,], na.rm=TRUE)

#Get "total" column
table5.append <- matrix(NA, nrow = 7, ncol = 1)
colnames(table5.append) <- "Total"
table5.append[2] <- sqrt(sum(alltds$Q22_abdiff[alltds$PartySimple != "Ind"] ^ 2, na.rm = TRUE) / sum(!is.na(alltds$Q22_abdiff[alltds$PartySimple != "Ind"])))
table5.append[3] <- sqrt(sum(alltds$Q21_abdiff[alltds$PartySimple != "Ind"] ^ 2, na.rm = TRUE) / sum(!is.na(alltds$Q21_abdiff[alltds$PartySimple != "Ind"])))
table5.append[4] <- sqrt(sum(alltds$Q26_abdiff[alltds$PartySimple != "Ind"] ^ 2, na.rm = TRUE) / sum(!is.na(alltds$Q26_abdiff[alltds$PartySimple != "Ind"])))
table5.append[5] <- sqrt(sum(alltds$Q24_abdiff[alltds$PartySimple != "Ind"] ^ 2, na.rm = TRUE) / sum(!is.na(alltds$Q24_abdiff[alltds$PartySimple != "Ind"])))
table5.append[6] <- sqrt(sum(alltds$Q23_abdiff[alltds$PartySimple != "Ind"] ^ 2, na.rm = TRUE) / sum(!is.na(alltds$Q23_abdiff[alltds$PartySimple != "Ind"])))
table5.append[7] <- sqrt(sum(alltds$Q25_abdiff[alltds$PartySimple != "Ind"] ^ 2, na.rm = TRUE) / sum(!is.na(alltds$Q25_abdiff[alltds$PartySimple != "Ind"])))
table5.append[1] <- mean(table5.append[2:7])
table5 <- cbind(table5.append, table5)

#Write to table
table5 <- table5[,c(1:7)]
write.csv(table5, "Table 5 - TD-Party Self-Placement Deviation.csv")

#Sig tests
#Compute _differences
alltds$Q21_abdiff_sq <- (alltds$Q21A - alltds$Q21B) ^ 2
alltds$Q22_abdiff_sq <- (alltds$Q22A - alltds$Q22B) ^ 2
alltds$Q23_abdiff_sq <- (alltds$Q23A - alltds$Q23B) ^ 2
alltds$Q24_abdiff_sq <- (alltds$Q24A - alltds$Q24B) ^ 2
alltds$Q25_abdiff_sq <- (alltds$Q25A - alltds$Q25B) ^ 2
alltds$Q26_abdiff_sq <- (alltds$Q26A - alltds$Q26B) ^ 2
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


##========Table 6 (TD-constituency difference) -- Old Table 8=========
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

