rm(list = ls())

library("rstudioapi")
library("foreign")

setwd(dirname(getActiveDocumentContext()$path))

##=========generate alltds data=========
alltds <- read.dta("All TDs data - 13 Jan 2018.dta")
alltds$Interviewed <- NULL
alltds$Party3cat <- NULL

#Note that the TD names, and other publicly available data, are real
#However, we will simulate answers to survey questions to preserve anonymity
#We will also not disclose which actual TDs completed the survey

#Generate random data for the policy variables
varnames.ab <- c("Q21A", "Q22A", "Q23A", "Q24A", "Q25A", "Q26A", "Q27A",
                 "Q21B", "Q22B", "Q23B", "Q24B", "Q25B", "Q26B", "Q27B")
nreps <- length(varnames.ab)
alltds[varnames.ab] <- replicate(nreps, floor(runif(491, 0, 11)))

alltds$fpc <- 313/491



##=========generate tdsurvey data=========

#Variables needed: Wave, Name, Party, Constituency_coded, Age, PartySimple, fpc

##--Sample TDs--

#Randomly select 313 fake survey responses from the universe of 491 possible responses across the 3 Dail sessions
#This is a two-level dataset: with TD-level and survey-level variables
#This allows for the possibility that one TD could complete multiple surveys
set.seed(8257)
faketds <- data.frame(Name = sapply(sample(x=491, size = 313, replace = TRUE), toString), stringsAsFactors = FALSE)

#Check if any TD in the simulated data was sampled more than three times
#Since this is a three-wave survey, this would be logically impossible
#If there are problematic cases, manually regenerate data 
#I've picked a seed that meets the needed property
table(faketds$Name)
sum(table(faketds$Name) > 3)

##--Generate time-invariant TD-level variables--
#party and constituency

#Define parties - exclude PD since we never had any PD interviews
partynames <- c("FF", "FG", "Lab", "Gr", "SF", "Left", "Ind")

#Note that this sampling doesn't impose a constraint for which parties existed in which years, except for omitting PDs
unique_indivs <- data.frame(Name = unique(faketds$Name))
unique_indivs$PartySimple <- sample(partynames, size = nrow(unique_indivs), replace = TRUE)
unique_indivs$Constituency_coded <- sample(LETTERS, size = nrow(unique_indivs), replace = TRUE)

##--Generate time-dependent survey-level variables--

#Count the number of surveys that a given individiual has completed
nwaves <- data.frame(table(faketds$Name))
names(nwaves) <- c("Name", "nwaves")
unique_indivs <- merge(unique_indivs, nwaves)

#Randomly select which waves a respondent has completed, given the number of waves they have
#Then generate a "true" or "false" variable for each year
years_answered <- data.frame(t(sapply(unique_indivs$nwaves, function(x) {
    yearlist <- sample(c("2007", "2011", "2016"), size = x)
    c("2007" %in% yearlist, "2011" %in% yearlist, "2016" %in% yearlist)
})))
names(years_answered) <- c("answered.2007", "answered.2011", "answered.2016")
unique_indivs <- cbind(unique_indivs, years_answered)

#Generate a random age for the first observation for each individual, then add the appropriate number onto subsequent individuals
unique_indivs$Age.2007 <- floor(runif(n = nrow(unique_indivs), min = 18, max = 85))
unique_indivs$Age.2011 <- unique_indivs$Age.2007 + 4
unique_indivs$Age.2016 <- unique_indivs$Age.2007 + 9

#Convert from wide to long data, and drop observations where we didn't receive a survey
tdsurvey <- reshape(unique_indivs, varying = c("answered.2007", "Age.2007", "answered.2011", "Age.2011", "answered.2016", "Age.2016"),
                    idvar = "Name", direction = "long")

tdsurvey <- tdsurvey[tdsurvey$answered == TRUE, c("Name", "PartySimple", "Constituency_coded", "time", "Age")]
names(tdsurvey)[4] <- "Wave"

##--Generate survey-level variables that are independent of time--

varnames.abcd <- as.vector(sapply(c("A", "B", "C", "D"), function(x) paste0("Q", 21:27, x)))
nreps <- length(varnames.abcd)
tdsurvey[varnames.abcd] <- replicate(nreps, floor(runif(313, 0, 11)))

tdsurvey$ipc <- factor(x = sample(c("yes", "no"), size = 313, replace = TRUE),
                       levels = c("yes", "no"))

#Set fpc manually
tdsurvey$fpc[tdsurvey$Wave == "2007"] <- 102/166
tdsurvey$fpc[tdsurvey$Wave == "2011"] <- 115/166
tdsurvey$fpc[tdsurvey$Wave == "2016"] <- 97/158

##=========Save data=========

write.dta(tdsurvey, "TD Survey 2007-16 fake data.dta", convert.factors = "string")
write.dta(alltds, "All TDs fake data.dta", convert.factors = "string")


