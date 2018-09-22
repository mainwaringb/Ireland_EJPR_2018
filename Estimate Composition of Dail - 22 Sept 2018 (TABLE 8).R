rm(list = ls())
library("survey")
library("foreign")
library("rstudioapi")

setwd(dirname(getActiveDocumentContext()$path))

#Load survey data
alltds <- read.dta("All TDs data - 13 Jan 2018.dta")
alltds.svy <- svydesign(ids = ~Name_of_TDs, fpc = ~fpc, data = alltds)


##=============TABLE 8 (COMPUTE YEAR EFFECTS) ==========

#Check which variables topline year effects are visible for
reg.base.q21 <- svyglm(Q21A ~ PartySimple + year, design = alltds.svy)
reg.base.q22 <- svyglm(Q22A ~ PartySimple + year, design = alltds.svy)
reg.base.q23 <- svyglm(Q23A ~ PartySimple + year, design = alltds.svy)
reg.base.q24 <- svyglm(Q24A ~ PartySimple + year, design = alltds.svy)
reg.base.q25 <- svyglm(Q25A ~ PartySimple + year, design = alltds.svy)
reg.base.q26 <- svyglm(Q26A ~ PartySimple + year, design = alltds.svy)
reg.base.q27 <- svyglm(Q27A ~ PartySimple + year, design = alltds.svy)

#Quuck function to get year effects for each party
#INPUT: a regression object
#OUTPUT: a data frame with 3 columns (estimate, std error, p-val) and rwos for each party-year
#FF, FG, Lab, Gr (2016 only), SF, Left (base year is 2011), Ind
#This function could probably be more elegant, but probably not worth the effort

yeareffects <- function(regobject){
    emptycol <- rep(as.numeric(NA), 12)
    coefs <- data.frame(estimate = emptycol, stderror = emptycol, pval = emptycol)
    row.names(coefs) <- c("FF: 07-11", "FF: 11-16", "FG: 07-11", "FG: 11-16", "Lab: 07-11", "Lab: 11-16",
                           "SF: 07-11", "SF: 11-16", 
                          "Ind: 07-11", "Ind: 11-16", "Gr: 07-16", "Left: 11-16")
    
    #Pull coefficients and vcov matrix from regression object
    allestimates <- regobject$coefficients
    allvcov <- vcov(regobject)
    
    #2016 effect for leftists: leftist + 2016
    #2011 effect for leftists: leftist + 2011 + leftist:2011
    #Difference: 2016 - 2011 - leftist:2011
    
    #Compute coefficients
    coefs$estimate[1] <- allestimates["year2011"]
    coefs$estimate[2] <- allestimates["year2016"] - allestimates["year2011"]
    coefs$estimate[3] <- allestimates["year2011"] + allestimates["PartySimpleFG:year2011"]
    coefs$estimate[4] <- (allestimates["year2016"] + allestimates["PartySimpleFG:year2016"]) - 
        coefs$estimate[3]
    coefs$estimate[5] <- allestimates["year2011"] + allestimates["PartySimpleLab:year2011"]
    coefs$estimate[6] <- (allestimates["year2016"] + allestimates["PartySimpleLab:year2016"]) - 
        coefs$estimate[5]
    coefs$estimate[7] <- allestimates["year2011"] + allestimates["PartySimpleSF:year2011"]
    coefs$estimate[8] <- allestimates["year2016"] + allestimates["PartySimpleSF:year2016"] - 
        coefs$estimate[7]
    coefs$estimate[9] <- allestimates["year2011"] + allestimates["PartySimpleInd:year2011"]
    coefs$estimate[10] <- (allestimates["year2016"] + allestimates["PartySimpleInd:year2016"]) - 
        coefs$estimate[9]
    coefs$estimate[11] <- allestimates["year2016"] + allestimates["PartySimpleGr:year2016"]
    coefs$estimate[12] <- -allestimates["PartySimpleLeft:year2011"]
    
    
    #Cov(W+X, Y+Z) = cov(W, Y) + cov(W,Z) + cov(X,Y) + cov(X,Z)
    #VAR((W+X) - (Y+Z)) = VAR(W+X) + VAR(Y+Z) - 2COV(W+X,Y+Z)
    # = VAR(W) + VAR(X) + 2COV(W,X) + VAR(Y) + VAR(Z) + 2COV(Y,Z)
    #       - 2COV(W,Y) - 2COV(W,Z) - 2COV(X,Y) - 2COV(X,Z)
    #W = 2016, X = Party:2016, Y = 2011, Z = Party:2011
    
    #Compute SEs
    #First compute variances
    coefs$stderror[1] <- allvcov["year2011", "year2011"]
    coefs$stderror[2] <- allvcov["year2011", "year2011"] + allvcov["year2016", "year2016"] - 
        (2 * allvcov["year2011", "year2016"])
    
    coefs$stderror[3] <- allvcov["year2011", "year2011"] + allvcov["PartySimpleFG:year2011", "PartySimpleFG:year2011"] +
        (2 * allvcov["year2011", "PartySimpleFG:year2011"])
    coefs$stderror[4] <- allvcov["year2016", "year2016"] + allvcov["PartySimpleFG:year2016", "PartySimpleFG:year2016"] +
        (2 * allvcov["year2016", "PartySimpleFG:year2016"]) + coefs$stderror[3] -
        (2 * allvcov["year2016", "year2011"]) - (2 * allvcov["year2016", "PartySimpleFG:year2011"]) -
        (2 * allvcov["PartySimpleFG:year2016", "year2011"]) - (2 * allvcov["PartySimpleFG:year2016", "PartySimpleFG:year2011"])
        
    coefs$stderror[5] <- allvcov["year2011", "year2011"] + allvcov["PartySimpleLab:year2011", "PartySimpleLab:year2011"] +
        (2 * allvcov["year2011", "PartySimpleLab:year2011"])
    coefs$stderror[6] <- allvcov["year2016", "year2016"] + allvcov["PartySimpleLab:year2016", "PartySimpleLab:year2016"] +
        (2 * allvcov["year2016", "PartySimpleLab:year2016"]) + coefs$stderror[5] -
        (2 * allvcov["year2016", "year2011"]) - (2 * allvcov["year2016", "PartySimpleLab:year2011"]) -
        (2 * allvcov["PartySimpleLab:year2016", "year2011"]) - (2 * allvcov["PartySimpleLab:year2016", "PartySimpleLab:year2011"])
  
    coefs$stderror[7] <- allvcov["year2011", "year2011"] + allvcov["PartySimpleSF:year2011", "PartySimpleSF:year2011"] +
        (2 * allvcov["year2011", "PartySimpleSF:year2011"])
    coefs$stderror[8] <- allvcov["year2016", "year2016"] + allvcov["PartySimpleSF:year2016", "PartySimpleSF:year2016"] +
        (2 * allvcov["year2016", "PartySimpleSF:year2016"]) + coefs$stderror[7] -
        (2 * allvcov["year2016", "year2011"]) - (2 * allvcov["year2016", "PartySimpleSF:year2011"]) -
        (2 * allvcov["PartySimpleSF:year2016", "year2011"]) - (2 * allvcov["PartySimpleSF:year2016", "PartySimpleSF:year2011"])
    
    #2016 effect for leftists: leftist + 2016
    #2011 effect for leftists: leftist + 2011 + leftist:2011
    #Difference: 2016 - 2011 - leftist:2011
    
    #Cov(W+X, Y+Z) = cov(W, Y) + cov(W,Z) + cov(X,Y) + cov(X,Z)
    #Cov(W+X, Y + 0) = cov(W,Y) + cov(W, 0) + cov(X, Y) + cov(X,0) = cov(W,Y) + cov(X,Y)
    #Var(W-X-Y) = Var(W-X) + Var(Y) - 2Cov(W-X, Y) = Var(W-X) + Var(Y) -2(cov(W,Y) + cov(X,Y))
    #   = Var(W) + Var(X) - 2Cov(W,X) + Var(Y) -2Cov(W,Y) - 2Cov(X,Y)
    #Where W = 2016, X = 2011, Y = leftist:2011
    coefs$stderror[9] <- allvcov["year2011", "year2011"] + allvcov["PartySimpleInd:year2011", "PartySimpleInd:year2011"] +
        (2 * allvcov["year2011", "PartySimpleInd:year2011"])
    coefs$stderror[10] <- allvcov["year2016", "year2016"] + allvcov["PartySimpleInd:year2016", "PartySimpleInd:year2016"] +
        (2 * allvcov["year2016", "PartySimpleInd:year2016"]) + coefs$stderror[9] -
        (2 * allvcov["year2016", "year2011"]) - (2 * allvcov["year2016", "PartySimpleInd:year2011"]) -
        (2 * allvcov["PartySimpleInd:year2016", "year2011"]) - (2 * allvcov["PartySimpleInd:year2016", "PartySimpleInd:year2011"])

    
    coefs$stderror[11] <- allvcov["PartySimpleGr", "PartySimpleGr"] + allvcov["PartySimpleGr:year2016", "PartySimpleGr:year2016"] +
        (2 * allvcov["PartySimpleGr", "PartySimpleGr:year2016"])
    coefs$stderror[12] <- allvcov["year2016", "year2016"] + allvcov["year2011", "year2011"] + allvcov["PartySimpleLeft:year2011", "PartySimpleLeft:year2011"] -
        (2 * allvcov["year2016", "year2011"]) - (2 * allvcov["year2016", "PartySimpleLeft:year2011"]) - (2 * allvcov["year2011", "PartySimpleLeft:year2011"])
    
    
    #Then take square root of variances
    coefs$stderror <- sqrt(coefs$stderror)
    
    #Then compute t-values
    df <- regobject$df.residual
    coefs$pval <- sapply(coefs$estimate / coefs$stderror, function(x) 1 - pt(q = abs(x), df = df))
    
    return(coefs)
}

#Is it worth turning the below into a function or lapply/sapply statement? probably not

#Q21
reg.int.q21 <- svyglm(Q21A ~ PartySimple + year + PartySimple * year, design = alltds.svy)
write.csv(yeareffects(reg.int.q21), "Q21.csv")

#Q22 (United Ireland)
reg.int.q22 <- svyglm(Q22A ~ PartySimple + year + PartySimple * year, design = alltds.svy)
write.csv(yeareffects(reg.int.q22), "Q22.csv")

#Q23 (Fiscal policy)
reg.int.q23 <- svyglm(Q23A ~ PartySimple + year + PartySimple * year, design = alltds.svy)
write.csv(yeareffects(reg.int.q23), "Q23.csv")

#Q24 (European Union)
reg.int.q24 <- svyglm(Q24A ~ PartySimple + year + PartySimple * year, design = alltds.svy)
write.csv(yeareffects(reg.int.q24), "Q24.csv")

#Q25 (enviornment)
reg.int.q25 <- svyglm(Q25A ~ PartySimple + year + PartySimple * year, design = alltds.svy)
write.csv(yeareffects(reg.int.q25), "Q25.csv")

#Q26 (abortion)
reg.int.q26 <- svyglm(Q26A ~ PartySimple + year + PartySimple * year, design = alltds.svy)
write.csv(yeareffects(reg.int.q26), "Q26.csv")
