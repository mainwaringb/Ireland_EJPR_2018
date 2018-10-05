library("survey")
library("foreign")
library("rstudioapi")

rm(list = ls())
setwd(dirname(getActiveDocumentContext()$path))

tdsurvey <- read.dta("TD Survey 2007-16 fake data.dta")
tdsurvey16 <- tdsurvey[tdsurvey$Wave == "2016",]

#================boxplots of TD attitudes===============
#Reverse order of parties so that FF is at top
tdsurvey16$reverseparty <- factor(tdsurvey16$PartySimple, c("Ind", "Left", "Gr", "SF", "Lab", "FG", "FF"))

#United Ireland and Left-Right
png(filename = "Figure 1A.PNG", width = 800, height = 350)
par(mfrow=c(1,2), mar=c(5.1, 2.1, 0.6, 2.1), cex=1.3, oma = c(0,2,0,0), las = 1)
boxplot(Q22A ~ reverseparty, data = tdsurvey16, horizontal = TRUE, xlab = "United Ireland", ylim=c(0,10))
par(mar = c(5.1, 2.1, 0.6, 2.1))
boxplot(Q21A ~ reverseparty, data = tdsurvey16, horizontal = TRUE, xlab = "Left-Right", names = rep("", 7), ylim=c(0,10))
dev.off()

#Abortion and EU
png(filename = "Figure 1B.PNG", width = 800, height = 350)
par(mfrow=c(1,2), mar=c(5.1, 2.1, 0.6, 2.1), cex=1.3, oma = c(0,2,0,0), las = 1)
boxplot(Q26A ~ reverseparty, data = tdsurvey16, horizontal = TRUE, xlab = "Abortion", ylim=c(0,10))
par(mar = c(5.1, 2.1, 0.6, 2.1))
boxplot(Q24A ~ reverseparty, data = tdsurvey16, horizontal = TRUE, xlab = "European Union", names = rep("", 7), ylim=c(0,10))
dev.off()

#Fiscal policy and environment
png(filename = "Figure 1C.PNG", width = 800, height = 350)
par(mfrow=c(1,2), mar=c(5.1, 2.1, 0.6, 2.1), cex=1.3, oma = c(0,2,0,0), las = 1)
boxplot(Q23A ~ reverseparty, data = tdsurvey16, horizontal = TRUE, xlab = "Fiscal Policy", ylim=c(0,10))
par(mar = c(5.1, 2.1, 0.6, 2.1))
boxplot(Q25A ~ reverseparty, data = tdsurvey16, horizontal = TRUE, xlab = "Environment", names = rep("", 7), ylim=c(0,10))
dev.off()


setEPS()
#United Ireland and Left-Right
postscript(file = "Figure 1A.eps")
par(mfrow=c(1,2), mar=c(5.1, 2.1, 0.6, 2.1), cex=1.3, oma = c(0,2,0,0), las = 1)
boxplot(Q22A ~ reverseparty, data = tdsurvey16, horizontal = TRUE, xlab = "United Ireland", ylim=c(0,10))
par(mar = c(5.1, 2.1, 0.6, 2.1))
boxplot(Q21A ~ reverseparty, data = tdsurvey16, horizontal = TRUE, xlab = "Left-Right", names = rep("", 7), ylim=c(0,10))
dev.off()

#Abortion and EU
postscript(file = "Figure 1B.eps")
par(mfrow=c(1,2), mar=c(5.1, 2.1, 0.6, 2.1), cex=1.3, oma = c(0,2,0,0), las = 1)
boxplot(Q26A ~ reverseparty, data = tdsurvey16, horizontal = TRUE, xlab = "Abortion", ylim=c(0,10))
par(mar = c(5.1, 2.1, 0.6, 2.1))
boxplot(Q24A ~ reverseparty, data = tdsurvey16, horizontal = TRUE, xlab = "European Union", names = rep("", 7), ylim=c(0,10))
dev.off()

#Fiscal policy and environment
postscript(file = "Figure 1C.eps")
par(mfrow=c(1,2), mar=c(5.1, 2.1, 0.6, 2.1), cex=1.3, oma = c(0,2,0,0), las = 1)
boxplot(Q23A ~ reverseparty, data = tdsurvey16, horizontal = TRUE, xlab = "Fiscal Policy", ylim=c(0,10))
par(mar = c(5.1, 2.1, 0.6, 2.1))
boxplot(Q25A ~ reverseparty, data = tdsurvey16, horizontal = TRUE, xlab = "Environment", names = rep("", 7), ylim=c(0,10))
dev.off()