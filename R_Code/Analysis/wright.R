rm(list = ls())

# load packages 
library(arm)
library(compactr)
library(foreign)

setwd("~/Dropbox/Projects/Misreports/")
nes.house <- read.dta("Data/NES_House_misreport.dta")
nes.senate <- read.dta("Data/NES_Senate_misreport.dta")
ses.house <- read.dta("Data/SES_House_misreport.dta")
ses.senate <- read.dta("Data/SES_Senate_misreport.dta")

pdf("Figures/wright.pdf", height = 4.5, width = 4.5)
par(mfrow = c(3,1), mar = rep(0.75, 4), oma = c(3,4,1,1))
nudge <- .4

# Senate
eplot(xlim = c(1988, 2008), ylim = c(-1, 1),
      xlab = "Year", 
      ylab = "Estimated\nOverreporting", ylabpos = 2.3,
      main = "Senate Elections",
      xat = 1988 + c(0:8*2, 20))
abline(h = 0, lty = 2, col = "grey")
# loop over NES data
for (i in 1988 + c(0:8*2, 20)) {
  m <- lm(Rep_Sen_vote_dif ~ rep_sen_vote_prop, data = nes.senate, subset = year == i,
          weights = weight)
  est <- coef(m)[2]
  se <- sqrt(diag(vcov(m))[2])
  lwr <- est - 1.64*se 
  upr <- est + 1.64*se
  points(i, est, pch = 19, cex = .8)
  lines(c(i, i), c(lwr, upr))
}
# loop over SES data
for (i in c(1988, 1990, 1992)) {
  m <- lm(Rep_sen_vote_diff ~ rep_vote_prop, data = ses.senate, subset = year == i,
          weights = weight)
  est <- coef(m)[2]
  se <- sqrt(diag(vcov(m))[2])
  lwr <- est - 1.64*se 
  upr <- est + 1.64*se
  lines(c(i + nudge, i + nudge), c(lwr, upr), col = "red")
  points(i + nudge, est, cex = .8, col = "red", pch = 21, bg = "white")
  
}
legend(x = 1988, y = -.4, legend = c("NES", "SES"), lty = 1, bg = NA, 
       box.lwd = NA, pch = c(19, 21), pt.bg = "white", col = c("black", "red"),
       cex = .8)


# House
aplot("House Elections")
abline(h = 0, lty = 2, col = "grey")
# loop over NES data
for (i in 1988 + c(0:8*2, 20)) {
  m <- lm(Rep_House_vote_dif ~ rvprop, data = nes.house, subset = year == i,
          weights = weight)
  est <- coef(m)[2]
  se <- sqrt(diag(vcov(m))[2])
  lwr <- est - 1.64*se 
  upr <- est + 1.64*se
  points(i, est, pch = 19, cex = .8)
  lines(c(i, i), c(lwr, upr))
}
# loop over SES data
for (i in c(1988, 1990, 1992)) {
  m <- lm(Rep_House_vote_dif ~ rvprop, data = ses.house, subset = year == i,
          weights = weight)
  est <- coef(m)[2]
  se <- sqrt(diag(vcov(m))[2])
  lwr <- est - 1.64*se 
  upr <- est + 1.64*se
  lines(c(i + nudge, i + nudge), c(lwr, upr), col = "red")
  points(i + nudge, est, cex = .8, col = "red", pch = 21, bg = "white")
  
}

# Competitive House
aplot("Competitive House Elections")
abline(h = 0, lty = 2, col = "grey")
# loop over NES data
for (i in 1988 + c(0:8*2, 20)) {
  m <- lm(Rep_House_vote_dif ~ rvprop, data = nes.house, 
          subset = year == i & rvprop > .2 & rvprop < .8,
          weights = weight)
  est <- coef(m)[2]
  se <- sqrt(diag(vcov(m))[2])
  lwr <- est - 1.64*se 
  upr <- est + 1.64*se
  points(i, est, pch = 19, cex = .8)
  lines(c(i, i), c(lwr, upr))
}
# loop over SES data
for (i in c(1988, 1990, 1992)) {
  m <- lm(Rep_House_vote_dif ~ rvprop, data = ses.house,
          subset = year == i & rvprop > .2 & rvprop < .8,
          weights = weight)
  est <- coef(m)[2]
  se <- sqrt(diag(vcov(m))[2])
  lwr <- est - 1.64*se 
  upr <- est + 1.64*se
  lines(c(i + nudge, i + nudge), c(lwr, upr), col = "red")
  points(i + nudge, est, cex = .8, col = "red", pch = 21, bg = "white")
  
}

dev.off()