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

pdf("Figures/interview_date.pdf", height = 4.5, width = 4.5)

par(mfrow = c(3,1), mar = rep(0.75, 4), oma = c(3,4,1,1))
nudge <- .4

# Senate
eplot(xlim = c(1988, 2008), ylim = c(-.15, .15),
      xlab = "Year", 
      ylab = "Effect of Time\non Misreporting", ylabpos = 2.3,
      main = "Senate Elections",
      #yat = c(-0.05, 0.00, 0.05, 0.1),
      xat = 1988 + c(0:8*2, 20))
abline(h = 0, lty = 2, col = "grey")
# loop over NES data
for (i in 1988 + c(0:8*2, 20)) {
  m <- lm(Rep_Sen_vote_dif ~ rep_sen_vote_prop + open_seat + open_seat_x_rvp + 
            post_int_days + post_int_days_x_rvp, data = nes.senate, subset = year == i,
          weights = weight)
  est <- coef(m)[6]
  se <- sqrt(diag(vcov(m))[6])
  lwr <- est - 1.64*se 
  upr <- est + 1.64*se
  points(i, est, pch = 19, cex = .8)
  lines(c(i, i), c(lwr, upr))
}
# loop over SES data
for (i in c(1988, 1990, 1992)) {
  m <- lm(Rep_sen_vote_diff ~ rep_vote_prop + open_seat + open_seat_x_rvp + 
            post_int_days + post_int_days_x_rvp, data = ses.senate, subset = year == i,
          weights = weight)
  est <- coef(m)[6]
  se <- sqrt(diag(vcov(m))[6])
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
  m <- lm(Rep_House_vote_dif ~ rvprop + House_open_seat + House_open_seat_x_rvp + 
            post_int_days + post_int_days_x_rvp, data = nes.house, subset = year == i,
          weights = weight)
  est <- coef(m)[6]
  se <- sqrt(diag(vcov(m))[6])
  lwr <- est - 1.64*se 
  upr <- est + 1.64*se
  points(i, est, pch = 19, cex = .8)
  lines(c(i, i), c(lwr, upr))
}
# loop over SES data
for (i in c(1988, 1990, 1992)) {
  m <- lm(Rep_House_vote_dif ~ rvprop + open_seat_House_b + open_seat_House_b_x_rvp + 
            post_int_days + post_int_days_x_rvp, data = ses.house, subset = year == i,
          weights = weight)
  est <- coef(m)[6]
  se <- sqrt(diag(vcov(m))[6])
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
  m <- lm(Rep_House_vote_dif ~ rvprop + House_open_seat + House_open_seat_x_rvp + 
            post_int_days + post_int_days_x_rvp, data = nes.house, 
          subset = year == i & rvprop > .2 & rvprop < .8,
          weights = weight)
  est <- coef(m)[6]
  se <- sqrt(diag(vcov(m))[6])
  lwr <- est - 1.64*se 
  upr <- est + 1.64*se
  points(i, est, pch = 19, cex = .8)
  lines(c(i, i), c(lwr, upr))
}
# loop over SES data
for (i in c(1988, 1990, 1992)) {
  m <- lm(Rep_House_vote_dif ~ rvprop + open_seat_House_b + open_seat_House_b_x_rvp + 
            post_int_days + post_int_days_x_rvp, data = ses.house, 
          subset = year == i & rvprop > .2 & rvprop < .8,
          weights = weight)
  est <- coef(m)[6]
  se <- sqrt(diag(vcov(m))[6])
  lwr <- est - 1.64*se 
  upr <- est + 1.64*se
  lines(c(i + nudge, i + nudge), c(lwr, upr), col = "red")
  points(i + nudge, est, cex = .8, col = "red", pch = 21, bg = "white")
  
}
dev.off()