rm(list = ls())

setwd("~/Dropbox/Modeling Self-Reported Vote Choice")


pdf("Figures/incumbency_rep.pdf", height = 3, width = 9)
library(R2jags)
library(foreign)
library(arm)

house.exit <- read.dta("Data/US_92_exit_polls.dta")
nes92 <- read.dta("Data/Carlisle_1992_ANES_supplemented.dta")

nes92$Rep_win <- 1*(nes92$rv > nes92$dv)
nes92$Dem_win <- 1*(nes92$rv < nes92$dv)
nes92$Rep_time <- 1*(nes92$rv > nes92$dv)*nes92$post_int_days
nes92$Dem_time <- 1*(nes92$rv < nes92$dv)*nes92$post_int_days

par(oma = c(1,1,1,1), mar = c(3, 1, 3, 1), mfrow = c(1,2))
plot(NULL, xlim = c(-.3, .3), ylim = c(3.5, 8), xlab = NA, ylab = NA, axes = F)
axis(side = 1, cex.axis = .9)
mtext(side = 1, "Pr(Vote Rep | Rep Inc) - Pr(Vote Rep | No Inc)", line = 2, cex = .9)
mtext(side = 3, "NES Data", line = 2)




## Exit Polls
m1 <- glm(Rep_House_vote ~ Rep_pid + Dem_pid + Rep_House_inc + Dem_House_inc + 
  Bush_vote + Clinton_vote, family = binomial, data = house.exit)
sims1 <- coef(sim(m1, 1000))
x.clinton <- c(1, 1, 0, 0, 0, 1, 0)
sims.clinton <- plogis(sims1%*%x.clinton)
x.bush <-    c(1, 1, 0, 1, 0, 1, 0)
sims.bush <- plogis(sims1%*%x.bush)
sims.dif <- sims.bush - sims.clinton
q.clinton <- quantile(sims.clinton, c(.05, .5, .95))
q.bush <- quantile(sims.bush, c(.05, .5, .95))
q.dif <- quantile(sims.dif, c(.05, .5, .95))

lines(c(q.dif[1], q.dif[3]), c(4,4))
points(q.dif[2], 4, pch = 19)
text(q.dif[2], 4, "Exit Poll Estimate", cex = .7, pos = 3)


## Naive Approach
m1 <- glm(Rep_House_vote ~ Rep_pid + Dem_pid + Rep_House_inc + Dem_House_inc + 
  Bush_vote + Clinton_vote, family = binomial, data = nes92)
sims1 <- coef(sim(m1, 1000))
x.clinton <- c(1, 1, 0, 0, 0, 1, 0)
sims.clinton <- plogis(sims1%*%x.clinton)
x.bush <- c(1, 1, 0, 1, 0, 1, 0)
sims.bush <- plogis(sims1%*%x.bush)
sims.dif <- sims.bush - sims.clinton
q.clinton <- quantile(sims.clinton, c(.05, .5, .95))
q.bush <- quantile(sims.bush, c(.05, .5, .95))
q.dif <- quantile(sims.dif, c(.05, .5, .95))

lines(c(q.dif[1], q.dif[3]), c(5,5))
points(q.dif[2], 5, pch = 19)
text(q.dif[2], 5, "Naive Estimate", cex = .7, pos = 3)

## Simple Approach
m1 <- glm(Rep_House_vote ~ Rep_pid + Dem_pid + Rep_House_inc + Dem_House_inc + 
  Bush_vote + Clinton_vote + Rep_time + Dem_time,
  family = binomial, data = nes92)
sims1 <- coef(sim(m1))
x.clinton <- c(1, 1, 0, 0, 0, 1, 0, 0, 0)
sims.clinton <- plogis(sims1%*%x.clinton)
x.bush <-    c(1, 1, 0, 1, 0, 1, 0, 0, 0)
sims.bush <- plogis(sims1%*%x.bush)
sims.dif <- sims.bush - sims.clinton
q.clinton <- quantile(sims.clinton, c(.05, .5, .95))
q.bush <- quantile(sims.bush, c(.05, .5, .95))
q.dif <- quantile(sims.dif, c(.05, .5, .95))

lines(c(q.dif[1], q.dif[3]), c(6,6))
points(q.dif[2], 6, pch = 19)
text(q.dif[2], 6, "Simple Estimate", cex = .7, pos = 3)

## Interactive Approach
m1 <- glm(Rep_House_vote ~ Rep_pid + Dem_pid + Rep_House_inc + Dem_House_inc + 
  Bush_vote + Clinton_vote +
  Rep_time + Dem_time +
  Bush_vote:Rep_time + Bush_vote:Dem_time + 
  Clinton_vote:Rep_time +Clinton_vote:Dem_time,
  family = binomial, data = nes92)
sims1 <- coef(sim(m1))
x.clinton <- c(1, 1, 0, 0, 0, 1, 0, 0,0,0,0,0,0)
sims.clinton <- plogis(sims1%*%x.clinton)
x.bush <-    c(1, 1, 0, 1, 0, 1, 0, 0,0,0,0,0,0)
sims.bush <- plogis(sims1%*%x.bush)
sims.dif <- sims.bush - sims.clinton
q.clinton <- quantile(sims.clinton, c(.05, .5, .95))
q.bush <- quantile(sims.bush, c(.05, .5, .95))
q.dif <- quantile(sims.dif, c(.05, .5, .95))

lines(c(q.dif[1], q.dif[3]), c(7,7))
points(q.dif[2], 7, pch = 19)
text(q.dif[2], 7, "Interactive Estimate", cex = .7, pos = 3)

model.vars <- c("Rep_House_vote",
               "Rep_pid",
               "Dem_pid",
               "Rep_House_inc",
               "Dem_House_inc",
               "Bush_vote",
               "Clinton_vote",
               "Rep_time",
               "Dem_time"
  )

nes92 <- na.omit(nes92[, model.vars])

nes92$id <- with(nes92, paste(Rep_pid,
               Dem_pid,
               Rep_House_inc,
               Dem_House_inc,
               Bush_vote,
               Clinton_vote,
               Rep_time,
               Dem_time, sep="/"))

d.compact <- NULL
for (id in unique(nes92$id)) {
  print(id)
  d.temp <- nes92[nes92$id == id, ]
  d.temp$n.trials <- nrow(d.temp)
  d.temp[1, "Rep_House_vote"] <- sum(d.temp[,1])  
  d.temp <- d.temp[1, ]
  d.compact <- rbind(d.compact, d.temp)
}


# Set up objects for jags call
m.data <- with(d.compact, list(
               n = nrow(d.compact),
               Rep_House_vote = Rep_House_vote,
               Rep_pid = Rep_pid,
               Dem_pid = Dem_pid,
               Rep_House_inc = Rep_House_inc,
               Dem_House_inc = Dem_House_inc,
               Bush_vote = Bush_vote,
               Clinton_vote = Clinton_vote,
               Rep_time = log(Rep_time),
               Dem_time = log(Dem_time),
               n.trials = n.trials
  ))
  
m.parameters <- c("beta0", "beta1", "beta2", "beta3",
                  "beta4", "beta5", "beta6",
                  "gamma0", "delta0", "pr0", "pr1", "fd")

m.inits <- function () {
  list (
    beta0 = rnorm(1),
    beta1 = rnorm(1),
    beta2 = rnorm(1),
    beta3 = rnorm(1),
    beta4 = rnorm(1),
    beta5 = rnorm(1),
    gamma0 = runif(1), #rnorm(1),
    delta0 = runif(1)) #rnorm(1))
  }

write(file = "model.bugs",
  "model {
    # INDIVIDUAL LEVEL MODEL
    for (i in 1:n)  {
      Rep_House_vote[i] ~ dbin(p.bound[i], n.trials[i])
      p.say.rep[i] <- p.honest[i]*p.vote.rep[i] + p.rep[i]
      p.bound[i] <- max(0, min(1, p.say.rep[i]))

      logit(p.vote.rep[i]) <- Xbeta[i]
      p.honest[i] <- 1 - p.dem[i] - p.rep[i]
      p.dem[i] <- 1/(1 + exp(-Zgamma[i]) + exp(-Zdelta[i]))
      p.rep[i] <- 1/(1 + exp(-Zgamma[i]) + exp(-Zdelta[i]))

      Xbeta[i] <- beta0 + beta1*Rep_pid[i] + beta2*Dem_pid[i] +
        beta3*Rep_House_inc[i] + beta4*Dem_House_inc[i] + 
        beta5*Bush_vote[i] + beta6*Clinton_vote[i]
      Zgamma[i] <- log(gamma0) + Rep_time[i]
      Zdelta[i] <- log(delta0) + Dem_time[i]      
    }
    logit(pr0) <- beta0 + beta1 + beta5
    logit(pr1) <- beta0 + beta1 + beta3 + beta5
    fd <- pr1 - pr0
    # PRIORS
    beta0 ~ dnorm(0, .01)
    beta1 ~ dnorm(0, .01)
    beta2 ~ dnorm(0, .01)
    beta3 ~ dnorm(0, .01)
    beta4 ~ dnorm(0, .01)
    beta5 ~ dnorm(0, .01)
    beta6 ~ dnorm(0, .01)
    gamma0 ~ dbeta(1.2, 1.2)
    delta0 ~ dbeta(1.2, 1.2)
  }"
)

  m <- jags(model.file = "model.bugs",
    data = m.data,
    inits = m.inits,
    parameters.to.save = m.parameters,
    n.chains = 3,
    n.iter = 5000,
    n.burnin = 2000)

attach.jags(m)
q.clinton <- quantile(pr0, c(.05, .5, .95))
q.bush <- quantile(pr1, c(.05, .5, .95))
q.dif <- quantile(fd, c(.05, .5, .95))

lines(c(q.dif[1], q.dif[3]), c(8,8))
points(q.dif[2], 8, pch = 19)
text(q.dif[2], 8, "Partial Observability Estimate", cex = .7, pos = 3, xpd = T)
detach.jags()



### Repeat for SES

rm(list = ls())

library(R2jags)
library(foreign)
library(arm)

house.exit <- read.dta("Data/Senate_appended_stacked_state_exit_polls_92.dta")
nes92 <- read.dta("Data/Carlisle_SES_Senate_92_merged_vote_choice.dta")

nes92$rv <- nes92$rep_sen_vote_prop
nes92$dv <- 1 - nes92$rep_sen_vote_prop
nes92$Bush_vote <- nes92$Rep_pres_vote_92
nes92$Clinton_vote <- nes92$Dem_pres_vote_92
nes92$Rep_House_inc <- nes92$rep_inc
nes92$Dem_House_inc <- nes92$dem_inc
house.exit$Rep_House_inc <- house.exit$rep_sen_inc
house.exit$Dem_House_inc <- house.exit$dem_sen_inc
house.exit$Rep_House_vote <- house.exit$rep_sen_vote

nes92$Rep_win <- 1*(nes92$rv > nes92$dv)
nes92$Dem_win <- 1*(nes92$rv < nes92$dv)
nes92$Rep_time <- 1*(nes92$rv > nes92$dv)*nes92$post_int_days
nes92$Dem_time <- 1*(nes92$rv < nes92$dv)*nes92$post_int_days

plot(NULL, xlim = c(-.3, .3), ylim = c(3.5, 8), xlab = NA, ylab = NA, axes = F)
axis(side = 1, cex.axis = .9)
mtext(side = 1, "Pr(Vote Rep | Rep Inc) - Pr(Vote Rep | No Inc)", line = 2, cex = .9)
mtext(side = 3, "SES Data", line = 2)




## Exit Polls
m1 <- glm(Rep_House_vote ~ Rep_pid + Dem_pid + Rep_House_inc + Dem_House_inc + 
  Bush_vote + Clinton_vote, family = binomial, data = house.exit)
sims1 <- coef(sim(m1, 1000))
x.clinton <- c(1, 1, 0, 0, 0, 1, 0)
sims.clinton <- plogis(sims1%*%x.clinton)
x.bush <- c(1, 1, 0, 1, 0, 1, 0)
sims.bush <- plogis(sims1%*%x.bush)
sims.dif <- sims.bush - sims.clinton
q.clinton <- quantile(sims.clinton, c(.05, .5, .95))
q.bush <- quantile(sims.bush, c(.05, .5, .95))
q.dif <- quantile(sims.dif, c(.05, .5, .95))

lines(c(q.dif[1], q.dif[3]), c(4,4))
points(q.dif[2], 4, pch = 19)
text(q.dif[2], 4, "Exit Poll Estimate", cex = .7, pos = 3)


## Naive Approach
m1 <- glm(Rep_House_vote ~ Rep_pid + Dem_pid + Rep_House_inc + Dem_House_inc + 
  Bush_vote + Clinton_vote, family = binomial, data = nes92)
sims1 <- coef(sim(m1, 1000))
x.clinton <- c(1, 1, 0, 0, 0, 1, 0)
sims.clinton <- plogis(sims1%*%x.clinton)
x.bush <- c(1, 1, 0, 1, 0, 1, 0)
sims.bush <- plogis(sims1%*%x.bush)
sims.dif <- sims.bush - sims.clinton
q.clinton <- quantile(sims.clinton, c(.05, .5, .95))
q.bush <- quantile(sims.bush, c(.05, .5, .95))
q.dif <- quantile(sims.dif, c(.05, .5, .95))

lines(c(q.dif[1], q.dif[3]), c(5,5))
points(q.dif[2], 5, pch = 19)
text(q.dif[2], 5, "Naive Estimate", cex = .7, pos = 3)

## Simple Approach
m1 <- glm(Rep_House_vote ~ Rep_pid + Dem_pid + Rep_House_inc + Dem_House_inc + 
  Bush_vote + Clinton_vote + Rep_time + Dem_time,
  family = binomial, data = nes92)
sims1 <- coef(sim(m1))
x.clinton <- c(1, 1, 0, 0, 0, 1, 0, 0, 0)
sims.clinton <- plogis(sims1%*%x.clinton)
x.bush <-    c(1, 1, 0, 1, 0, 1, 0, 0, 0)
sims.bush <- plogis(sims1%*%x.bush)
sims.dif <- sims.bush - sims.clinton
q.clinton <- quantile(sims.clinton, c(.05, .5, .95))
q.bush <- quantile(sims.bush, c(.05, .5, .95))
q.dif <- quantile(sims.dif, c(.05, .5, .95))

lines(c(q.dif[1], q.dif[3]), c(6,6))
points(q.dif[2], 6, pch = 19)
text(q.dif[2], 6, "Simple Estimate", cex = .7, pos = 3)

## Interactive Approach
m1 <- glm(Rep_House_vote ~ Rep_pid + Dem_pid + Rep_House_inc + Dem_House_inc + 
  Bush_vote + Clinton_vote +
  Rep_time + Dem_time +
  Bush_vote:Rep_time + Bush_vote:Dem_time + 
  Clinton_vote:Rep_time +Clinton_vote:Dem_time,
  family = binomial, data = nes92)
sims1 <- coef(sim(m1))
x.clinton <- c(1, 1, 0, 0, 0, 1, 0, 0,0,0,0,0,0)
sims.clinton <- plogis(sims1%*%x.clinton)
x.bush <-    c(1, 1, 0, 1, 0, 1, 0, 0,0,0,0,0,0)
sims.bush <- plogis(sims1%*%x.bush)
sims.dif <- sims.bush - sims.clinton
q.clinton <- quantile(sims.clinton, c(.05, .5, .95))
q.bush <- quantile(sims.bush, c(.05, .5, .95))
q.dif <- quantile(sims.dif, c(.05, .5, .95))

lines(c(q.dif[1], q.dif[3]), c(7,7))
points(q.dif[2], 7, pch = 19)
text(q.dif[2], 7, "Interactive Estimate", cex = .7, pos = 3)

model.vars <- c("Rep_House_vote",
               "Rep_pid",
               "Dem_pid",
               "Rep_House_inc",
               "Dem_House_inc",
               "Bush_vote",
               "Clinton_vote",
               "Rep_time",
               "Dem_time"
  )

nes92 <- na.omit(nes92[, model.vars])

nes92$id <- with(nes92, paste(Rep_pid,
               Dem_pid,
               Rep_House_inc,
               Dem_House_inc,
               Bush_vote,
               Clinton_vote,
               Rep_time,
               Dem_time, sep="/"))

d.compact <- NULL
for (id in unique(nes92$id)) {
  print(id)
  d.temp <- nes92[nes92$id == id, ]
  d.temp$n.trials <- nrow(d.temp)
  d.temp[1, "Rep_House_vote"] <- sum(d.temp[,1])  
  d.temp <- d.temp[1, ]
  d.compact <- rbind(d.compact, d.temp)
}


# Set up objects for jags call
m.data <- with(d.compact, list(
               n = nrow(d.compact),
               Rep_House_vote = Rep_House_vote,
               Rep_pid = Rep_pid,
               Dem_pid = Dem_pid,
               Rep_House_inc = Rep_House_inc,
               Dem_House_inc = Dem_House_inc,
               Bush_vote = Bush_vote,
               Clinton_vote = Clinton_vote,
               Rep_time = log(Rep_time),
               Dem_time = log(Dem_time),
               n.trials = n.trials
  ))
  
m.parameters <- c("beta0", "beta1", "beta2", "beta3",
                  "beta4", "beta5", "beta6",
                  "gamma0", "delta0", "pr0", "pr1", "fd")

m.inits <- function () {
  list (
    beta0 = rnorm(1),
    beta1 = rnorm(1),
    beta2 = rnorm(1),
    beta3 = rnorm(1),
    beta4 = rnorm(1),
    beta5 = rnorm(1),
    gamma0 = runif(1), #rnorm(1),
    delta0 = runif(1)) #rnorm(1))
  }

write(file = "model.bugs",
  "model {
    # INDIVIDUAL LEVEL MODEL
    for (i in 1:n)  {
      Rep_House_vote[i] ~ dbin(p.bound[i], n.trials[i])
      p.say.rep[i] <- p.honest[i]*p.vote.rep[i] + p.rep[i]
      p.bound[i] <- max(0, min(1, p.say.rep[i]))

      logit(p.vote.rep[i]) <- Xbeta[i]
      p.honest[i] <- 1 - p.dem[i] - p.rep[i]
      p.dem[i] <- 1/(1 + exp(-Zgamma[i]) + exp(-Zdelta[i]))
      p.rep[i] <- 1/(1 + exp(-Zgamma[i]) + exp(-Zdelta[i]))

      Xbeta[i] <- beta0 + beta1*Rep_pid[i] + beta2*Dem_pid[i] +
        beta3*Rep_House_inc[i] + beta4*Dem_House_inc[i] + 
        beta5*Bush_vote[i] + beta6*Clinton_vote[i]
      Zgamma[i] <- log(gamma0) + Rep_time[i]
      Zdelta[i] <- log(delta0) + Dem_time[i]      
    }
    logit(pr0) <- beta0 + beta1 + beta5
    logit(pr1) <- beta0 + beta1 + beta3 + beta5
    fd <- pr1 - pr0
    # PRIORS
    beta0 ~ dnorm(0, .01)
    beta1 ~ dnorm(0, .01)
    beta2 ~ dnorm(0, .01)
    beta3 ~ dnorm(0, .01)
    beta4 ~ dnorm(0, .01)
    beta5 ~ dnorm(0, .01)
    beta6 ~ dnorm(0, .01)
    gamma0 ~ dbeta(1.2, 1.2)
    delta0 ~ dbeta(1.2, 1.2)
  }"
)

  m <- jags(model.file = "model.bugs",
    data = m.data,
    inits = m.inits,
    parameters.to.save = m.parameters,
    n.chains = 3,
    n.iter = 5000,
    n.burnin = 2000)

attach.jags(m)
q.clinton <- quantile(pr0, c(.05, .5, .95))
q.bush <- quantile(pr1, c(.05, .5, .95))
q.dif <- quantile(fd, c(.05, .5, .95))

lines(c(q.dif[1], q.dif[3]), c(8,8))
points(q.dif[2], 8, pch = 19)
text(q.dif[2], 8, "Partial Observability Estimate", cex = .7, pos = 3, xpd = T)
detach.jags()
dev.off()





##########################################################





rm(list = ls())


library(R2jags)
library(foreign)
library(arm)

house.exit <- read.dta("Data/US_92_exit_polls.dta")
nes92 <- read.dta("Data/Carlisle_1992_ANES_supplemented.dta")

nes92$Rep_win <- 1*(nes92$rv > nes92$dv)
nes92$Dem_win <- 1*(nes92$rv < nes92$dv)
nes92$Rep_time <- 1*(nes92$rv > nes92$dv)*nes92$post_int_days
nes92$Dem_time <- 1*(nes92$rv < nes92$dv)*nes92$post_int_days

pdf("Figures/incumbency_dem.pdf", height = 3, width = 9)
par(oma = c(1,1,1,1), mar = c(3, 1, 3, 1), mfrow = c(1,2))
plot(NULL, xlim = c(-.3, .3), ylim = c(3.5, 8), xlab = NA, ylab = NA, axes = F)
axis(side = 1, cex.axis = .9)
mtext(side = 1, "Pr(Vote Rep | Dem Inc) - Pr(Vote Rep | No Inc)", line = 2, cex = .9)
mtext(side = 3, "NES Data", line = 2)




## Exit Polls
m1 <- glm(Rep_House_vote ~ Rep_pid + Dem_pid + Rep_House_inc + Dem_House_inc + 
  Bush_vote + Clinton_vote, family = binomial, data = house.exit)
sims1 <- coef(sim(m1, 1000))
x.clinton <- c(1, 1, 0, 0, 0, 1, 0)
sims.clinton <- plogis(sims1%*%x.clinton)
x.bush <-    c(1, 1, 0, 0, 1, 1, 0)
sims.bush <- plogis(sims1%*%x.bush)
sims.dif <- sims.bush - sims.clinton
q.clinton <- quantile(sims.clinton, c(.05, .5, .95))
q.bush <- quantile(sims.bush, c(.05, .5, .95))
q.dif <- quantile(sims.dif, c(.05, .5, .95))

lines(c(q.dif[1], q.dif[3]), c(4,4))
points(q.dif[2], 4, pch = 19)
text(q.dif[2], 4, "Exit Poll Estimate", cex = .7, pos = 3)


## Naive Approach
m1 <- glm(Rep_House_vote ~ Rep_pid + Dem_pid + Rep_House_inc + Dem_House_inc + 
  Bush_vote + Clinton_vote, family = binomial, data = nes92)
sims1 <- coef(sim(m1, 1000))
x.clinton <- c(1, 1, 0, 0, 0, 1, 0)
sims.clinton <- plogis(sims1%*%x.clinton)
x.bush <- c(1, 1, 0, 0, 1, 1, 0)
sims.bush <- plogis(sims1%*%x.bush)
sims.dif <- sims.bush - sims.clinton
q.clinton <- quantile(sims.clinton, c(.05, .5, .95))
q.bush <- quantile(sims.bush, c(.05, .5, .95))
q.dif <- quantile(sims.dif, c(.05, .5, .95))

lines(c(q.dif[1], q.dif[3]), c(5,5))
points(q.dif[2], 5, pch = 19)
text(q.dif[2], 5, "Naive Estimate", cex = .7, pos = 3)

## Simple Approach
m1 <- glm(Rep_House_vote ~ Rep_pid + Dem_pid + Rep_House_inc + Dem_House_inc + 
  Bush_vote + Clinton_vote + Rep_time + Dem_time,
  family = binomial, data = nes92)
sims1 <- coef(sim(m1))
x.clinton <- c(1, 1, 0, 0, 0, 1, 0, 0, 0)
sims.clinton <- plogis(sims1%*%x.clinton)
x.bush <-    c(1, 1, 0, 0, 1, 1, 0, 0, 0)
sims.bush <- plogis(sims1%*%x.bush)
sims.dif <- sims.bush - sims.clinton
q.clinton <- quantile(sims.clinton, c(.05, .5, .95))
q.bush <- quantile(sims.bush, c(.05, .5, .95))
q.dif <- quantile(sims.dif, c(.05, .5, .95))

lines(c(q.dif[1], q.dif[3]), c(6,6))
points(q.dif[2], 6, pch = 19)
text(q.dif[2], 6, "Simple Estimate", cex = .7, pos = 3)

## Interactive Approach
m1 <- glm(Rep_House_vote ~ Rep_pid + Dem_pid + Rep_House_inc + Dem_House_inc + 
  Bush_vote + Clinton_vote +
  Rep_time + Dem_time +
  Bush_vote:Rep_time + Bush_vote:Dem_time + 
  Clinton_vote:Rep_time +Clinton_vote:Dem_time,
  family = binomial, data = nes92)
sims1 <- coef(sim(m1))
x.clinton <- c(1, 1, 0, 0, 0, 1, 0, 0,0,0,0,0,0)
sims.clinton <- plogis(sims1%*%x.clinton)
x.bush <-    c(1, 1, 0, 0, 1, 1, 0, 0,0,0,0,0,0)
sims.bush <- plogis(sims1%*%x.bush)
sims.dif <- sims.bush - sims.clinton
q.clinton <- quantile(sims.clinton, c(.05, .5, .95))
q.bush <- quantile(sims.bush, c(.05, .5, .95))
q.dif <- quantile(sims.dif, c(.05, .5, .95))

lines(c(q.dif[1], q.dif[3]), c(7,7))
points(q.dif[2], 7, pch = 19)
text(q.dif[2], 7, "Interactive Estimate", cex = .7, pos = 3)

model.vars <- c("Rep_House_vote",
               "Rep_pid",
               "Dem_pid",
               "Rep_House_inc",
               "Dem_House_inc",
               "Bush_vote",
               "Clinton_vote",
               "Rep_time",
               "Dem_time"
  )

nes92 <- na.omit(nes92[, model.vars])

nes92$id <- with(nes92, paste(Rep_pid,
               Dem_pid,
               Rep_House_inc,
               Dem_House_inc,
               Bush_vote,
               Clinton_vote,
               Rep_time,
               Dem_time, sep="/"))

d.compact <- NULL
for (id in unique(nes92$id)) {
  print(id)
  d.temp <- nes92[nes92$id == id, ]
  d.temp$n.trials <- nrow(d.temp)
  d.temp[1, "Rep_House_vote"] <- sum(d.temp[,1])  
  d.temp <- d.temp[1, ]
  d.compact <- rbind(d.compact, d.temp)
}


# Set up objects for jags call
m.data <- with(d.compact, list(
               n = nrow(d.compact),
               Rep_House_vote = Rep_House_vote,
               Rep_pid = Rep_pid,
               Dem_pid = Dem_pid,
               Rep_House_inc = Rep_House_inc,
               Dem_House_inc = Dem_House_inc,
               Bush_vote = Bush_vote,
               Clinton_vote = Clinton_vote,
               Rep_time = log(Rep_time),
               Dem_time = log(Dem_time),
               n.trials = n.trials
  ))
  
m.parameters <- c("beta0", "beta1", "beta2", "beta3",
                  "beta4", "beta5", "beta6",
                  "gamma0", "delta0", "pr0", "pr1", "fd")

m.inits <- function () {
  list (
    beta0 = rnorm(1),
    beta1 = rnorm(1),
    beta2 = rnorm(1),
    beta3 = rnorm(1),
    beta4 = rnorm(1),
    beta5 = rnorm(1),
    gamma0 = runif(1), #rnorm(1),
    delta0 = runif(1)) #rnorm(1))
  }

write(file = "model.bugs",
  "model {
    # INDIVIDUAL LEVEL MODEL
    for (i in 1:n)  {
      Rep_House_vote[i] ~ dbin(p.bound[i], n.trials[i])
      p.say.rep[i] <- p.honest[i]*p.vote.rep[i] + p.rep[i]
      p.bound[i] <- max(0, min(1, p.say.rep[i]))

      logit(p.vote.rep[i]) <- Xbeta[i]
      p.honest[i] <- 1 - p.dem[i] - p.rep[i]
      p.dem[i] <- 1/(1 + exp(-Zgamma[i]) + exp(-Zdelta[i]))
      p.rep[i] <- 1/(1 + exp(-Zgamma[i]) + exp(-Zdelta[i]))

      Xbeta[i] <- beta0 + beta1*Rep_pid[i] + beta2*Dem_pid[i] +
        beta3*Rep_House_inc[i] + beta4*Dem_House_inc[i] + 
        beta5*Bush_vote[i] + beta6*Clinton_vote[i]
      Zgamma[i] <- log(gamma0) + Rep_time[i]
      Zdelta[i] <- log(delta0) + Dem_time[i]      
    }
    logit(pr0) <- beta0 + beta1 + beta5
    logit(pr1) <- beta0 + beta1 + beta4 + beta5
    fd <- pr1 - pr0
    # PRIORS
    beta0 ~ dnorm(0, .01)
    beta1 ~ dnorm(0, .01)
    beta2 ~ dnorm(0, .01)
    beta3 ~ dnorm(0, .01)
    beta4 ~ dnorm(0, .01)
    beta5 ~ dnorm(0, .01)
    beta6 ~ dnorm(0, .01)
    gamma0 ~ dbeta(1.2, 1.2)
    delta0 ~ dbeta(1.2, 1.2)
  }"
)

  m <- jags(model.file = "model.bugs",
    data = m.data,
    inits = m.inits,
    parameters.to.save = m.parameters,
    n.chains = 3,
    n.iter = 5000,
    n.burnin = 2000)

attach.jags(m)
q.clinton <- quantile(pr0, c(.05, .5, .95))
q.bush <- quantile(pr1, c(.05, .5, .95))
q.dif <- quantile(fd, c(.05, .5, .95))

lines(c(q.dif[1], q.dif[3]), c(8,8))
points(q.dif[2], 8, pch = 19)
text(q.dif[2], 8, "Partial Observability Estimate", cex = .7, pos = 3, xpd = T)
detach.jags()



### Repeat for SES

rm(list = ls())


library(R2jags)
library(foreign)
library(arm)

house.exit <- read.dta("Data/Senate_appended_stacked_state_exit_polls_92.dta")
nes92 <- read.dta("Data/Carlisle_SES_Senate_92_merged_vote_choice.dta")

nes92$rv <- nes92$rep_sen_vote_prop
nes92$dv <- 1 - nes92$rep_sen_vote_prop
nes92$Bush_vote <- nes92$Rep_pres_vote_92
nes92$Clinton_vote <- nes92$Dem_pres_vote_92
nes92$Rep_House_inc <- nes92$rep_inc
nes92$Dem_House_inc <- nes92$dem_inc
house.exit$Rep_House_inc <- house.exit$rep_sen_inc
house.exit$Dem_House_inc <- house.exit$dem_sen_inc
house.exit$Rep_House_vote <- house.exit$rep_sen_vote

nes92$Rep_win <- 1*(nes92$rv > nes92$dv)
nes92$Dem_win <- 1*(nes92$rv < nes92$dv)
nes92$Rep_time <- 1*(nes92$rv > nes92$dv)*nes92$post_int_days
nes92$Dem_time <- 1*(nes92$rv < nes92$dv)*nes92$post_int_days

plot(NULL, xlim = c(-.3, .3), ylim = c(3.5, 8), xlab = NA, ylab = NA, axes = F)
axis(side = 1, cex.axis = .9)
mtext(side = 1, "Pr(Vote Rep | Dem Inc) - Pr(Vote Rep | No Inc)", line = 2, cex = .9)
mtext(side = 3, "SES Data", line = 2)




## Exit Polls
m1 <- glm(Rep_House_vote ~ Rep_pid + Dem_pid + Rep_House_inc + Dem_House_inc + 
  Bush_vote + Clinton_vote, family = binomial, data = house.exit)
sims1 <- coef(sim(m1, 1000))
x.clinton <- c(1, 1, 0, 0, 0, 1, 0)
sims.clinton <- plogis(sims1%*%x.clinton)
x.bush <- c(1, 1, 0, 0, 1, 1, 0)
sims.bush <- plogis(sims1%*%x.bush)
sims.dif <- sims.bush - sims.clinton
q.clinton <- quantile(sims.clinton, c(.05, .5, .95))
q.bush <- quantile(sims.bush, c(.05, .5, .95))
q.dif <- quantile(sims.dif, c(.05, .5, .95))

lines(c(q.dif[1], q.dif[3]), c(4,4))
points(q.dif[2], 4, pch = 19)
text(q.dif[2], 4, "Exit Poll Estimate", cex = .7, pos = 3)




## Naive Approach
m1 <- glm(Rep_House_vote ~ Rep_pid + Dem_pid + Rep_House_inc + Dem_House_inc + 
  Bush_vote + Clinton_vote, family = binomial, data = nes92)
sims1 <- coef(sim(m1, 1000))
x.clinton <- c(1, 1, 0, 0, 0, 1, 0)
sims.clinton <- plogis(sims1%*%x.clinton)
x.bush <- c(1, 1, 0, 0, 1, 1, 0)
sims.bush <- plogis(sims1%*%x.bush)
sims.dif <- sims.bush - sims.clinton
q.clinton <- quantile(sims.clinton, c(.05, .5, .95))
q.bush <- quantile(sims.bush, c(.05, .5, .95))
q.dif <- quantile(sims.dif, c(.05, .5, .95))

lines(c(q.dif[1], q.dif[3]), c(5,5))
points(q.dif[2], 5, pch = 19)
text(q.dif[2], 5, "Naive Estimate", cex = .7, pos = 3)

## Simple Approach
m1 <- glm(Rep_House_vote ~ Rep_pid + Dem_pid + Rep_House_inc + Dem_House_inc + 
  Bush_vote + Clinton_vote + Rep_time + Dem_time,
  family = binomial, data = nes92)
sims1 <- coef(sim(m1))
x.clinton <- c(1, 1, 0, 0, 0, 1, 0, 0, 0)
sims.clinton <- plogis(sims1%*%x.clinton)
x.bush <-    c(1, 1, 0, 0, 1, 1, 0, 0, 0)
sims.bush <- plogis(sims1%*%x.bush)
sims.dif <- sims.bush - sims.clinton
q.clinton <- quantile(sims.clinton, c(.05, .5, .95))
q.bush <- quantile(sims.bush, c(.05, .5, .95))
q.dif <- quantile(sims.dif, c(.05, .5, .95))

lines(c(q.dif[1], q.dif[3]), c(6,6))
points(q.dif[2], 6, pch = 19)
text(q.dif[2], 6, "Simple Estimate", cex = .7, pos = 3)

## Interactive Approach
m1 <- glm(Rep_House_vote ~ Rep_pid + Dem_pid + Rep_House_inc + Dem_House_inc + 
  Bush_vote + Clinton_vote +
  Rep_time + Dem_time +
  Bush_vote:Rep_time + Bush_vote:Dem_time + 
  Clinton_vote:Rep_time +Clinton_vote:Dem_time,
  family = binomial, data = nes92)
sims1 <- coef(sim(m1))
x.clinton <- c(1, 1, 0, 0, 0, 1, 0, 0,0,0,0,0,0)
sims.clinton <- plogis(sims1%*%x.clinton)
x.bush <-    c(1, 1, 0, 0, 1, 1, 0, 0,0,0,0,0,0)
sims.bush <- plogis(sims1%*%x.bush)
sims.dif <- sims.bush - sims.clinton
q.clinton <- quantile(sims.clinton, c(.05, .5, .95))
q.bush <- quantile(sims.bush, c(.05, .5, .95))
q.dif <- quantile(sims.dif, c(.05, .5, .95))

lines(c(q.dif[1], q.dif[3]), c(7,7))
points(q.dif[2], 7, pch = 19)
text(q.dif[2], 7, "Interactive Estimate", cex = .7, pos = 3)

model.vars <- c("Rep_House_vote",
               "Rep_pid",
               "Dem_pid",
               "Rep_House_inc",
               "Dem_House_inc",
               "Bush_vote",
               "Clinton_vote",
               "Rep_time",
               "Dem_time"
  )

nes92 <- na.omit(nes92[, model.vars])

nes92$id <- with(nes92, paste(Rep_pid,
               Dem_pid,
               Rep_House_inc,
               Dem_House_inc,
               Bush_vote,
               Clinton_vote,
               Rep_time,
               Dem_time, sep="/"))

d.compact <- NULL
for (id in unique(nes92$id)) {
  print(id)
  d.temp <- nes92[nes92$id == id, ]
  d.temp$n.trials <- nrow(d.temp)
  d.temp[1, "Rep_House_vote"] <- sum(d.temp[,1])  
  d.temp <- d.temp[1, ]
  d.compact <- rbind(d.compact, d.temp)
}


# Set up objects for jags call
m.data <- with(d.compact, list(
               n = nrow(d.compact),
               Rep_House_vote = Rep_House_vote,
               Rep_pid = Rep_pid,
               Dem_pid = Dem_pid,
               Rep_House_inc = Rep_House_inc,
               Dem_House_inc = Dem_House_inc,
               Bush_vote = Bush_vote,
               Clinton_vote = Clinton_vote,
               Rep_time = log(Rep_time),
               Dem_time = log(Dem_time),
               n.trials = n.trials
  ))
  
m.parameters <- c("beta0", "beta1", "beta2", "beta3",
                  "beta4", "beta5", "beta6",
                  "gamma0", "delta0", "pr0", "pr1", "fd")

m.inits <- function () {
  list (
    beta0 = rnorm(1),
    beta1 = rnorm(1),
    beta2 = rnorm(1),
    beta3 = rnorm(1),
    beta4 = rnorm(1),
    beta5 = rnorm(1),
    gamma0 = runif(1), #rnorm(1),
    delta0 = runif(1)) #rnorm(1))
  }

write(file = "model.bugs",
  "model {
    # INDIVIDUAL LEVEL MODEL
    for (i in 1:n)  {
      Rep_House_vote[i] ~ dbin(p.bound[i], n.trials[i])
      p.say.rep[i] <- p.honest[i]*p.vote.rep[i] + p.rep[i]
      p.bound[i] <- max(0, min(1, p.say.rep[i]))

      logit(p.vote.rep[i]) <- Xbeta[i]
      p.honest[i] <- 1 - p.dem[i] - p.rep[i]
      p.dem[i] <- 1/(1 + exp(-Zgamma[i]) + exp(-Zdelta[i]))
      p.rep[i] <- 1/(1 + exp(-Zgamma[i]) + exp(-Zdelta[i]))

      Xbeta[i] <- beta0 + beta1*Rep_pid[i] + beta2*Dem_pid[i] +
        beta3*Rep_House_inc[i] + beta4*Dem_House_inc[i] + 
        beta5*Bush_vote[i] + beta6*Clinton_vote[i]
      Zgamma[i] <- log(gamma0) + Rep_time[i]
      Zdelta[i] <- log(delta0) + Dem_time[i]      
    }
    logit(pr0) <- beta0 + beta1 + beta5
    logit(pr1) <- beta0 + beta1 + beta4 + beta5
    fd <- pr1 - pr0
    # PRIORS
    beta0 ~ dnorm(0, .01)
    beta1 ~ dnorm(0, .01)
    beta2 ~ dnorm(0, .01)
    beta3 ~ dnorm(0, .01)
    beta4 ~ dnorm(0, .01)
    beta5 ~ dnorm(0, .01)
    beta6 ~ dnorm(0, .01)
    gamma0 ~ dbeta(1.2, 1.2)
    delta0 ~ dbeta(1.2, 1.2)
  }"
)

  m <- jags(model.file = "model.bugs",
    data = m.data,
    inits = m.inits,
    parameters.to.save = m.parameters,
    n.chains = 3,
    n.iter = 5000,
    n.burnin = 2000)

attach.jags(m)
q.clinton <- quantile(pr0, c(.05, .5, .95))
q.bush <- quantile(pr1, c(.05, .5, .95))
q.dif <- quantile(fd, c(.05, .5, .95))

lines(c(q.dif[1], q.dif[3]), c(8,8))
points(q.dif[2], 8, pch = 19)
text(q.dif[2], 8, "Partial Observability Estimate", cex = .7, pos = 3, xpd = T)
detach.jags()
dev.off()