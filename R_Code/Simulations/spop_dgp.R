setwd("~/Dropbox/Projects/Misreports)

n.iter <- 400

res <- array(NA, dim = c(n.iter, 4, 3))

for (i in 1:n.iter) {
print(i)

library(arm)

n <- 3000
x1 <- rbinom(n, 1, .5)
#x2 <- rnorm(n)
x3.star <- rnorm(n)
x3 <- rbinom(n, 1, pnorm(x3.star))
t <- floor(runif(n , 1, 5)*10)
rep.win <- rbinom(n, 1, pnorm(.5*x3.star))
dem.win <- 1 - rep.win

p.vote.rep <- plogis(0 + .2*x1 + .4*x3)
Zgamma <- log(.02) + log(t*rep.win)
Zdelta <- log(.02) + log(t*dem.win)
alpha <- 1      

p.dem <- exp(Zgamma)/(1 + exp(Zgamma) + exp(Zdelta))
p.rep <- exp(Zdelta)/(1 + exp(Zgamma) + exp(Zdelta))
p.honest <- 1 - p.dem - p.rep

p.say.rep <- p.honest*p.vote.rep + p.rep

true.fd <- plogis(.4) - plogis(0)

par(mfrow = c(2,2))
plot(t, p.say.rep)
plot(t, p.rep)
plot(t, p.dem)
plot(t, p.honest)


#  p.say.rep.given.vote.dem*(1 - p.vote.rep)

y <- rbinom(n, 1, p.say.rep)
t.dem <- log(t*dem.win)
t.rep <- log(t*rep.win)

d <- data.frame(y, x1, x3, t.dem, t.rep)

d$id <- paste(x1, x3, t.dem, t.rep, sep="/")

d.compact <- NULL
for (id in unique(d$id)) {
  print(id)
  d.temp <- d[d$id == id, ]
  d.temp$n.trials <- nrow(d.temp)
  d.temp[1, "y"] <- sum(d.temp[,1])  
  d.temp <- d.temp[1, ]
  d.compact <- rbind(d.compact, d.temp)
}

# Set up objects for jags call
m.data <- with(d.compact, list(
               n = nrow(d.compact),
               y = y,
               x1 = x1, 
               x3 = x3,
               t.dem = t.dem,
               t.rep = t.rep,
               n.trials = n.trials
  ))
  
m.parameters <- c("beta0", "beta1", "beta3",
                  "gamma0",
                  "delta0", "fd", "pr0", "pr1")

m.inits <- function () {
  list (
    beta0 = rnorm(1),
    beta1 = rnorm(1),
    beta3 =  rnorm(1),
    gamma0 = runif(1), #rnorm(1),
    delta0 = runif(1))
  }

write(file = "BUGS/model_ex.bugs",
  "model {
    # INDIVIDUAL LEVEL MODEL
    for (i in 1:n)  {
      y[i] ~ dbin(p.bound[i], n.trials[i])
      p.bound[i] <- max(0, min(1, p.say.rep[i]))
      p.say.rep[i] <- p.honest[i]*p.vote.rep[i] + p.rep[i]
      
      logit(p.vote.rep[i]) <- Xbeta[i]
      p.honest[i] <- 1 - p.dem[i] - p.rep[i]
      p.dem[i] <- exp(Zgamma[i])/(1 + exp(Zgamma[i]) + exp(Zdelta[i]))
      p.rep[i] <- exp(Zdelta[i])/(1 + exp(Zgamma[i]) + exp(Zdelta[i]))

      Xbeta[i] <- beta0 + beta1*x1[i] + beta3*x3[i]
      Zgamma[i] <- log(gamma0) + t.rep[i]
      Zdelta[i] <- log(delta0) + t.dem[i]      
    }
    logit(pr0) <- beta0
    logit(pr1) <- beta0 + beta3
    fd <- pr1 - pr0  
    # PRIORS
    beta0 ~ dnorm(0, .001)
    beta1 ~ dnorm(0, .001)
    beta3 ~ dnorm(0, .001)
    gamma0 ~ dbeta(1.2, 1.2)
    delta0 ~ dbeta(1.2, 1.2)
  }"
)

library(R2jags)
  m.bayes <- jags(model.file = "BUGS/model_ex.bugs",
    data = m.data,
    inits = m.inits,
    parameters.to.save = m.parameters,
    n.chains = 1,
    n.iter = 1000,
    n.burnin = 500,
    n.thin = 1)
#print(m)

d$t.rep <- exp(d$t.rep)
d$t.dem <- exp(d$t.dem)

m.naive <- glm(y ~ x1 + x3, family = binomial, data = d)
sim.naive <- coef(sim(m.naive, 1000))
fd.naive <- plogis(c(1,0,1)%*%t(sim.naive)) - plogis(c(1,0,0)%*%t(sim.naive))
m.simple <- glm(y ~ x1 + x3 + t.rep + t.dem, family = binomial, data = d)
sim.simple <- coef(sim(m.simple, 1000))
fd.simple <- plogis(c(1,0,1,0,0)%*%t(sim.simple)) - plogis(c(1,0,0,0,0)%*%t(sim.simple))
m.int <- glm(y ~ x1 + x3*t.rep + x3*t.dem, family = binomial, data = d)
sim.int <- coef(sim(m.int, 1000))
fd.int <- plogis(c(1,0,1,0,0,0,0)%*%t(sim.int)) - plogis(c(1,0,0,0,0,0,0)%*%t(sim.int))
fd.bayes <- as.vector(m.bayes$BUGSoutput$sims.array[,,"fd"])

res[i, 1, ] <- quantile(fd.naive, c(.05, .5, .95))
res[i, 2, ] <- quantile(fd.simple, c(.05, .5, .95))
res[i, 3, ] <- quantile(fd.int, c(.05, .5, .95))
res[i, 4, ] <- quantile(fd.bayes, c(.05, .5, .95))
}

model.names <- c("Naive Model",
                 "Simple Model",
                 "Interaction Model",
                 "Split-Population Model")

par(mfrow = c(1, 4), oma = c(1,1,1,1), mar = c(4,1,1,1),
  family = "serif")
for (k in 1:4) {
plot(NULL, xlim = c(-.1, .3), ylim = c(0, n.iter), 
  xlab = NA, ylab = NA, axes = F)
axis(side = 1)
mtext(side = 1, "Estimated Effect", line = 2.5, cex = .7)
mtext(side = 3, model.names[k], line = .2, cex = .7)
abline(v = true.fd, col = "grey80")
for(i in 1:n.iter) {
   lines(c(res[i,k,1], res[i,k,3]), c(i,i))
   points(res[i,k,2], i, pch = 19, cex = .1)
 }
box()

}

label.at <- c(0.01221137, 0.07162902, 0.08695908, 0.10505285)
par(mfrow = c(1,1), oma = c(0,0,0,0), mar = c(5,5,2,10))
plot(NULL, xlim = c(0, n.iter), ylim = c(-.05, .2),
     axes = F, xlab = NA, ylab = NA,xaxs = "i", yaxs = "i")
axis(side = 1)
mtext(side = 1, "Simulations", line = 3)
axis(side = 2, las = 1)
mtext(side = 2, "Estimated Effect", line = 4)
abline(h = true.fd, col = "grey50")

for(i in 1:4) {
  lines(1:n.iter, res[,i,2], col = i)
  text(50, label.at[i], model.names[i], pos = 4, col = i, cex = .7,
       xpd = T)
}

d.naive <- density(res[, 1, 2])
d.simple <- density(res[, 2, 2])
d.interaction <- density(res[, 3, 2])
d.pobs <- density(res[, 4, 2])

mean(res[, 3, 2])
sd(res[, 3, 2])

library(compactr)

pdf("Figures/sims_pobs_correct.pdf", height = 1.5, width = 8)
par(mfrow = c(1, 4), oma = c(3,3,1,1), mar = rep(.5, 4))
eplot(xlim = mm(c(d.naive$x, d.simple$x, d.interaction$x, d.pobs$x)),
      ylim = mm(c(d.naive$y, d.simple$y, d.interaction$y, d.pobs$y)),
      xlab = "Estimated Effect",
      ylab = "Density", ylabpos = 1.7,
      main = "Naive Model")
lines(d.naive)
abline(v = true.fd)
aplot("Simple Model")
lines(d.simple)
abline(v = true.fd)
aplot("Interaction Model")
lines(d.interaction)
abline(v = true.fd)
aplot("Partial Observability Model")
lines(d.pobs)
abline(v = true.fd)
dev.off()