library(tidyverse)
library(dslabs)
data(height)

str(heights)

x <- seq(-4, 4, length = 100)
data.frame(x, f = dnorm(x)) %>% 
  ggplot(aes(x = x, y = f)) + 
  geom_line()

# Monte Carlo simulation
# How common are 7-footers?
# Generate 10,000 samples of 800 heights and save the tallest of each simulation
x <- heights[heights$sex == "Male",] %>% pull(height)

tallest <- replicate(1E4, {
  simulation <- rnorm(800,
                      mean(x), 
                      sd(x))
  return(max(simulation))
})

mean(tallest >= 7*12)

data.frame(height = tallest) %>% 
  ggplot(aes(x = height)) +
  geom_histogram(binwidth = 2)

data.frame(dat = rnorm(800,
      mean(x), 
      sd(x))) %>% 
  ggplot(aes(x = dat)) +
  geom_histogram(binwidth = 1)

# ACT Scores
set.seed(16)

act.scores <- rnorm(1E4, 20.9, 5.7)
mean(act.scores)
sd(act.scores)

sum(act.scores == 36.0)
hist(act.scores)

sum(act.scores >= 36)

# probability of an ACT score > 30
sum(act.scores > 30) / length(act.scores)

# probability of an ACT score <= 10
sum(act.scores <= 10) / length(act.scores)

x <- 1:36
f_x <- dnorm(x, 20.9, 5.7)
lines(x, f_x)

# convert raw ACT scores into Z-scores
# subtract the mean and divide by sd
act.z <- (act.scores - mean(act.scores)) / sd(act.scores) 

# probability of a z-score greater than 2 (sd's above the mean)?
sum(act.z > 2) / length(act.scores)

# ACT score at 2 sd's above the mean (z = 2)?
mean(act.scores) + (2 * sd(act.scores))

qnorm(0.975, mean = mean(act.scores), sd = sd(act.scores))

qnorm(p = 0.95, mean = 20.9, sd = 5.7)

sample_quantiles <- quantile(act.scores, seq(0.01, 0.99, 0.01))
sample_quantiles

# Monte Carlo

# sampling model 1: define urn, then sample
color <- rep(c("Black", "Red", "Green"), c(18, 18, 2)) # define the urn for the sampling model
n <- 1000
X <- sample(ifelse(color == "Red", -1, 1), n, replace = TRUE)
X[1:10]

# sampling model 2: define urn inside sample function by noting probabilities
x <- sample(c(-1, 1), n, replace = TRUE, prob = c(9/19, 10/19))    # 1000 independent draws
S <- sum(x)    # total winnings = sum of draws
S

n <- 1000    # number of roulette players
B <- 10000    # number of Monte Carlo experiments
S <- replicate(B, {
  X <- sample(c(-1,1), n, replace = TRUE, prob = c(9/19, 10/19))    # simulate 1000 spins
  sum(X)    # determine total profit
})

mean(S < 0)    # probability of the casino losing money

s <- seq(min(S), max(S), length = 100)    # sequence of 100 values across range of S
normal_density <- data.frame(s = s, f = dnorm(s, mean(S), sd(S))) # generate normal density for S
data.frame (S = S) %>%    # make data frame of S for histogram
  ggplot(aes(S, ..density..)) +
  geom_histogram(color = "black", binwidth = 10) +
  ylab("Probability") +
  geom_line(data = normal_density, mapping = aes(s, f), color = "blue")

# SAT
# An old version of the SAT college entrance exam had a -0.25 point penalty for 
# every incorrect answer and awarded 1 point for a correct answer. The
# quantitative test consisted of 44 multiple-choice questions each with 5 
# answer choices. Suppose a student chooses answers by guessing for all 
# questions on the test.

# E[X] of points for guessing on one question
(1 - 0.25 - 0.25 - 0.25 - 0.25) / 5

# E[X] for guessing on all 44 questions
ev <- (44) * ((1*0.2) + (-0.25 * 0.8))

# SE[X] of guessing on all 44 questions
se <- sqrt(44) * (abs(-0.25 - 1) * sqrt(0.8*0.2))

# Use Central Limit Theorem to determine probability that a guessing student
# scores 8 points or higher
1 - pnorm(q = 8, mean = ev, sd = se)

set.seed(21, sample.kind = "Rounding")

# Set the seed to 21, then run a Monte Carlo simulation of 
# 10,000 students guessing on the test.

mc <- replicate(1E5, {
  x <- sample(c(1, -0.25),
              prob = c(0.2, 0.8),
              size = 44, 
              replace = TRUE)
  return(sum(x))
})

hist(mc)

# probability that guessing student scores 8 or higher
mean(mc >= 8)

(44) * (1 * 0.25) + (0 * (1 - 0.75))

p <- seq(0.25, 0.95, 0.05)

scores.25 <- replicate(1E4, {
  x <- sample(c(0,1), 
                prob = c(0.75, 0.25),
                size = 44, 
                replace = TRUE)
    return(sum(x))
  })

scores.95 <- replicate(1E4, {
  x <- sample(c(0,1), 
              prob = c(0.05, 0.95),
              size = 44, 
              replace = TRUE)
  return(sum(x))
})

scores.05 <- replicate(1E4, {
  x <- sample(c(0,1), 
              prob = c(0.95, 0.05),
              size = 44, 
              replace = TRUE)
  return(sum(x))
})

mean(scores.95)

for (p in seq(0.25, 0.95, 0.05)){
  scores <- replicate(1E4, {
    x <- sample(c(0,1),
                prob = c(1 - p, p),
                replace = TRUE,
                size = 44)
    return(sum(x))
  })
  print(c(p, mean(scores > 35)))
}

# Roulette

# E[X] of the payout for one bet
win <- 6
loss <- -1
p.win <- 5 / 38
p.loss <- 33 / 38

ev <- (win * p.win) + (loss * p.loss)

# SE{X} of the payout for one bet
se <- abs(loss - win) * (sqrt(p.win * p.loss))

# expected value of the average payout over 500 bets
n <- 500

(n * ev) / n

# SE[average payout over 500 bets]
se / sqrt(n)

# E[sum of 500 bets]
ev.sum <- n * ev

# SE[sum of 500 bets]
se.sum <- sqrt(n) * se

# Pr(X <= 0) the probability of losing money over 500 bets
pnorm(0, mean = ev.sum, sd = se.sum)

# The Big Short
data("death_prob")
head(death_prob)

death_prob %>% 
  filter(age == "50" & sex == "Female") %>% 
  pull(prob)

# What is the expected value of the company's net profit on one policy for a 
# 50 year old female?

loss <- -150000.0
gain <- 1150.0
p_loss <- death_prob %>% 
  filter(age == "50" & sex == "Female") %>% 
  pull(prob)
p_gain <- 1 - p_loss

ev <- (loss * p_loss) + (gain * p_gain)
ev

se <- abs(loss - gain) * sqrt(p_loss * p_gain)
se

# E[X] over 1,000 policies for 50 y/o female
n <- 1E3
ev.sum <- n * ev
se.sum <- sqrt(n) * se

# Use the Central Limit Theorem to calculate the probability that the insurance
# company loses money on this set of 1,000 policies.
pnorm(0, ev.sum, se.sum)

# 2A
p.m.death <- death_prob %>%
  filter(age == 50 & sex == "Male") %>% 
  pull(prob)

a <- -150000
ev <- 700000
n <- 1000

b <- ( (ev / 1000) - (a * p.m.death) ) / (1 - p.m.death)
b

se.1000 <- sqrt(n) * ( abs(b - a) * sqrt(p.m.death * (1 - p.m.death)) )
se.1000

pnorm(0, 700000, se.1000)

# 3A
loss <- -150000.0
gain <- 1150.0
p.loss <- 0.015
p.gain <- 1 - p.loss
n <- 1000

ev <- n * ( (loss * p.loss) + (gain * p.gain) )
ev

se <- sqrt(n) * (abs(loss - gain) * sqrt(p.loss * p.gain))
se

# 3C probability of losing money
pnorm(0, ev, se)

# 3D probability of losing more than 1 million
pnorm(-1E6, ev, se)

# 3E
p <- seq(0.01, 0.03, 0.001)

ev <- function(p.loss, loss) {
  ev <- 1000 * ( (-150000.0 * p.loss) + (1150.0 * (1 - p.loss)) )
  se <- sqrt(1000) * (abs(-150000.0 - 1150.0) * sqrt(p.loss * (1 - p.loss) ))
  return(pnorm(loss, ev, se))
}

percs <- data.frame(p = p, chance.0 = sapply(p, ev, loss = 0))
percs

percs[percs$chance >= 0.9,]

p <- seq(0.01, 0.03, 0.0025)
percs <- data.frame(p = p, chance = sapply(p, ev, loss = -1E6))
percs

#4A 
# Define a sampling model for simulating the total profit over 1,000 loans 
# with probability of claim p_loss = .015, loss of -$150,000 on a claim, and 
# profit of $1,150 when there is no claim. Set the seed to 25, then run the model once.
set.seed(25, sample.kind = "Rounding")

x <- sample(c(-150000, 1150),
            size = 1000,
            replace = TRUE,
            prob = c(0.015, 1 - 0.015))

sum(x) / 1E6

# 4B
# Set the seed to 27, then run a Monte Carlo simulation of your sampling model 
# with 10,000 replicates to simulate the range of profits/losses over 1,000 
# loans.
# What is the observed probability of losing $1 million or more?
set.seed(27, sample.kind = "Rounding")

y <- function() {
  sample(c(-150000, 1150),
         size = 1000,
         replace = TRUE,
         prob = c(0.015, 1 - 0.015))
}

mc <- replicate(1E4, sum(y()))

sum(mc <= -1E6) / 1E4

# 5A
# Calculate the premium required for a 5% chance of losing money given loans, 
# probability of death , and loss per claim . Save this premium as x for use 
# in further questions.
a <- -150000
n <- 1E3
p <- 0.015
z <- qnorm(0.05)

b <- -a*( n*p - z*sqrt(n*p*(1-p)))/ ( n*(1-p) + z*sqrt(n*p*(1-p)))
b

( (a * p) + (b * (1 - p)))

# 5D
# Run a Monte Carlo simulation with B=10000to determine the probability of 
# losing money on 1,000 policies given the new premium x, loss on a claim of 
# $150,000, and probability of claim . 
set.seed(28, sample.kind = "Rounding")

B <- 1E4
x <- replicate(B, sum(sample(c(a, b),
                         size = 1E3,
                         prob = c(p, 1 - p),
                         replace = TRUE)))

# What is the probability of losing money here?
sum(x < 0) / length(x)

# The company cannot predict whether the pandemic death rate will stay stable. 
# Set the seed to 29, then write a Monte Carlo simulation that for each of 
# iterations:
# - randomly changes by adding a value between -0.01 and 0.01 with 
# sample(seq(-0.01, 0.01, length = 100), 1)
# - uses the new random to generate a sample of policies with premium x and 
# loss per claim 
# - returns the profit over policies (sum of random variable)
set.seed(29, sample.kind = "Rounding")

mc <- function(p = 0.015){
  p <- p + sample(seq(-0.01, 0.01, length = 100), 1)
  policies <- sample(x = c(a, b),
                     prob = c(p, 1 - p),
                     replace = TRUE,
                     size = 1E3)
  return(sum(policies))
}

x <- replicate(B, mc())

# What is the expected value over 1,000 policies?
sum(x) / length(x)

# What is the probability of losing money?
sum(x < 0) / length(x)

# What is the probability of losing more than $1 million?
sum(x < -1E6) / length(x)
