
flip = function(p=.5) return(runif(1,0,1) < p)
  # flips a coin with weight p
normalize = function(v) v/sum(v)
  # normalizes a vector of probabilities to ensure it sums to 1
  # usually invoked after conditionalization/Bayes' rule

p.rain = .3
p.sprinkler = .4

gen.worlds = function(n) {t(sapply(1:n, FUN=function(i) {
    rain = flip(p.rain)
    sprinkler = flip(p.sprinkler)
    wet.grass = rain || sprinkler
    return(c(rain=rain, sprinkler=sprinkler, wet.grass=wet.grass))
}))}
gen.worlds(1)
gen.worlds(30)

#
# prob. approximation from samples 
# 

some.worlds = gen.worlds(100)
  # approximate priors on rain, sprinkler, WG
normalize(table(some.worlds[,1]))  #rain
normalize(table(some.worlds[,2]))  #sprinkler
normalize(table(some.worlds[,3]))  #wet grass

# notice that these values are close, but not identical, to the priors that we would specify (for ran, sprinkler) or that we would calcualate for wet grass, as
# P(rain) + P(wet grass) - P(rain & wet gress) = .3 + .4 - .3*.4 = .58
# where P(rain & wet gress) = P(rain) * P(wet grass) because these variables are independent

# if you run the code again, you'll get a slightly different approximation.

# now, try changing the line 'some.worlds = gen.worlds(100)' above to make it generate more or fewer worlds. How does this affect the quality of the approximation?

#
# approximate inference: the generate-and-ignore method
#
# suppose we observe: the grass is wet
  # how likely are rain and sprinkler now?

filtered = some.worlds[which(some.worlds[,3] == TRUE),]
  # throw out all the worlds where wet.grass is false

#
# now we can use these conditional samples to estimate probabilities of all variables, conditional on wet grass:
#

normalize(table(filtered[,1]))  #rain
normalize(table(filtered[,2]))  #sprinkler
normalize(table(filtered[,3]))  #wet grass

# notice that the probabilities of rain and sprinker go up, since the observation provides probabilistic support for both hypotheses that could explain it.

#
# Rejection sampling
#
# rejection sampling is a slightly different way to do generate-and-ignore. It's a semantically clear, though inefficient, way to do approximate inference.
#
# In effect, rejection sampling means that we do the filtering inside the simulation by throwing out samples that don't meet the condition without even recording them.

conditional.gen.world = function() { 
    # generate a world, and return if only if it makes wet.grass true
  gen.world = function() {
    rain = flip(p.rain)
    sprinkler = flip(p.sprinkler)
    wet.grass = rain || sprinkler
    return(c(rain, sprinkler, wet.grass))
  } 
  sampled.world = gen.world()
  while (sampled.world[3] == FALSE) { # if wet.grass if false, grab another sample
    sampled.world = gen.world()
  }
  return(sampled.world) # once you get a sample where wet.grass is true, return it
}

conditional.gen.world()
  # run this over and over - each time you'll get a possibly different sample, but the condition will always be true

conditional.gen.worlds = function(n) t(sapply(1:n, FUN=function(i) conditional.gen.world()))
  # a helper to make it easy to generate lots of conditional samples at once
conditional.gen.worlds(100)
 
#
# Explaining away: if we also learn that it rained, the observation of wet grass no longer provides support for sprinker 
#

some.worlds = gen.worlds(5000)
# approximate prior on sprinkler
normalize(table(some.worlds[,2]))  #probs for sprinkler

# observation: the grass is wet
  # how likely is sprinkler now?

filtered = some.worlds[which(some.worlds[,3] == TRUE),]
normalize(table(filtered[,2]))  #updated probs for sprinkler

# further observation: it rained
  # how likely is sprinkler now?

filtered = filtered[which(filtered[,1] == TRUE),]
normalize(table(filtered[,2])) ##updated probs for sprinkler

# Why does it go back down to the prior?

#
# THE VENDING MACHINE
#

hypotheses = c(.2, .4, .6, .8)
observation = c('B', 'B', 'B', 'B', 'C', 'B', 'B')

### EXACT INFERENCE

prior = function(p) {
	return(1/length(hypotheses))
}
likelihood = function(p, obs) {
	n.bagels = length(which(obs == 'B'))
	n.cookies = length(which(obs == 'C'))
	return(p^n.bagels * (1-p)^n.cookies)
}

posterior = rep(NA, length(hypotheses))
for (i in 1:length(posterior)) { # for each hypothesis
	pr = prior(hypotheses[i]) # compute prior
	lk = likelihood(hypotheses[i], observation)
	posterior[i] =  pr * lk
}

# NORMALIZE
posterior = posterior/sum(posterior)

plot(hypotheses, posterior, xlim=c(0,1), pch=20, type='b')

### APPROXIMATE INFERENCE

button.press = function(p) {
	if (runif(1,0,1) < p) return("B") 
	else return("C")
}
many.button.presses = function(p,n) {
	return(sapply(1:n, function(i) return(button.press(p))))
}

n.sims = 150000
accepted.samples = c()
num.presses = length(observation)
for (i in 1:n.sims) {
	p = sample(hypotheses, 1)
	sample.button.presses = many.button.presses(p, num.presses)
	if (all(sample.button.presses == observation)) {
		accepted.samples = c(accepted.samples, p)
	}
}

# side-by-side plot
par(mfrow=c(1,2))
plot(hypotheses, posterior, xlim=c(0,1), pch=20, main='Exact inference', type='b')
plot(table(accepted.samples), xlim=c(0,1), main='Approximate inference')

posterior
approx = normalize(table(accepted.samples))
cor(posterior, approx)

#
# the lottery scenario
#
# we'll go through the process of estimating the conditional probability of mary winning, given that he  buys a ticket. This will seem fairly laborious given that the probability is obviously .01, but it reveals the inner workings of conditional sampling in a way that will be important when we introduce the trivalent semantics for conditionals
# 

sims = function(n) {sapply(1:n, FUN=function(i) {
  n.tickets = 100
  buy.ticket = flip(p=.5) # mary buys a ticket with some probability 
  winning.ticket.number = sample(1:n.tickets, 1)
  if (buy.ticket) {
    marys.ticket = sample(1:n.tickets, 1)
    win = winning.ticket.number == marys.ticket
  } else {
    marys.ticket = NA
    win = FALSE
  }
  return(c(buy.ticket, win))
})}
t(sims(30))
samples = t(sims(1000))
normalize(table(samples[,2])) # unconditional prob of winning
                                # should be around .05

conditional.samples = samples[which(samples[,1] == TRUE),]
      # condition on mary buying a ticket
normalize(table(conditional.samples[,2]))
      # how likely is it that he wins, if he does ?






