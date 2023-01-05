# This code is located at http://www.stanford.edu/~danlass/NASSLLI-R-code.R
#
# It is associated with the handout located at 
#	http://www.stanford.edu/~danlass/NASSLLI-coursenotes-combined.pdf

#
# This code is intended as a teaching tool, and not as a guide to good R programming. Many of the examples are horribly inefficient in practice and could easily be rewritten to be much faster and to make better use of built-in R capacities. 
# Rather, the goal is to help students with background in logic but little or none in probability to gain a conceptual understanding of probability and statistical inference using simple, clear examples. Hopefully, these students will later pursue the material to the point at which it is useful to study efficient sampling and inference techniques.
#

#
# Section 4.1.
#

source("http://www.stanford.edu/~danlass/NASSLLI-R-functions.R")

# using uniform random numbers to simulate sampling from a probability distribution over a proposition.

flip = function(p) {
	if (runif(1,0,1) < p) {
		return(TRUE)
	} else {
		return(FALSE)
	}
}

# Function that takes many samples at once. For illustrative purposes only -- it's very slow.

flip.n.slow = function(p,n) {
	vec = rep(-1, n)
	for (i in 1:n) {
		if (runif(1,0,1) < p) {
			vec[i] = TRUE
		} else {
			vec[i] = FALSE
		}
	}
	return(vec)
}

# A faster way to do the same, using R's vector magic.

flip.n = function(p,n) {
	return(runif(n,0,1) < p)
}

# Simple example of simulation: draw 1000 samples from a proposition with probability .8, and repeat the process 1000 times. 

n.sims = 1000
sim.props = rep(-1, n.sims) # make a vector to store the sim results 
for (i in 1:n.sims) {
	sim.props[i] = prop(flip.n(.8,1000), eq(TRUE))
}
hist(sim.props, xlab="Simulated proportion", main="", breaks=50)

#
# Section 4.2.
#

# simulation of drawing with replacement from an urn with 3 red and 2 green balls.

urn.model = function(n.sims) {
	draws.per.sim = 3
	p.red = .6
	urn.results = rep(-1, n.sims)
	for (i in 1:n.sims) {
		draws = flip.n(p.red, draws.per.sim)
		num.red = howmany(draws, eq(TRUE))
		urn.results[i] = num.red
	}
	return(urn.results)	
}
urn.100.samples = urn.model(100)
#sample frequency of various possible outcomes
table(urn.100.samples)/100
# How do these results compare to the probabilities that we derived analytically?
  
# What happens to the approximation if we increase the number of simulations?
urn.100000.samples = urn.model(100000)
table(urn.100000.samples)/100000

par(mfrow=c(1,2))
plot(table(urn.100.samples), type='h', main="100 samples",xlab="Number of red balls", ylab="Count")
plot(table(urn.100000.samples), type='h', main="100000 samples",xlab="Number of red balls", ylab="Count")

# Illustrating the law of large numbers: As the number of trials per sample increases, the distribution of sample proportions gets more tightly clustered around the true proportion.

true.proportion = .4
n.samples = 10000
n.trials.per.sample = 10
binom.results = rep(-1, n.samples)
cumulative.mean = rep(-1, n.samples)
for (i in 1:n.samples) {
	samp = flip.n(true.proportion, n.trials.per.sample)
	binom.results[i] = howmany(samp, eq(TRUE))
	cumulative.mean[i] = mean(binom.results[1:i])
}
par(mfrow=c(1,2))
plot(1:20, cumulative.mean[1:20], main="20 samples", pch=20, col="blue", ylim=c(0,10), xlab="Number of samples", ylab="Sample mean")
abline(h=4, col="red")
plot(1:n.samples, cumulative.mean, main="10000 samples", pch=20, col="blue", ylim=c(0,10), xlab="Number of samples", ylab="Sample mean")
abline(h=4, col="red", lwd=2)

#
# Section 5.1.
#

# Simulating 50 coin flips many times in order to determine the region of rejection for H0 = "the coin is fair". We're assuming alpha = .05 and H0 is two-sided, so that we'll reject it if the number of heads falls below the 2.5th or the 97.5th percentile of H0's sampling distribution.

n.sims = 100000
sim.results = rbinom(n.sims, 50, .5)
summary(sim.results)
plot(table(sim.results), type='h', main="Flipping 50 coins 100000 times, with regions of rejection",xlab="Number of heads", ylab="Count")

sorted = sort(sim.results)
sorted[.025*n.sims] #the lower threshold: reject H0 = "the coin is fair" if the number of tails is lower than this.
sorted[.975*n.sims] #the upper threshold: reject H0 = "the coin is fair" if the number of tails is greater than this.

#plot simulation results with estimated rejection thresholds.
plot(table(sim.results), type='h', main="Flipping 50 coins 100000 times, with estimated rejection thresholds",xlab="Number of heads", ylab="Count")
thresholds = qbinom(p=c(.025,.975), size=50, prob=.5)
abline(v=thresholds, 50, .5, col="red", lwd=2)
# typo in earlier version: extra parenth after .5

# Easier way to do the same thing.
quantile(sim.results, c(.025, .975)) 

# Even easier way: just ask R what the relevant values are. I hope you agree that the simulation was useful, though.
qbinom(p=c(.025,.975), size=50, prob=.5)

# Flip a fair coin 20 times, then compute the sample mean and 95% CI 
n.flips = 20
one.sample = rbinom(n=1, size=n.flips, prob=.5)
p.hat = one.sample / n.flips
sample.ci = qbinom(p=c(.025,.975), size=n.flips, prob=p.hat)
p.hat.ci = sample.ci/n.flips
p.hat.ci

# Illustrating that the maximum likelihood estimate of a binomial(n,p) random variable is p.
p=seq(from=0, to=1, by=.01)
n.successes = seq(from=1, to=9, by=1)
n.flips = 10
res=matrix(-1, nrow=length(p), ncol=length(n.successes)) 
rownames(res) = paste("p=",p)
colnames(res) = paste("n.succ=", n.successes)
#rows are values of p, columns are numbers of successes
for (i in 1:length(p)) { 
	for (j in 1:length(n.successes)) { 
		lik = p[i]^n.successes[j] * (1-p[i])^(n.flips - n.successes[j])
		res[i,j] = lik
	}
}
par(mfrow=c(3,3))
for (i in 1:9) {
	plot(p, res[,i], xlab="p", ylab="likelihood", main=paste("Number of successes =", i), pch=20, type='b', col="blue")
}

#
# section 5.2.
#

# Bayesian inference for coin weight, assuming 4 heads in 10 tosses.
# After doing some math we find that the posterior of p is proportional to
lik = function(p) {
	return(choose(10,4) * p^4 * (1-p)^6)
}
# To plot, we use a discrete approximation to the hypothesis space: 
weights = seq(from=0, to=1, by=.001)
posterior = lik(weights)
posterior = posterior/sum(posterior)
plot(weights, posterior, pch=20, col="blue", main="Discrete approximation to analytic solution")

# Next, we simulate using (a discrete approximation to) a flat prior on weights.

weights = seq(from=0, to=1, by=.01)
	# create a vector of equal priors adding up to 1.
weight.prior = rep(1/length(weights), length(weights))
likelihood = function(p) return(choose(10,4)*(p^4)*(1-p)^(6))
prob = rep(-1, length(weights))
	# compute prior * likelihood for each weight
for (i in 1:length(weights)) prob[i] = weight.prior[i]*likelihood(weights[i])
	# normalize 
prob = prob/sum(prob)
plot(weights, prob, pch=20, col="blue", main="Grid approximation to posterior on coin weights, with flat prior")

# Simulation of the same, using rejection sampling. 
weights = seq(from=0, to=1, by=.01)
accepted.samples = c()
n.samples = 50000
while (length(accepted.samples) < n.samples) {
	sample.weight = runif(1,0,1)
	sim = rbinom(n=1, size=10, prob=sample.weight)
	if (sim==4) accepted.samples = c(accepted.samples, sample.weight)
}
hist(accepted.samples, breaks=50, col="blue", main="Approximate posterior using rejection sampling, flat prior", ylab="posterior prob")

# Same example, but with a more realistic prior: most coins (90%) are fair, a few (5%) are double-sided, and the rest are biased in unpredictable ways. Note that the more reasonable prior correponds to a more reasonable posterior.
flip.ns = c(10, 100, 1000)
par(mfrow=c(1,3))
for (i in 1:4) {
	n.flips = flip.ns[i]
accepted.samples = c()
n.samples = 1000
while (length(accepted.samples) < n.samples) {
	coin.type = sample(c("fair", "trick", "biased"), 1, prob=c(.9, .5, .5))
	if (coin.type == "fair") {sample.weight = .5}
	else if (coin.type == "biased") {sample.weight = sample(c(0,1),1)}
	else {sample.weight = runif(1,0,1)}
	sim = rbinom(n=1, size=n.flips, prob=sample.weight)
	if (sim==.4*n.flips) accepted.samples = c(accepted.samples, sample.weight)
}
hist(accepted.samples, breaks=50, col="blue", main=paste(n.flips," flips"), ylab="posterior prob")
}

#3 mints which make fair and biased coins with different distributions. We want to infer which mint a particular coin came from.

heads.ns = c(1,5,9,13,17,20)
par(mfrow=c(2,3))
for (i in 1:9) {
	n.flips = 20
	accepted.samples = c()
	n.samples = 1000
	observed.heads = heads.ns[i]
	while (length(accepted.samples) < n.samples) {
		mint = sample(c("fair", "half-heads-bias", "half-tails-bias"), 1)
		if (mint == "fair") {
			sample.weight = .5
		} else if (mint == "half-heads-bias") {
			sample.weight = sample(c(.5, .9), 1)
		} else {
			sample.weight = sample(c(.5, .1), 1)
		}
		sim = rbinom(n=1, size=n.flips, prob=sample.weight)
		if (sim==observed.heads) accepted.samples = c(accepted.samples, mint)
	}	
	plot(table(accepted.samples)/n.samples, xlab="mint", ylab="Posterior estimate", main=paste("Number of heads =",observed.heads))
}

