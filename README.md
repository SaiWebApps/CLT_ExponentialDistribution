# Exponential Distributions and the Central-Limit Theorem
## By: Sairam Krishnan ([sairambkrishnan@gmail.com](mailto:sairambkrishnan@gmail.com))

### Overview


### Code for Simulations

#### Dependencies

* ggplot2
* [Multiplot.R](http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)) - Copy this code to a file called "Multiplot.R," and place the file in the same directory as the main script below.

#### ExponentialDistributionUtils.R

```r
## @param numSimulations
##	(Required)
##	Number of times we need to generate random exponential distributions and apply
##	some function to them.
##
## @param numExponentials
##	(Required)
##	Number of random exponentials to generate.
##
## @param lambda
##	(Required)
##    2nd parameter for rexp function.
##
## @param applyFunction
##	(Optional, NULL by default)
##	A function that shall take in an exponential distribution as a parameter
##	and execute some function upon it (e.g., mean).
## 
## @return
##	- If applyFunction is NULL, then return a numeric vector with "numSimulations" sets
##	  of random exponential distributions, each of which consists of "numExponentials"
##	  numbers.
##	- If applyFunction is not NULL, then return a numeric vector with "numSimulations"
##	  sets of the results from applyFunction, which shall take in an exponential distribution
##	  as a parameter.
getDistributionOfExponentials <- function(numSimulations, numExponentials, lambda, 
		applyFunction = NULL) {
	results <- numeric()

	# If applyFunction is NULL, then by default, just return the exponential distribution
	# as is.
	if (is.null(applyFunction)) {
		applyFunction <- function(exponentialDistribution) {
			exponentialDistribution
		}
	}

	# Generate a random exponential distribution with "numExponentials" elements
	# "numSimulations" times; invoke applyFunction on said distribution, and accumulate
	# or aggregate the results within the "results" numeric vector.
	for (i in 1:numSimulations) {
		randomExpDistribution <- rexp(numExponentials, lambda)
		results <- c(results, applyFunction(randomExpDistribution))
	}

	results
}

## @param numSimulations
##	(Required)
##	Number of times we need to generate random exponential distributions and apply
##	some function to them.
##
## @param numExponentials
##	(Required)
##	Number of random exponentials to generate.
##
## @param lambda
##	(Required)
##    2nd parameter for rexp function.
##
## @param applyFunction
##	(Required)
##	A function that shall take in an exponential distribution as a parameter
##	and execute some function upon it (e.g., mean).
##
## @param functionIdMessage
##	(Required)
##	Character vector containing an id/message that differentiates the regular
##	exponential distribution's results from those resulting from getDistributionOfExponentials.
##
## @return
##	A data frame containing 2 columns:
##	- "Type": Indicates whether we're dealing with regular exponential distribution results
##	   or those from getDistributionOfExponentials
##	- "Data": The actual values/data points
compareDistributionsOfExponentials <- function(numSimulations, numExponentials, lambda, 
		applyFunction, functionIdMessage) {
	randomExponentials <- data.frame(
		Data = rexp(numSimulations, lambda),
		Type = paste(numSimulations, " Random Exponentials")
	)

	expDistributionAfterFunction <- data.frame(
		Data = getDistributionOfExponentials(numSimulations, numExponentials, lambda, 
				applyFunction),
		Type = functionIdMessage
	)

	rbind(randomExponentials, expDistributionAfterFunction)
}

## @param numSimulations
##	(Required)
##	Number of times we need to generate random exponential distributions and apply
##	some function to them.
##
## @param numExponentials
##	(Required)
##	Number of random exponentials to generate.
##
## @param lambda
##	(Required)
##    2nd parameter for rexp function.
##
## @return
##	The result of "compareDistributionsOfExponentials" for applyFunction = function(ed) {
##	mean(ed) } 
compareAverages <- function(numSimulations, numExponentials, lambda) {
	compareDistributionsOfExponentials(numSimulations, numExponentials, lambda, function(ed) {
		mean(ed)
	}, paste(numSimulations, " Averages of ", numExponentials, " Random Exponentials"))
}

## @param numSimulations
##	(Required)
##	Number of times we need to generate random exponential distributions and apply
##	some function to them.
##
## @param numExponentials
##	(Required)
##	Number of random exponentials to generate.
##
## @param lambda
##	(Required)
##    2nd parameter for rexp function.
##
## @return
##	The result of "compareDistributionsOfExponentials" for applyFunction = function(ed) {
##	var(ed) } 
compareVariances <- function(numSimulations, numExponentials, lambda) {
	compareDistributionsOfExponentials(numSimulations, numExponentials, lambda, function(ed) {
		var(ed)
	}, paste(numSimulations, " Variances of ", numExponentials, " Random Exponentials"))
}

## @dependencies
##	ggplot2
##	"Multiplot.R" - R script should be in same directory as this script.
##
## @param numSimulations
##	(Optional, default = 1000)
##	Number of times we need to generate random exponential distributions and apply
##	some function to them.
##
## @param numExponentials
##	(Optional, default = 40)
##	Number of random exponentials to generate.
##
## @param lambda
##	(Optional, default = 0.2)
##    2nd parameter for rexp function.
##
## @return
##	2 histograms, one comparing the sample and theoretical mean distributions and another
##	comparing the sample and theoretical variance distributions.
runSimulation <- function(numSimulations = 1000, numExponentials = 40, lambda = 0.2) {
	library("ggplot2")

	avgs <- ggplot(compareAverages(numSimulations, numExponentials, lambda), aes(Data, fill = Type)) +
		geom_histogram(aes(y=..density..)) +
		geom_density(alpha = 0.2) +
		theme(legend.position = "top") +
		ggtitle("Comparing Sample and Theoretical Means' Distributions for Random Exponentials")

	vars <- ggplot(compareVariances(numSimulations, numExponentials, lambda), aes(Data, fill = Type)) +
		geom_histogram(aes(y=..density..)) +
		geom_density(alpha = 0.2) +
		theme(legend.position = "top") +
		ggtitle("Comparing Sample and Theoretical Variances' Distributions for Random Exponentials")

	source("Multiplot.R")
	multiplot(avgs, vars, cols = 2)

```

#### How to Execute

```
>> source("ExponentialDistributionUtils.R")
>> runSimulation()
```

### Results

#### Graphs
![Simulation Results]("https://github.com/SaiWebApps/CLT_ExponentialDistribution/SimulationResults.jpeg")

#### Analysis

