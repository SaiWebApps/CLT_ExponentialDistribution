# Exponential Distributions and the Central-Limit Theorem
## By: Sairam Krishnan ([sairambkrishnan@gmail.com](mailto:sairambkrishnan@gmail.com))

### Overview
The Central-Limit Theorem essentially states that gathering arithmetic means of a random variable through a large number of iterations of an experiment will yield an approximately normal distribution. The purpose of this report is to examine the Central-Limit Theorem for R's random exponential distribution. Specifically, we will compare a random exponential distribution with 1000 exponentials to the distribution of 1000 arithmetic means of random exponential distributions consisting of 40 elements. Note that for this report, lambda, the second parameter used to generate random exponential distributions in R, is 0.2.

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

#### Explanations
compareAverages and compareVariances generate data frames with 2 columns: Type and Data. <br>
The latter contains the actual data that will be graphed while the former contains the measure used to classify the data (the categories in the legend). Specifically, the former (Type) will indicate whether the data is from the regular random exponential distribution with 1000 elements, from the distribution of 1000 means of 40-long random exponential distributions, or from the distribution of 1000 variances of 40-long random random exponential distributions.<br>
runSimulation is the main method or driver, so to speak. It graphs the data in compareAverages and compareVariances in a single window; compareAverages is displayed on the left-hand side, and compareVariances is shown on the right-hand side. In both graphs, a red curve + histogram would be shown concurrently with a blue curve + histogram; the red is for the regular random exponential distribution while the blue is for the 1000 iterations of the simulation.

#### How to Execute

```
>> source("ExponentialDistributionUtils.R")
>> runSimulation()
```

### Results

#### Graphs
![Simulation Results](https://github.com/SaiWebApps/CLT_ExponentialDistribution/blob/master/SimulationResults.jpeg)

#### Analysis
As we postulated in the Overview above, even if the regular random exponential distribution is not normally distributed, the distribution of the 1000 averages of random exponential distributions IS normally distributed. A primary distinguishing characteristic of the normal distribution is its bell-shape, which we can clearly see in the left-hand graph for the blue data (1000-averages-of-40-Random-Exponentials). We can see that the same is true for the right-hand graph, which is comparing variances instead.<br>
For random exponential distributions, theoretically speaking, the mean and standard deviation should both be 1/lambda, which is 5 in this case given that lambda = 0.2; consequently, the theoretical variance should be 25. While experimentation in R shows that the regular random exponential distribution certainly conforms to this expectation, the main difference between the regular random exponential distribution (the red curve) and the distribution of 1000 means (the blue curve) lies in the fact that the central point on the blue curve (by virtue of the fact that it's approximately normally distributed) almost exactly lies on the theoretical values (5 and 25 respectively). That is not the case for the red curves, both of which tend to skew left.