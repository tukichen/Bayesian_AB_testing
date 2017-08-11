# Bayesian A/B testing

This notebook presents step by step instruction how to build a Bayesian A/B Test Calculator with visualization of results using R.
The Shiny web app under construction is https://qiaolinchen.shinyapps.io/ab_test/. 
Another way to use is to run on R console.

install.packages("shiny")
library(shiny)
runGitHub("Bayesian_AB_testing", "tukichen")

Here is an outline of the project:

### 1. Dataset simulation and  Data manipulation
* To assess the performance of the Bayesina A/B test, I simulate datasets for which true test diferences are known.
* Write functions to process data and compute the cumulative counts and conversions.

### 2. Bayesian A/B test: 
Write functions to do A/B testing for data observed up to a particular day: 
   - Summarize cumulative counts and cumulative conversion rates by group
   - Derive posterior probability, point estimates and credible interval of difference in proportion
   - Perform Bayesian testing using Bayes factors
   - Visualization of prior, and posterior probabilities

### 3. Repeat Bayesian A/B test on cumulative conversion rate over time: 
For all days, perform Bayesian A/B testing using the above functions:
   - When the user select a particular day after the tests begin, give summaries of Bayesian A/B testing
   - Plot the point estimates of difference in proportion, and thier credible interval over time
   - Plot the probability of each test being the best over time
   - Plot the Bayes factor over time 

### 4. Wrap all functions to build a Shiny web app 
Users can use this app to read A/B test data, perform tests, plot the pior and posterior probabilities, monitor the test progress by examing the trend of cumulative conversion rate change over time, and make decisions.


## Backgroups and Methods

Most A/B test approaches are centered around frequentist hypothesis tests used to come up with a point estimate (probability of rejecting the null) of a hard-to-interpret value. Oftentimes, the statistician or data scientist laying down the groundwork for the A/B test will have to do a power test to determine sample size and then interface with a Product Manager or Marketing Exec in order to relay the results. This quickly gets messy in terms of interpretability. More importantly it is simply not as robust as A/B testing given informative priors and the ability to inspect an entire distribution over a parameter, not just a point estimate.

### Bayesian A/B testing
Bayesian methods provide several benefits over frequentist methods in the context of A/B tests - namely in interpretability. Instead of p-values you get direct probabilities on whether A is better than B (and by how much). Instead of point estimates your posterior distributions are parametrized random variables which can be summarized any number of ways. Bayesian tests are also immune to ‘peeking’ and are thus valid whenever a test is stopped.

### Methods

Unlike a frequentist method, in a Bayesian approach you first encapsulate your prior beliefs mathematically. This involves choosing a distribution over which you believe your parameter might lie. As you expose groups to different tests, you collect the data and combine it with the prior to get the posterior distribution over the parameter(s) in question. Mathematically, you are looking for P(parameter | data) which is a combination of the prior and posterior (the math, while relatively straightforward, is outside of the scope of this brief intro).

## Bayesian A/B Test Calculator 
