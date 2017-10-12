# Bayesian A/B testing

This notebook presents step by step instruction how to build a Bayesian A/B Test Calculator with visualization of results using R.
* The Shiny web app under construction is https://qiaolinchen.shinyapps.io/ab_test/. 
* Another way to use is to run on R console: 

install.packages("shiny");  library(shiny); runGitHub("Bayesian_AB_testing", "tukichen")

## Functions of this web app:
### 1. Bayesian A/B test Calculator: 
- Perform a single A/B testing using input test data and prior parameters 
- Summarize the Bayes factor, point estimate of rate change with credible interval, probability of variant better than default, and a frequentist p-value.
- Visualizae prior and posterior probabilities

### 2. Compare performance of Bayesian and Frequentist A/B Tesing using Simulation
* Simulate datasets for which true test diferences are known.
- Summarize and visualize the conversion rates change over time
- Visualize the trends of conversion rate change over time with CI (Bayesian credible interval or frequentist confidence interval) over time
- Visualize the trends of the following quantities over time:  Bayes factor, p-value, Uplift probability of variant better than control 

### 3. Upload data and perform Bayesian A/B test: 
- Perform A/B test on datasets uploaded by users
- Give the same summary and visualization as above in simulation part

## Backgroups and Methods

Most A/B test approaches are centered around frequentist hypothesis tests used to come up with a point estimate (probability of rejecting the null) of a hard-to-interpret value. Oftentimes, the statistician or data scientist laying down the groundwork for the A/B test will have to do a power test to determine sample size and then interface with a Product Manager or Marketing Exec in order to relay the results. This quickly gets messy in terms of interpretability. More importantly it is simply not as robust as A/B testing given informative priors and the ability to inspect an entire distribution over a parameter, not just a point estimate.

### Bayesian A/B testing
Bayesian methods provide several benefits over frequentist methods in the context of A/B tests - namely in interpretability. Instead of p-values you get direct probabilities on whether A is better than B (and by how much). Instead of point estimates your posterior distributions are parametrized random variables which can be summarized any number of ways. Bayesian tests are also immune to ‘peeking’ and are thus valid whenever a test is stopped.

### Methods

Unlike a frequentist method, in a Bayesian approach you first encapsulate your prior beliefs mathematically. This involves choosing a distribution over which you believe your parameter might lie. As you expose groups to different tests, you collect the data and combine it with the prior to get the posterior distribution over the parameter(s) in question. Mathematically, you are looking for P(parameter | data) which is a combination of the prior and posterior (the math, while relatively straightforward, is outside of the scope of this brief intro).

