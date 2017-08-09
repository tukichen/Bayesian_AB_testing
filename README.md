# Bayesian A/B testing
## A web app for analyzing A/B testing data using Bayesian approach

Most A/B test approaches are centered around frequentist hypothesis tests used to come up with a point estimate (probability of rejecting the null) of a hard-to-interpret value. Oftentimes, the statistician or data scientist laying down the groundwork for the A/B test will have to do a power test to determine sample size and then interface with a Product Manager or Marketing Exec in order to relay the results. This quickly gets messy in terms of interpretability. More importantly it is simply not as robust as A/B testing given informative priors and the ability to inspect an entire distribution over a parameter, not just a point estimate.

### Bayesian A/B testing
Bayesian methods provide several benefits over frequentist methods in the context of A/B tests - namely in interpretability. Instead of p-values you get direct probabilities on whether A is better than B (and by how much). Instead of point estimates your posterior distributions are parametrized random variables which can be summarized any number of ways. Bayesian tests are also immune to ‘peeking’ and are thus valid whenever a test is stopped.
