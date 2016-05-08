

# The followig data represent data from A/B Testing a landing page
# and we want to determine the results.
# Each row denotes a visitor to the landing page and 1 represents a 
# converted visitor to a user.
# Column A represents the results from the control landing page.
# Column B represents results from the test landing page.

conversion_data <- read.csv('landing_page_data.csv', header = T)
View(conversion_data)

# Looking at the data, each row represents one sample. 
# A visitor is randomly assigned to landing page A or B, and 
# either converts or does not convert to a user.
# The number of samples conducted is our two sample sizes. 
# Since we want to determine if the conversion rate for landing page
# B is higher than the rate for A, we want the statistical test which 
# allows for comparison of the two conversion rates.

# We first check the number of rows in data set.
nrow(conversion_data)
# We count the ones that do NOT contain NA's
colSums(!is.na(conversion_data))

# We compute here the total number of of conversions for each
# landing page
sum(conversion_data$a, na.rm = TRUE)
sum(conversion_data$b, na.rm = TRUE)


# t-test is the most common statistical test for comparing two 
# groups and can be used here. The more correct test is, however,
# the z-test for comparing two PROPORTIONS
# NOTE ON Z-test:
# The following should be true for z-tests:
#  1. The two samples must be independent.
#  2. The sample sizes must be large enough (>30) for the normal 
#     distribution to apply.
#  3. The samples must be randomly selected.
# 
# For this excerices we assume the samples are independent 
# (a visitor of one page had no affect on a visitor of another page).
# Our sample sizes are large enough; Central Limit Theorem applies.
# We assume the experiment was randomized
# If our sample size was less than 30, a t-test would be more appropriate.

# In the ideal world, all statistical hypotheses would be tested 
# on entire populations. However, this is often impractical or 
# impossible, so we typically examine a random sample from the
# population.
# Hypothesis testing is defined as the process to which you can test a 
# claim about a population parameter.
# In our landing page example, the population parameter is the 
# difference between conversion rates of landing page B and 
# landing page A.
# Because we don't have data for every user that will visit their 
# two landing pages 
# (which would enable us to calculate the true population parameter), 
# We therefore take a sample, calculate a sample statistic, and then determine
# the likelihood that our sample statistic could have occured, 
# assuming our null hypothesis regarding the population parameter is true.
# In our landing page example, our sample statistic is calculated from the
# data provided. 

# Null & Alternative Hypothesis

# The null hypothesis:
# The statement about the population parameter that is assumed to be true, 
# it's usually the hypothesis that the sample statistic observed was purely 
# from chance.
#
# The alternative hypothesis:
# The alternative hypothesis directly contradicts the null and is 
# usually the hypothesis that the sample statistic was influenced by some cause.

# Our null hypothesis: the difference of conversion rates between landing 
# page B (P-hat 1) and landing page A (P-hat 2) is equal to 0
# Our alternative hypothesis: the difference of conversion rates between 
# landing page B (P-hat 1) and landing page A (P-hat 2) is greater than 0.
# Since our sample statistic is an 
# observed estimate, we denote that with ^ over the variable.

# Performing the Z-Test
#
#

# With our sample statistic we now must normalize it.
# This is done by converting 
# it into a z-score. It's called a z-score because the normal 
# distribution is 
# also known as the Z-distribution.
# We can use the following formula to calculate our z-score for comparing two 
# proportions:

# p1: the conversion rate of our test landing page B
# p2: the conversion rate of our control landing page A
# Y1: the number of successes in our test landing page B
# Y2: the number of successes in our control landing page A
# n1: total sample size of A
# n2: total sample size of B
# p: pooled proportion

# To calculate the two conversion rates we can simply calculate the proportion 
# of 1s in the two columns. The only trick is use na.omit() to remove the NA 
# in column A.

a_obs <- na.omit(conversion_data$a)
b_obs <- na.omit(conversion_data$b)
a_proportion = sum(a_obs)/length(a_obs)
b_proportion = sum(b_obs)/length(b_obs)
a_proportion
b_proportion

# Once we have our test statistic (sample statistic) we then want to 
# figure out, what are our odds of getting this statistic again?
# We calculate the test statistic and assign it to the variable test_statistic, 
# verify your calculation below

test_statistic <- b_proportion - a_proportion
test_statistic

# Calculating the pooled proportion
# After calculating the test statistic we need to calculate the pooled 
# proportion and assign it to the variable pooled_proportion. 
# We must calculate the pooled proportion because our null hypothesis is 
# that the difference between conversion rates is equal to 0.

# The pooled proportion is computed below

pooled_proportion <- (sum(a_obs) + sum(b_obs)) / (length(a_obs) + length(b_obs))
pooled_proportion


# We now calculate the variability of our test statistic in its
# sampling distribution; the standard error.
# The standard error is the denominator of the equation for Z.
# 
SError <- sqrt(pooled_proportion*(1-pooled_proportion)*(1/length(a_obs) + 1/(length(b_obs))))
SError

# Calculating the Z-Score
# By dividing our test statistic by its standard error we create the
# z-score or Z in our equation. The Z-score gives us a relative value to 
# which we can determine how extreme the occurence of our test statistic is 
# based on the normal distribution.

z_score <- (b_proportion - a_proportion) / SError
z_score

# The z-score represents how many standard deviations away our observed 
# test statistic calculates to assuming the null hypothesis is true
# We now use the normal distribution to determine what is the probability of
# getting a z-score more extreme than the one we observed. 
# Since our test statistic is positive we want to look at the area under
# the right tail of the normal distribution

# To calculate the area under the normal curve to the right of our z-score 
# we use the pnorm() function in R. 
# We set lower.tail to be FALSE so that the function calculates the area under 
# the right.

p_value <- pnorm(z_score, lower.tail = F)
p_value

# The area under the curve to the right of the z-score is 
# the chance of getting a test statistic as extreme as, 
# or more extreme than, what we observed, given that the 
# null hypothesis is true.
# This area is known as the P-value.
# The P-value is the chance of getting a more extreme 
# test statistic assuming that the null hypothesis is true.
# It is compared with the alpha level, 
# a user set threshold for Type I error 
# (concluding a false positive).
# The alpha level sets the standard for how extreme the data 
# must be for you to reject your null hypothesis. 
# It is the amount of chance you are willing to accept to 
# attaining a more extreme test statistic.
# A low amount means you require a low probability of the 
# occurance in your experiment being due to chance.
# In research it is most commonly set at 5%.


# Determining the results:
# If our p-value is greater than our alpha, that means we 
# fail to reject your null hypothesis and conclude that the 
# test statistic we observed may have been due to chance.
# If our p-value is less than our alpha, that means we 
# reject your null hypothesis and conclude that the test 
# statistic we observed was most likely not due to chance or,
# within the accepted low probability of it being chance.

# In our example we fail to reject the null hypothesis and conclude that the 
# increased conversion in landing page B may have been due to chance
 
# Confidence Intervals + R Functions

# Confidence intervals provide an alternative to reporting a single 
# test statistic and provide a summary measure of the uncertainty of 
# the test statistic.

# We calculate the 95% confidence interval manually with the following
# code:

pnorm(1.96, lower.tail = T)
upper_bound = (b_proportion - a_proportion) + 1.96*SE
lower_bound = (b_proportion - a_proportion) - 1.96*SE

upper_bound
lower_bound
# 1.96 is the z-score associated with a confidence level of 95%
# Many people incorrectly assume that a 95% confidence interval means 
# there is a 95% chance of our true population parameter being between 
# the two bounds.
# Confidence intervals are a measure of uncertainty that comes with 
# the sampling method. In repeated samples, some intervals would include
# the true population parameter and some would not, 
# 95% of those intervals calculated would include the true population 
# parameter.

# We can much more quickly perform the appropriate statistical 
# analysis using functions that exist in R. 
#  
# The t.test() function in R can quickly perform the statistical 
# analysis and recognizes the greater than 30 sample size, 
# thereby using the normal distribution.

t.test(b_obs, a_obs)


# Notice that p_value is equal to the R output's p-value divided by two.
# because t.test() defaults to a two-sided test, 
# this calculates the probability of getting a test statistic more 
# extreme on both sides of the normal distribution. 
# 
# Since in our experiment we are only interested in 
# the probability of getting the positive test statistic or 
# something more extreme as defined by our alternative hypothesis
# we only care about the area on the right side of the curve.
# Since the distribution is symmetric we can simply divide the p-value 
# by two. In R we can change which area under the curve we 
# calculate (p-value) by changing alternative to less or greater
# Observe the p-value in the t-test below to see that it equals 
# the p-value we manually calculated

t.test(b_obs, a_obs, alternative = "greater")


# Degrees of Freedom are the number of values calculated in our test 
# statistic that are free to vary.
# Suppose we calculated our test statistic with the equation: 
# Test Statistic = x + y + z. The variables x and y can technically be any 
# number, but once they are selected you have no choice (no freedom) to 
# choose the value for z. Therefore this equation would have two degrees 
# of freedom.