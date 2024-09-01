
##### R Project 5: Central Limit Theorem
##### Name: Hanel Vujic
##### Version Number: 4



## (C1) Load workspace.



## (C2) Table of trusted
# Code

table(trusted)

# Copy and paste results here

#ABC News                 BBC            CBS News                 CNN           Economist            Facebook 
#420                  279                 114                      677                  51                 100 
#Fox News     Huffington Post            LA Times     Local Newspaper               MSNBC            NBC News 
#543                  64                  55                  68                     93                 124 
#New York Times                 NPR               Other        The Guardian             Twitter           USA Today 
#291                            200                 452                  33                 196                  75 
#Wall Street Journal     Washington Post               Yahoo 
#97                                 115                  54 

# What trusted news network is trusted by the largest number of people?
#(ANSWER): CNN is with a value of 677


# What trusted news network is trusted by the least number of people?
#(ANSWER): The Guardian with only 33 people.






## (C3) Find estimates for the Mean, Variance, Standard Deviation Estimates for dollars using Built-In R Functions

# Mean Code

mean(dollars)

# Copy and paste mean results here
# 8.853984


# Variance Code
var(dollars)


# Copy and paste variance results here
# 2250.183

# Standard Deviation Code
sd(dollars)


# Copy and paste standard deviation results here

# 47.43609





## (C4) Histogram of dollars
## Remember to save your plot and also submit it to Gradescope.

# Code:
hist(dollars, breaks = c( 0, 75, 150, 225, 300, 375,450, 525, 600, 675, 750, 825, 900, 975, 1050), xlim = c(0, 1200))


# Describe your histogram:

#It is a right-skewed histogram.




# (C5) Shapiro-Wilk test on dollars

# Code:

shapiro.test(dollars)


# Copy and paste results here

#Shapiro-Wilk normality test

#data:  dollars
#W = 0.17576, p-value < 2.2e-16


# Do you think that your population is normally distributed?

#ANSWER: No, the data is not normally distributed.

# Reason: 

#(ANSWER): Our p-value is < 2.2e-16, which is way less than 0.05, making our data not normally distributed.



# (C6) Copy and paste function, then run it. (After you run it, you'll see a new function added in the Environment window on the right side.)

sampling_jp <- function(dataset, seed_value, num_samples, sizejp){
  samples1 <- matrix(NA, nrow = num_samples, ncol = sizejp)
  set.seed(seed_value)
  for(i in 1:num_samples){
    samples1[i,] <- sample(dataset, size = sizejp, replace = TRUE)
  }
  rowmeans1 <- rowMeans(samples1)
  graph <- hist(rowmeans1, right = FALSE, xlab = "Sample Means",
                main = "Histogram",
                sub = paste("Size = ", sizejp))
  sw <- shapiro.test(rowmeans1)
  result <- list(SampleMeans = rowmeans1, Shapiro = sw, Histogram = graph)
  return(result)
}







# (Q7) Give a brief description of num_samples if the value for it is 25.

# Number of Samples (more information than "number of samples" is needed):

#(ANSWER): It's just the number of samples. Just kidding.
# The sample size affects the variability in our result and it would affect
# how our graph would look. 





# (Q8) Describe what each individual part is doing in (C6) for code provided


# sample() does:

#(ANSWER): The sample() function is appropriately named as it takes a sample of whatever is inputted as its x
# or sample data with or without replacement (meaning all our sample values can be independent or not)


# dataset does:

#(ANSWER) Holds the elements from which we'll be choosing from


# size = sizejp does (also include what sizejp means):

#(ANSWER) Must be non-negative and it specifies how many elements we'll be choosing from

# sizejp is the variable we'll be using as the argument for size in this case, the number of items chosen

# replace = TRUE does:

#(ANSWER) allows replacement to be used






# (C9) Build sampling distribution of samples of dollars
## Remember to save your histogram and also submit it to Gradescope.

# Code:
run1 <- sampling_jp(dataset = dollars, seed_value = 503, num_samples = 178, sizejp = 11)
run1


# Copy and paste Shapiro-Wilk test results here

``# $Shapiro

# Shapiro-Wilk normality test

# data:  rowmeans1
# W = 0.63827, p-value < 2.2e-16



# (Q10) From (C9): Does it appear normally distributed?  Why or why not?
# Normally Distributed (Yes or No): No

# Reason using histogram:
# The graph is grossly right-skewed and a normal distribution must have zero skewness.



# Reason using the Shapiro-Wilk Test:
# (ANSWER): The p-value from our Shapiro-Wilk test is less than 2.2e-16, which is way less than 0.05.





# (C11) Build sampling distribution of samples of dollars
## Remember to save your histogram and also submit it to Gradescope.

# Code:

run2<-sampling_jp(dataset = dollars, seed_value = 503, num_samples = 178, sizejp = 782)
run2

# Copy and paste Shapiro-Wilk test results here

#$Shapiro

#Shapiro-Wilk normality test

#data:  rowmeans1
#W = 0.99491, p-value = 0.806



# (Q12) From (C11): Does it appear normally distributed?  Why or why not?
# Normally Distributed (Yes or No): Yes

# Reason using histogram: The histogram clearly looks more normally distributed compared to our
# previous histograms.



# Reason using the Shapiro-Wilk Test:

# Our p-value is 0.806, way above the 0.05 threshold we needed.



# (Q13) CLT?
# Do your results in (C9) violate the CLT?: YES

# Reason: Our size is 11, which is less than 30, and our histogram was rightly skewed, 
# thus not allowing the CLT to apply.


# Does the CLT hold for (C11)?: YES

# Reason: Our size was 782, way above 30, and our histogram was normally distributed.



# (Q14) Expected Value of the Sample Mean using CLT formulas

# Code for first sample size:
mean(dollars)



# Copy and paste results here
# 8.853984



# Code for second sample size:
mean(dollars)


# Copy and paste results here

# 8.853984



# (Q15) Standard Deviation of the Sample Mean using CLT formulas

# Code for first sample size:

standDevRun1 <- sd(dollars)/sqrt(11)
standDevRun1


# Copy and paste results here

#14.30252


# Code for second sample size:

standDevRun2 <- sd(dollars)/sqrt(782)
standDevRun2


# Copy and paste results here

#1.696311



# (C16) Average of the Sample Means from C9 and C11

# Code for average of sample means from C9

mean(run1$SampleMeans)


# Copy and paste results here

#10.51098


# Code for average of sample means from C11

mean(run2$SampleMeans)

# Copy and paste results here

#9.199955




# (C17) Standard Deviation of the Sample Means from C9 and C11

# Code for standard deviation of sample means from C9

sd(run1$SampleMeans)


# Copy and paste results here

#16.99159

# Code for standard deviation of sample means from C11

sd(run2$SampleMeans)


# Copy and paste results here

#1.706134



# (Q18) Does the CLT approximation appear to get better as the sample size increased? Why or why not?

# Does it get better?

#Yes it does get better

# Reason: The more the sample size increases, the more it becomes normally distributed. Our increase in run2
# also had a size way greater than 30. 





# (Q19)
# What was the main sample statistic used?

#Main sample mean

# When we found the sample statistic value, did we find it from the same population each time?

#Yes, we used the dollars population

