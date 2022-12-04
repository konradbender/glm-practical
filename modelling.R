library(rsq)
library(MASS)
library(dplyr)
library(knitr)
setwd("/Users/konrad/code/school/MT/glm-practical")
source("helpers.R")
options(digits = 3)

# SECTION 1: DEFINE HELPER FUNCTION

print_model_stats <- function(model, name) {
  print("-------------------")
  print(paste("Statistics for the model name:", name))
  print(paste("AIC of the model:", round(extractAIC(model)[2], 3)))
  print(paste("Deviance of the model:", round(model$deviance, 3)))
  print(paste("KL-R^2:", round(rsq.kl(model), 4)))
  print(paste("Number of coefficients (p):", length(model$coefficients)))
  print("===================")
}

## SECTION 2: LOAD DATA AND START MODELING
df <- get_eda_data()

# model 1: Baseline with all features
m1 <- glm(visits ~ . + gender * ., data = df, family =
  poisson(link = log))

# model 2: Eliminate some features with backwards AIC search
m2 <- stepAIC(m1,
              direction = "backward",
              scope = list(upper = visits ~ . + gender * .,
                           lower = ~1),
              trace = FALSE
)

print_model_stats(m1, "baseline")
print_model_stats(m2, "after AIC reduction")

# Conduct the hypothesis test if th suggested coefficients are 0
lambda <- m2$deviance - m1$deviance
delta.df <- length(m1$coefficients) - length(m2$coefficients)
prob <- 1 - pchisq(lambda, delta.df)
paste("Difference in deviances between the two models:",
            round(lambda, 3))
paste("Probability of xi squared test comparing the models:",
            round(prob, 3))


## SECTION 3: ASSESS THE MODEL FIT
# Conduct the xi squared test for the model
p.final <- length(m2$coefficients)
n <- dim(df)[1]
prob <- 1 - pchisq(m2$deviance, n - p.final)
paste("Probability of xi squared test for the final model",
            round(prob, 3))

# Analyze standardized deviance residuals of the model
deviance.resids <- rstandard(m2)
paste("Variance of standardizes deviance residuals",
            var(deviance.resids))

## SECTION 4: INTERPRET THE MODEL
# create a dataframe with the coefficients and the beta numbers
estimates <- summary(m2)$coefficients[, c(1, 2)]
estimates <- data.frame(estimates)
estimates[, "index"] <- rownames(estimates)
estimates[, "Coefficient"] <- 0:(dim(estimates)[1] - 1)
estimates[, "Coefficient"] <- sapply(estimates[, "Coefficient"],
                                     FUN = function(row) {
                                       paste0("beta_", row)
                                     })

# compute the means ratio
estimates[, "M"] <- exp(estimates[, "Estimate"])

# compute 95% Confidence intervals
c_interval <- data.frame(estimates[, "index"])
c_interval[, "CI.Estimate"] <- NA
c_interval[, "CI.M"] <- NA
for (i in 1:dim(c_interval)[1]) {
  est <- estimates[i, "Estimate"]
  std <- estimates[i, "Std..Error"]
  c_interval[i,
             "CI.Estimate"] <- paste0("[",
                                      round(est - 1.96 * std, 3), ", ",
                                      round(est + 1.96 * std, 3),
                                      "]")
  c_interval[i, "CI.M"] <- paste0("[",
                                  round(exp(est - 1.96 * std), 3), ", ",
                                  round(exp(est + 1.96 * std), 3),
                                  "]")
}
c_interval <- data.frame(c_interval)
colnames(c_interval)[1] <- "index"

# join the estimates and the confidence intervals
joined <- inner_join(estimates, c_interval, by = "index")

# select relevant columns and rename columns or features for the report
joined <- joined[, c("index", "Coefficient", "Std..Error",
                     "Estimate", "CI.Estimate", "M", "CI.M")]

joined[joined$index == "age", "index"] <- "age (for men)"
joined[joined$index == "age:genderfemale", "index"] <- "age-female"

joined <- joined %>% rename(
  Feature = index,
  "SE" = "Std..Error",
  "CI \\hat{beta}" = CI.Estimate,
  "MR" = M,
  "CI MR" = CI.M,
  "\\hat{beta}" = Estimate
)

joined[joined$Feature == "genderfemale", "Feature"] <- "female"
joined[joined$Feature == "lchronic1", "Feature"] <- "chronic disease"
joined[joined$Feature == "insuranceprivate", "Feature"] <-
  "private insurance"
joined[joined$Feature == "insurancefreepoor", "Feature"] <-
  "freepoor insurance"
joined[joined$Feature == "insurancefreerepat", "Feature"] <-
  "freepat insurance"

# output to Latex
out <- kable(joined, "latex",
             caption = "Parameter estimates and means ratios (MR)
             with their 0.95 confidence intervals",
             label = "estimates")

fileConn <- file("report/estimates.txt")
writeLines(out, fileConn)
close(fileConn)

# find the standard error for the influence of the age for women
varhat <- vcov(m2)
beta.f.age <- m2$coefficients[["age"]] + m2$coefficients[["age:genderfemale"]]

paste("Estimate for the female age
  coefficient:", round(beta.f.age, 4))

var.beta.f.age <- varhat["age", "age"] +
  varhat["age:genderfemale", "age:genderfemale"] +
  2 * varhat["age", "age:genderfemale"]
se.beta.f.age <- sqrt(var.beta.f.age)

# compute confidence interval and odds ratio for the influence
# of age for femalse
paste0("Confidence interval for the age of a female: [",
             round(beta.f.age - 1.96 * se.beta.f.age, 4), ", ",
             round(beta.f.age + 1.96 * se.beta.f.age, 4),
             "]")
odds.beta.a <- exp(beta.f.age)
paste("means ratio for the age of a female:", round(odds.beta.a, 4))
paste0("Confidence interval for the means ratio of the female age: [",
             round(exp(beta.f.age - 1.96 * se.beta.f.age), 4), ", ",
             round(exp(beta.f.age + 1.96 * se.beta.f.age), 4),
             "]")

## SECTION 5: ESTIMATE DISPERSION
pearson.phi_hat <- 1 / (n - p.final) * sum(residuals.glm(m2,
                                                         type = "pearson")^2)

paste("Phi Hat:", round(pearson.phi_hat, 3))