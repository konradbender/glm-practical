library(rsq)
library(MASS)
library(dplyr)
library(knitr)
setwd("/Users/konrad/code/school/MT/glm-practical")
source("helpers.R")
options(digits=3)

df.1 <- get_eda_data()

m1 <- glm(visits ~ . + gender*., data = df.1, family = poisson(link = log))


select_global_aic <- function(model, trace = FALSE) {
  stepAIC(model,
          scope = list(upper=visits ~ . + gender*.,
                       lower = ~1),
          trace = trace
  )
}

print_model_stats <- function(model, name){
  print(paste("Statistics for the model name:", name))
  print(paste("AIC of the model:", round(extractAIC(model)[2], 3)))
  print(paste("Deviance of the model:", round(model$deviance, 3)))
  print(paste("KL-R^2:", round(rsq.kl(model), 4)))
  print(paste("Number of coefficients (p):", length(model$coefficients)))
}

m2 <- stepAIC(m1,
              direction = "backward",
              scope = list(upper=visits ~ . + gender*.,
                     lower = ~1),
              trace = TRUE
)

print_model_stats(m1, "baseline")
print_model_stats(m2, "after AIC reduction")

lambda <- m2$deviance - m1$deviance
delta.df <- length(m1$coefficients) - length(m2$coefficients)
prob <- 1 - pchisq(lambda, delta.df)
print(paste("Difference in deviances:", round(lambda, 3)))

p.final <- length(m2$coefficients)
n <- dim(df.1)[1]
prob <- 1 - pchisq(m2$deviance, n - p.final)
print(paste("Probability of xi squared test:", round(prob, 3)))


plot(predict(m2,type="response"), rstandard(m2))
plot(predict(m2,type="link"), rstandard(m2))

deviance.resids <- rstandard(m2)
var(deviance.resids)

# Interpretation
estimates <- summary(m2)$coefficients[, c(1,2)]
estimates <- data.frame(estimates)
estimates[, "index"] <- rownames(estimates)
estimates[, "M"] <- exp(estimates[, "Estimate"])
estimates[, "beta_i"] <- 0:(dim(estimates)[1]-1)
estimates[, "beta_i"] <- sapply(estimates[, "beta_i"], FUN = function (row){
  paste0("i=", row)
})

c_interval <- data.frame(estimates[, "index"])
c_interval[, "CI.Estimate"] <- NA
c_interval[, "CI.M"] <- NA

for (i in 1:dim(c_interval)[1]){
  est <- estimates[i, "Estimate"]
  std <- estimates[i, "Std..Error"]
  c_interval[i, "CI.Estimate"] <- paste0("[", round(est - 1.96*std, 3), ", ",
                                      round(est + 1.96*std, 3),
                                      "]")
  c_interval[i, "CI.M"] <- paste0("[", round(exp(est - 1.96*std), 3), ", ",
                                           round(exp(est + 1.96*std), 3),
                                           "]")
}
c_interval <- data.frame(c_interval)
colnames(c_interval)[1] <- "index"

joined <- inner_join(estimates, c_interval, by="index")

joined <- joined[, c("index", "beta_i", "Std..Error", "Estimate", "CI.Estimate", "M", "CI.M")]

joined[joined$index == "age", "index"] <- "Age (for men)"
joined[joined$index == "age:genderfemale", "index"] <- "Age-Gender"

out <- kable(joined, "latex",
      caption="Parameter estimates and confidence intervals",
      midrule = "\\midrule", label = "estimates")

fileConn <- file("report/estimates.txt")
writeLines(out, fileConn)
close(fileConn)

varhat <- vcov(m2)

beta.a <- m2$coefficients[["age"]] + m2$coefficients[["age:genderfemale"]]
print(paste("Estimate for the female age coefficient:", round(beta.a, 4)))
var.beta.a <- varhat["age", "age"] + varhat["age:genderfemale", "age:genderfemale"] +
  2*varhat["age", "age:genderfemale"]
std.error.beta.a <- sqrt(var.beta.a)

print(paste0("Confidence interval for the age of a female: [",
            round(beta.a - std.error.beta.a, 4), ", ",
            round(beta.a + std.error.beta.a, 4),
            "]"))
odds.beta.a <- exp(beta.a)
print(paste("means ratio for the age of a female:", round(odds.beta.a, 4)))
print(paste0("Confidence interval for the means ratio: [",
             round(exp(beta.a - std.error.beta.a), 4), ", ",
             round(exp(beta.a + std.error.beta.a), 4),
             "]"))

pearson.phi_hat <- 1/(n-p.final) * sum(residuals.glm(m2, type = "pearson")^2)
manual.phi_hat <- 1/(n-p.final) * sum(residuals.glm(m2, type = "response")^2/
                                        predict.glm(m2, type = "response"))

print(paste("Phi Hat:", round(pearson.phi_hat, 3)))
# link <- eta
# response <- mu
