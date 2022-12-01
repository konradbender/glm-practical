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
make_ci <- function(row) {
  paste("[", round(row[["Estimate"]] - 1.96*row[["Std. Error"]], 3), ",",
        round(row[["Estimate"]] + 1.96*row[["Std. Error"]], 3),
  "]"
  )
}
c_interval <- apply(estimates, FUN = make_ci, MARGIN = 1)
c_interval <- data.frame(c_interval)
c_interval[, "index"] <- rownames(c_interval)

estimates <- data.frame(estimates)
estimates[, "index"] <- rownames(estimates)
estimates[, "odds ratio"] <- exp(estimates[, "Estimate"])
estimates[, "beta_i"] <- 0:(dim(estimates)[1]-1)
estimates[, "beta_i"] <- sapply(estimates[, "beta_i"], FUN = function (row){
  paste0("i=", row)
})

joined <- inner_join(estimates, c_interval, by="index")

joined <- joined[, c(3, 5, 1, 2, 4, 6)]

out <- kable(joined, "latex",
      caption="Parameter estimates and confidence intervals",
      midrule = "\\midrule", label = "estimates")

fileConn <- file("report/estimates.txt")
writeLines(out, fileConn)
close(fileConn)