library(rsq)
library(MASS)
source("helpers.R")


df <- get_data()
df.eda <- get_eda_data()
head(df)
head(df.eda)

m1 <- glm(visits ~ . + gender*., data = df.eda, family = poisson(link = log))

df.eda$age_sq <- df.eda$age^2
df.eda$income_sq <- df.eda$income^2
m2 <- glm(visits ~ . + gender*., data = df.eda, family = poisson(link = log))

selected.2 <- stepAIC(m2,
                    scope = list(upper=visits ~ . + gender*.,
                                 lower = ~1),
                    trace = TRUE
)

summary(selected.2)

selected.1 <- stepAIC(m1,
                      scope = list(upper=visits ~ . + gender*.,
                                   lower = ~1),
                      trace = TRUE
)

summary(selected.1)