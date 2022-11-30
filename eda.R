setwd("/Users/konrad/code/school/MT/glm-practical")
source("helpers.R")
library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)

FONT_SIZE <- 12

data <- get_eda_data()
head(data)

df <- data
# a lot of respondents did not go to the doctor in the previous
# two weeks at all.
ggplot(df) + geom_histogram(aes(visits)) +
  theme(text = element_text(size=FONT_SIZE))
ggsave("plots/histogram_of_visits.pdf")



# group data by income and insurance
# we see that with reduced income, the mean number of doctor visits increases
df.grouped <- df %>% group_by(income, insurance) %>%
  summarise(n(), max(visits), min(visits), mn = mean(visits))
ggplot(df.grouped, aes(income, mn, color=insurance)) + geom_point()
ggsave("plots/mean_vs_income_and_private.pdf")

df.grouped <- df %>% group_by(age, insurance) %>%
  summarise(n(), max(visits), min(visits), mn = mean(visits))
ggplot(df.grouped, aes(age, mn, color=insurance)) + geom_point()
ggsave("plots/mean_vs_age_and_insurance.pdf")

# group data by income only
# we see that with reduced income, the mean number of doctor visits increases

df.grouped <- df %>% group_by(income) %>%
  summarise(n(), max(visits), min(visits), mn = mean(visits))
p1 <- ggplot(df.grouped, aes(income, mn)) + geom_point()
p1 <- p1 + ggtitle("Mean doctor visists as a function of income")
p1 <- p1 + theme(text = element_text(size=FONT_SIZE))
ggsave("plots/mean_vs_age.pdf")

# group data by age only
# we see that with reduced income, the mean number of doctor visits increases
df.grouped <- df %>% group_by(age) %>%
  summarise(n(), max(visits), min(visits), mn = mean(visits))
p2 <- ggplot(df.grouped, aes(age, mn)) + geom_point()
p2 <- p2 + ggtitle("Mean doctor visists as a function of age")
p2 <- p2 + theme(text = element_text(size=FONT_SIZE))
ggsave("plots/mean_vs_age.pdf")

ggsave("plots/mean_vs_income_and_age.pdf", arrangeGrob(p1, p2, nrow = 1, ncol = 2),
       width =  15, height = 10)

# let's make a histogram how the type of insurance influences how often you go
means <- df %>% group_by(insurance) %>% summarise(mn = mean(visits))
labels <- list()
for (value in means$insurance){
  mean <- means[means$insurance == value, "mn"]
  label <- paste("Insurance:", value, "| Mean =", round(mean, 4))
  labels[value] <- label
}
p <- ggplot(df, aes(visits)) + geom_histogram() +
  facet_wrap(~insurance, labeller = as_labeller(unlist(labels)))
p <- p + geom_vline(aes(xintercept=mn), means) + xlim(-0.1,5) +
  theme(text = element_text(size=FONT_SIZE))
print(p)
ggsave("plots/histograms_of_insurances.pdf", width = 10, height = 10)


# let's make a histogram how the gender influences how often you go
means <- df %>% group_by(female) %>% summarise(mn = mean(visits))
labels <- list()
for (value in means$female){
  mean <- means[means$female == value, "mn"]
  label <- paste("Female:", value, "| Mean =", round(mean, 4))
  labels[as.character(value)] <- label
}
p <- ggplot(df, aes(visits)) + geom_histogram() +
  facet_wrap(~female, labeller = as_labeller(unlist(labels)))
p <- p + geom_vline(aes(xintercept=mn), means) + xlim(-0.1,5) +
  theme(text = element_text(size=FONT_SIZE))
print(p)
ggsave("plots/histograms_of_genders.pdf", width = 10, height = 10)

# make a plot that shows the chronic diseases
df.grouped <- df %>% group_by(age, lchronic) %>%
  summarise(n(), max(visits), min(visits), mn = mean(visits))
ggplot(df.grouped, aes(age, mn, color=lchronic)) + geom_point() +
  theme(text = element_text(size=FONT_SIZE)) +
  ggtitle("Mean visits vs. age for chronicly sick people and healthy people")
ggsave("plots/mean_vs_age_and_chronic.pdf", width = 10, height = 6)
