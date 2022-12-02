setwd("/Users/konrad/code/school/MT/glm-practical")
source("helpers.R")
library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)
library(knitr)

options(digits = 3)

# Define some constants - widths and heights in inches
FONT_SIZE <- 7
DOUBLE_FONT_SIZE <- 8
WIDTH <- 4
DOUBLE_WIDTH <- 6
DOUBLE_HEIGHT <- 3
HEIGHT <- 2

# use method from helper package
df <- get_eda_data()

## SECTION 1: PLOTTING ##

# Make a histogram of all the reponses together
ggplot(df) +
  geom_histogram(aes(visits)) +
  geom_vline(xintercept = mean(df$visits)) +
  ggtitle(paste("Number of doctor visists across all individuals | Mean =",
                round(mean(df$visits), 3))) +
  theme(text = element_text(size = FONT_SIZE)) +
  xlab("Visits") +
  ylab("Count of Respondents") +
  xlim(-0.25, 8)
ggsave("plots/histogram_of_visits.pdf",
       width = WIDTH, height = HEIGHT)

# Make plots by income and age
# First: group data by income only
df.grouped <- df %>%
  group_by(income) %>%
  summarise(n(), max(visits), min(visits), mn = mean(visits))
p1 <- ggplot(df.grouped, aes(income, mn)) + geom_point()
p1 <- p1 + ggtitle("Mean visists as a function of income")
p1 <- p1 +
  theme(text = element_text(size = DOUBLE_FONT_SIZE)) +
  xlab("Income") +
  ylab("Mean Visits")

# Second: group data by age only
df.grouped <- df %>%
  group_by(age) %>%
  summarise(n(), max(visits), min(visits), mn = mean(visits))
p2 <- ggplot(df.grouped, aes(age, mn)) + geom_point()
p2 <- p2 + ggtitle("Mean doctor visists as a function of age")
p2 <- p2 +
  theme(text = element_text(size = DOUBLE_FONT_SIZE)) +
  xlab("Age") +
  ylab("Mean Visits")

ggsave("plots/mean_vs_income_and_age.pdf",
       arrangeGrob(p1, p2, nrow = 1, ncol = 2),
       width = DOUBLE_WIDTH, height = HEIGHT)

# Make a histogram how the insurance influences how often people go
# mean number of visits for different insurance types
ins.means <- df %>%
  group_by(insurance) %>%
  summarise(mn = mean(visits))
labels <- list()
for (value in ins.means$insurance) {
  m <- ins.means[ins.means$insurance == value, "mn"]
  label <- paste("Insurance:", value, "| Mean =", round(m, 4))
  labels[value] <- label
}
# now make a histogram divided by insurance type
p <- ggplot(df, aes(visits)) +
  geom_histogram() +
  facet_wrap(~insurance, labeller = as_labeller(unlist(labels))) +
  geom_vline(aes(xintercept = mn), ins.means) +
  xlim(-0.1, 5) +
  ggtitle("Number of doctor visits for different insurance types") +
  theme(text = element_text(size = DOUBLE_FONT_SIZE)) +
  xlab("Visits") +
  ylab("Count of respondents")

ggsave("plots/histograms_of_insurances.pdf",
       width = DOUBLE_WIDTH, height = DOUBLE_HEIGHT)


# Make a histogram how the gender influences how often people go
means <- df %>%
  group_by(gender) %>%
  summarise(mn = mean(visits))
labels <- list()
for (value in means$gender) {
  mean <- means[means$gender == value, "mn"]
  label <- paste("Gender:", value, "| Mean =", round(mean, 4))
  labels[as.character(value)] <- label
}
p <- ggplot(df, aes(visits)) +
  geom_histogram() +
  facet_wrap(~gender, labeller = as_labeller(unlist(labels))) +
  geom_vline(aes(xintercept = mn), means) +
  xlim(-0.1, 5) +
  theme(text = element_text(size = DOUBLE_FONT_SIZE)) +
  xlab("Visits") +
  ylab("Count of Respondents")
ggtitle("Number of doctor visits for different genders")
ggsave("plots/histograms_of_genders.pdf",
       width = DOUBLE_WIDTH, height = HEIGHT)

# Make a plot that shows the chronic diseases impact
df.grouped <- df %>%
  group_by(age, lchronic) %>%
  summarise(n(), max(visits), min(visits), mn = mean(visits))
ggplot(df.grouped, aes(age, mn, color = lchronic)) +
  geom_point() +
  theme(text = element_text(size = FONT_SIZE)) +
  ggtitle("Mean visits vs. age for chronicly sick
    people and healthy people") +
  xlab("Age") +
  ylab("Mean Visits") +
  labs(colour = "Chronic\nDisease")
ggsave("plots/mean_vs_age_and_chronic.pdf",
       width = WIDTH, height = HEIGHT)

## SECTION 2: CREATING SOME SUMMARY STATISTICS ##
df.grouped <- df %>%
  group_by(gender, insurance) %>%
  summarise(n = n(),
            "mean age" = mean(age),
            "mean visits" = mean(visits),
            "variance of visits" = var(visits)
  )
# Output table to Latex
latex.table <- kable(df.grouped, "latex",
                     caption = "Descriptive statistics of
                     the dataset by gender and insurance",
                     label = "stats_fine")

fileConn <- file("report/descriptive_stats_fine.txt")
writeLines(latex.table, fileConn)
close(fileConn)

df.grouped <- df %>%
  group_by(gender) %>%
  summarise(n = n(),
            "mean age" = mean(age),
            "mean visits" = mean(visits),
            "variance of visits" = var(visits)
  )
latex.table <- kable(df.grouped, "latex",
                     caption = "Descriptive statistics of the
                     dataset by gender only",
                     label = "stats_coarse")

fileConn <- file("report/descriptive_stats_coarse.txt")
writeLines(latex.table, fileConn)
close(fileConn)