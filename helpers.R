library(dplyr)

get_eda_data <- function() {
  df <- read.csv("data/docvis.csv")
  df <- df %>%
    mutate(female = replace(female, female == 1, "female")) %>%
    mutate(female = replace(female, female == 0, "male"))
  df$female <- factor(df$female, levels = c("male", "female"))
  names(df)[names(df) == 'female'] <- 'gender'
  df$lchronic <- as.factor(df$lchronic)
  df[, "insurance"] <- "normal"
  df <- within(df, {
    insurance[df$private == 1] <- "private"
    insurance[df$freepoor == 1] <- "freepoor"
    insurance[df$freerepat == 1] <- "freerepat"
  } )

  df$insurance <- factor(df[, "insurance"],
                         levels = c("normal", "private", "freepoor", "freerepat"))

  subset(df, select = -c(private, freepoor, freerepat))
}