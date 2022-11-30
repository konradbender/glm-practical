get_data <- function() {
  data <- read.csv("data/docvis.csv")
  for (col in c("female", "private", "freepoor", "freerepat", "lchronic")){
    data[, col] <- as.factor(data[, col])
  }
  data
}

get_eda_data <- function() {
  df <- read.csv("data/docvis.csv")
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