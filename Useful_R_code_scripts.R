#Useful R code scripts

#Downloading dataset from Github into working directory
githubURL <- "url"
download.file(githubURL,"file_name", method = "curl")

#Median Imputation of NA's
data_clean = data.frame(lapply(data,function(x) {
  if(is.numeric(x)) ifelse(is.na(x),median(x,na.rm=T),x) else x}))

#Change all NA's to 0s
na_to_zero <- function(x) {
  out <- x
  out[is.na(out)] <- 0
  out
}

#Change dates to days (from origin)
dates_to_days <- function(x, date_to, na_replacement){
  out <- x[[1]]
  out <- as.Date(as.character(out), format = '%Y%m%d')
  out <- as.numeric(as.Date(date_to) - out)
  if (!missing(na_replacement)) out[is.na(out)] <- na_replacement
  out
}

#Change number of months to number of days
months_to_days <- function(x, na_replacement){
  out <- x * 30
  if (!missing(na_replacement)) out[is.na(out)] <- na_replacement
  out
}

#Replacing a cell input of NA with 'NULL' and making it a factor (because sometimes NA as a factor causes problems)
addNA2 <- function(x, ifany = FALSE, replacement = 'NULL') {
  if (!is.factor(x))
    x <- factor(x)
  if (ifany & !anyNA(x))
    return(x)
  ll <- levels(x)
  if (!anyNA(ll))
    ll <- c(ll, replacement)
  x <- factor(x, levels = ll, exclude = NULL)
  x[is.na(x)] <- replacement
  x
}

#Evaluating function accuracy
evaluate <- function(actuals, predictions){
  cf.matrix <- table(actuals,predictions)
  cf.precision <- cf.matrix[2, 2] / sum(cf.matrix[, 2])
  cf.prop_miss <- cf.matrix[2, 1] / sum(cf.matrix[2, ])
  cf.accuracy <- (cf.matrix[1, 1] + cf.matrix[2, 2]) / sum(cf.matrix)
  cf.TruePositiveRate <- cf.matrix[2,2] / sum(cf.matrix[2, ])
  cf.FalsePositiveRate <- cf.matrix[1, 2] / sum(cf.matrix[1, ])
  cf.prevalence <- sum(cf.matrix[2, ]) / sum(cf.matrix)
  
  output <- list(cf.matrix,cf.precision,cf.prop_miss,cf.accuracy,cf.TruePositiveRate,cf.FalsePositiveRate,cf.prevalence)
  names(output) <- c('confusion matrix','precision','percent missed','accuracy','True Positive', 'False Positive', 'prevalence')
  return(output)
}

#Calculate the number of NA's per variable
navars <- function(df){
  sapply(df, function(x){sum(is.na(x))})
}

#Calculate the number of rows that contain NAs
narows <- function(df){
  out <- c()
  naframe <- is.na(df)
  for (r in nrow(naframe)) {
    out[r] <- any(naframe[r,])
  }
  return(out)
}