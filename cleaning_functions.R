
#funtional operator
# apply_to_columns <- function(f){
#     force(f)
#     function(df, col_vec, ...){
#         out <- df
#         for(v in seq_along(col_vec)){
#             out[col_vec[v]] <- f(df[col_vec[v]], ...)
#         }
#         out
#     }
# }


apply_to_columns <- function(data, columns = rep(TRUE, ncol(data)), f, dropstr = FALSE, ...){
    if (is.logical(columns)) columns <- colnames(data)[columns]
    out <- data
    for (v in columns) {
        out[[v]] <- f(data[[v]], ...)
    }
    out
}


add_dummys <- function(data, columns = rep(TRUE, ncol(data)), label = '_dummy'){
    if (is.logical(columns)) columns <- colnames(data)[columns]
    out <- data
    for (v in seq_along(columns)) {
        out[paste0(columns[v], label)] <- is.na(data[,columns[v], drop = F])
    }
    out
}

na_to_zero <- function(x) {
    out <- x
    out[is.na(out)] <- 0
    out
}

dates_to_days <- function(x, date_to, na_replacement){
    out <- x[[1]]
    out <- as.Date(as.character(out), format = '%Y%m%d')
    out <- as.numeric(as.Date(date_to) - out)
    if (!missing(na_replacement)) out[is.na(out)] <- na_replacement
    out
}

months_to_days <- function(x, na_replacement){
    out <- x * 30
    if (!missing(na_replacement)) out[is.na(out)] <- na_replacement
    out
}

na_to_value <- function(x, values) {
    out <- x
    out[is.na(out)] <- values[names(x)]
    out
}

make_factor <- function(x, replacement = 'none'){
    out <- x
    if (!missing(replacement)) out[is.na(out)] <- replacement
    as.factor(out)
}

library(rpart)

controls <- rpart.control(minsplit = 1, minbucket = 20, cp = 0, maxsurrogate = 0, usesurrogate = 0, maxdepth = 3, xval = 0)
bin <- function(x, response) {
    data <- data.frame(response, x)
    tree <- rpart(response~., data = data, control = controls)
    splits <- tree$splits[ , 'index']
    if (length(splits) < 2) {return(as.factor(x))}
    out <- cut(x, breaks = splits)
    out
}

get_splits <- function(x, response) {
    data <- data.frame(response, x)
    tree <- rpart(response~., data = data, control = controls)
    splits <- tree$splits[ , 'index']
    splits <- c(-Inf, splits, Inf)
    splits
}

apply_splits <- function(x, splitlist){
    bare <- x[[1]]
    splits <- splitlist[names(x)][[1]]
    #if (length(splits) < 2) {return(as.factor(bare))}
    out <- cut(bare, breaks = splits)
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

using <- function(arg, f, ...){
    f(arg, ...)
}