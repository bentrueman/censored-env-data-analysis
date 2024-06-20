
# functions ---------------------------------------------------------------

modify_stan_template <- function(standatalist) {
  # get column numbers of censored variables:
  index_cens  <- get_censored_column_numbers(standatalist)
  # get column numbers of variables with missings:
  index_mi  <- get_missings_column_numbers(standatalist)
  # starter code:
  code_template <- readLines(here("R/stan/ppca.stan"))
  # build additional code for censored data:
  declare_Ncens <- build_Ncens(index_cens )
  declare_Jcens <- build_Jcens(index_cens )
  declare_U <- build_U(index_cens )
  declare_Ycens <- build_Ycens(index_cens )
  declare_yl_cens <- build_yl_cens(index_cens )
  # build additional code for missing data:
  declare_Nmi <- build_Nmi(index_mi )
  declare_Jmi <- build_Jmi(index_mi )
  declare_Ymi <- build_Ymi(index_mi )
  declare_yl_mi <- build_yl_mi(index_mi )
  # add to template:
  code <- c(
    code_template[3:7], # data block
    # additions to data block:
    "  // data for imputation of missings:",
    declare_Nmi,
    declare_Jmi,
    "  // data for censored imputation:",
    declare_Ncens,
    declare_Jcens,
    declare_U,
    code_template[8:14], # parameters data block
    # additions to parameters block:
    "  // missing value parameters:",
    declare_Ymi,
    "  // censored value parameters:",
    declare_Ycens,
    code_template[15],
    # add a transformed parameters block:
    "transformed parameters {",
    "  // combine observed with estimated censored:",
    "  array[N] vector[D] yl = y;",
    declare_yl_mi,
    declare_yl_cens,
    "}",
    code_template[16:26], # model block
    "    yl[n] ~ normal(w * col(z, n) + mu, sigma);",
    code_template[28] # model block
  )
  if (length(index_cens) == 0 && length(index_mi) == 0) code_template else code
}

build_standatalist <- function(data, components, censoring) {

  n <- nrow(data)

  data_list <- list(
    N = n,
    D = ncol(data),
    M = components,
    y = data
  )

  # number of missings:
  missings <- data.frame(apply(data, 2, is.na))
  Nmi <- lapply(missings, sum)
  names(Nmi) <- paste0("Nmi_y", seq(length(Nmi)))

  # positions of missings:
  index <- seq(n)
  Jmi <- lapply(missings, \(x) index[x])
  names(Jmi) <- paste0("Jmi_y", seq(length(Jmi)))

  # number of censored values:
  Ncens <- lapply(censoring, sum)
  names(Ncens) <- paste0("Ncens_y", seq(length(Ncens)))

  # positions of censored values:
  index <- seq(n)
  Jcens <- lapply(censoring, \(x) index[x])
  names(Jcens) <- paste0("Jcens_y", seq(length(Jcens)))

  # censoring limits:
  U <- mapply(\(x, y) x[y], data, censoring, SIMPLIFY = FALSE)
  names(U) <- paste0("U_y", seq(length(U)))

  # remove empty list elements because no censoring/missings for a particular variable:
  U <- U[Ncens > 0]
  Jcens <- Jcens[Ncens > 0]
  Ncens <- Ncens[Ncens > 0]
  Jmi <- Jmi[Nmi > 0]
  Nmi <- Nmi[Nmi > 0]

  # convert NAs to Inf for Stan:
  data_list$y <- data.frame(apply(data_list$y, 2, \(x) ifelse(is.na(x), Inf, x)))

  c(data_list, Ncens, Jcens, U, Nmi, Jmi)

}

# helper functions --------------------------------------------------------

# code snippets for censoring:

get_censored_column_numbers <- function(standatalist) {
  # get names of stan data list elements:
  names_standata <- names(standatalist)
  # retain names of list elements containing numbers of censored observations:
  names_ncens <- names_standata[grepl("Ncens_y", names_standata)]
  # retain names of list elements containing positions of censored observations:
  names_jcens <- names_standata[grepl("Jcens_y", names_standata)]
  # retain names of list elements containing censoring limits:
  names_u <- names_standata[grepl("U_y", names_standata)]
  # extract column numbers from the names extracted above:
  columns_ncens <- regmatches(names_ncens, m = regexpr("\\d+", names_ncens))
  columns_jcens <- regmatches(names_jcens, m = regexpr("\\d+", names_jcens))
  columns_u <- regmatches(names_u, m = regexpr("\\d+", names_u))
  # all three sets of column numbers should be the same:
  stopifnot(
    "stan data list must include corresponding variables for position,
    count, and upper limits of censored observations" =
      all(duplicated(list(columns_ncens, columns_jcens, columns_u))[-1])
  )
  as.numeric(columns_ncens)
}

build_Ncens <- function(index) {
  output <- sapply(
    index,
    \(x) paste0("  int<lower=0> Ncens_y", x, "; // number of censored, y", x)
  )
  if (length(output) == 0) character(0L) else output
}

build_Jcens <- function(index) {
  output <- sapply(
    index,
    \(x) paste0("  array[Ncens_y", x, "] int<lower=1> Jcens_y", x, "; // positions of censored, y", x)
  )
  if (length(output) == 0) character(0L) else output
}

build_U <- function(index) {
  output <- sapply(
    index,
    \(x) paste0("  array[Ncens_y", x, "] real U_y", x, "; // left-censoring limits, y", x)
  )
  if (length(output) == 0) character(0L) else output
}

build_Ycens <- function(index) {
  output <- sapply(
    index,
    \(x) paste0("  array[Ncens_y", x, "] real<lower=0, upper=U_y", x, "> Ycens_y", x, "; // estimated censored, y", x)
  )
  if (length(output) == 0) character(0L) else output
}

build_yl_cens <- function(index) {
  output <- sapply(
    index,
    \(x) paste0("  yl[Jcens_y", x, ", ", x, "] = Ycens_y", x, ";")
  )
  if (length(output) == 0) character(0L) else output
}

# code snippets for missings:

get_missings_column_numbers <- function(standatalist) {
  # get names of stan data list elements:
  names_standata <- names(standatalist)
  # retain names of list elements containing numbers of missings:
  names_nmi <- names_standata[grepl("Nmi_y", names_standata)]
  # retain names of list elements containing positions of missings:
  names_jmi <- names_standata[grepl("Jmi_y", names_standata)]
  # extract column numbers from the names extracted above:
  columns_nmi <- regmatches(names_nmi, m = regexpr("\\d+", names_nmi))
  columns_jmi <- regmatches(names_jmi, m = regexpr("\\d+", names_jmi))
  # both sets of column numbers should be the same:
  stopifnot(
    "stan data list must include corresponding variables for position,
    and counts of missings" =
      all(columns_nmi == columns_jmi)
  )
  as.numeric(columns_nmi)
}

build_Nmi <- function(index) {
  output <- sapply(
    index,
    \(x) paste0("  int<lower=0> Nmi_y", x, "; // number of missings, y", x)
  )
  if (length(output) == 0) character(0L) else output
}

build_Jmi <- function(index) {
  output <- sapply(
    index,
    \(x) paste0("  array[Nmi_y", x, "] int<lower=1> Jmi_y", x, "; // positions of missings, y", x)
  )
  if (length(output) == 0) character(0L) else output
}

build_Ymi <- function(index) {
  output <- sapply(
    index,
    \(x) paste0("  array[Nmi_y", x, "] real Ymi_y", x, "; // estimated missings, y", x)
  )
  if (length(output) == 0) character(0L) else output
}

build_yl_mi <- function(index) {
  output <- sapply(
    index,
    \(x) paste0("  yl[Jmi_y", x, ", ", x, "] = Ymi_y", x, ";")
  )
  if (length(output) == 0) character(0L) else output
}
