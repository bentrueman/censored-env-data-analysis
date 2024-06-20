
# functions ---------------------------------------------------------------

modify_stan_template <- function(standatalist) {
  # get column names of censored variables:
  index <- get_censored_column_numbers(standatalist)
  # starter code:
  code_template <- readLines(here("R/stan/ppca.stan"))
  # build additional code:
  declare_Ncens <- build_Ncens(index)
  declare_Jcens <- build_Jcens(index)
  declare_U <- build_U(index)
  declare_Ycens <- build_Ycens(index)
  declare_yl <- build_yl(index)
  # add to template:
  code <- c(
    code_template[3:7], # data block
    # additions to data block:
    "  // data for censored imputation:",
    declare_Ncens,
    declare_Jcens,
    declare_U,
    code_template[8:14], # parameters data block
    # additions to parameters block:
    "  // censored value parameters:",
    declare_Ycens,
    code_template[15],
    # add a transformed parameters block:
    "transformed parameters {",
    "  // combine observed with estimated censored:",
    "  array[N] vector[D] yl = y;",
    declare_yl,
    "}",
    code_template[16:26], # model block
    "    yl[n] ~ normal(w * col(z, n) + mu, sigma);",
    code_template[28] # model block
  )
  if (length(index) == 0) code_template else code
}

build_standatalist <- function(data, components, censoring) {

  n <- nrow(data)

  data_list <- list(
    N = n,
    D = ncol(data),
    M = components,
    y = data
  )

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

  # remove empty list elements because no censoring for a particular element:
  U <- U[Ncens > 0]
  Jcens <- Jcens[Ncens > 0]
  Ncens <- Ncens[Ncens > 0]

  c(data_list, Ncens, Jcens, U)

}

# helper functions --------------------------------------------------------

# code snippets for censoring:

get_censored_column_numbers <- function(standatalist) {
  # get names of stan data list elements:
  names_standata<- names(standatalist)
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
  sapply(
    index,
    \(x) paste0("  int<lower=0> Ncens_y", x, "; // number of censored, y", x)
  )
}

build_Jcens <- function(index) {
  sapply(
    index,
    \(x) paste0("  array[Ncens_y", x, "] int<lower=1> Jcens_y", x, "; // positions of censored, y", x)
  )
}

build_U <- function(index) {
  sapply(
    index,
    \(x) paste0("  array[Ncens_y", x, "] real U_y", x, "; // left-censoring limits, y", x)
  )
}

build_Ycens <- function(index) {
  sapply(
    index,
    \(x) paste0("  array[Ncens_y", x, "] real<lower=0, upper=U_y", x, "> Ycens_y", x, "; // estimated censored, y", x)
  )
}

build_yl <- function(index) {
  sapply(
    index,
    \(x) paste0("  yl[Jcens_y", x, ", ", x, "] = Ycens_y", x, ";")
  )
}
