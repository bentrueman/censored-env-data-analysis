
# functions ---------------------------------------------------------------

modify_stan_template <- function(dimension) {
  # starter code:
  code_template <- readLines(here("R/stan/ppca.stan"))
  # build additional code:
  declare_Ncens <- build_Ncens(dimension)
  declare_Jcens <- build_Jcens(dimension)
  declare_U <- build_U(dimension)
  declare_Ycens <- build_Ycens(dimension)
  declare_yl <- build_yl(dimension)
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
  if (dimension == 0) code_template else code
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
  Ncens <- Ncens[Ncens > 0]

  # positions of censored values:
  index <- seq(n)
  Jcens <- lapply(censoring, \(x) index[x])
  names(Jcens) <- paste0("Jcens_y", seq(length(Jcens)))
  Jcens <- Jcens[Ncens > 0]

  # censoring limits:
  U <- mapply(\(x, y) x[y], data, censoring, SIMPLIFY = FALSE)
  names(U) <- paste0("U_y", seq(length(U)))
  U <- U[Ncens > 0]

  c(data_list, Ncens, Jcens, U)

}

# helper functions --------------------------------------------------------

# code snippets for censoring:

build_Ncens <- function(dimension) {
  sapply(
    seq(dimension),
    \(x) paste0("  int<lower=0> Ncens_y", x, "; // number of censored, y", x)
  )
}

build_Jcens <- function(dimension) {
  sapply(
    seq(dimension),
    \(x) paste0("  array[Ncens_y", x, "] int<lower=1> Jcens_y", x, "; // positions of censored, y", x)
  )
}

build_U <- function(dimension) {
  sapply(
    seq(dimension),
    \(x) paste0("  array[Ncens_y", x, "] real U_y", x, "; // left-censoring limits, y", x)
  )
}

build_Ycens <- function(dimension) {
  sapply(
    seq(dimension),
    \(x) paste0("  array[Ncens_y", x, "] real<lower=0, upper=U_y", x, "> Ycens_y", x, "; // estimated censored, y", x)
  )
}

build_yl <- function(dimension) {
  sapply(
    seq(dimension),
    \(x) paste0("  yl[Jcens_y", x, ", ", x, "] = Ycens_y", x, ";")
  )
}
