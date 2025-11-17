# Mixed-type copula simulator for 2 continuous (std) + 24 ordinal/binary
# Author: (assistant) — use freely
#
# Inputs you must provide:
#   Sigma_hat   : 26 x 26 latent correlation matrix (numeric, PSD)
#   prop_list   : named list of length 24 OR matrix/data.frame with
#                 categories in rows and variables in columns (24 columns)
#                 Each element (or each column) must sum to 1 (within tol).
#   n           : number of synthetic rows to generate
# Optional:
#   cont_names  : character vector length 2 (default: c("age","x2"))
#   cat_names   : character vector length 24 (default: "v3".."v26")
#
# Output: data.frame with 26 columns (two continuous, 24 ordinal/binary {0,1,2,...})

library(MASS)

# -------------------------
# Helper: normalize prop_list
# -------------------------
normalize_prop_list <- function(prop_list) {
  # If it's a list already, check length 24
  if (is.list(prop_list) && !is.data.frame(prop_list)) {
    if (length(prop_list) != 24)
      stop("If prop_list is a list, it must have length 24 (one element per ordinal/binary variable).")
    # ensure numeric vectors and sum-to-one
    prop_list <- lapply(prop_list, function(v) {
      vnum <- as.numeric(v)
      if (any(vnum < 0)) stop("Negative probabilities found in prop_list.")
      if (abs(sum(vnum) - 1) > 1e-6) stop("One of the vectors in prop_list does not sum to 1.")
      vnum
    })
    return(prop_list)
  }
  
  # If it's a matrix/data.frame: categories x variables (rows = categories, cols = vars)
  if (is.matrix(prop_list) || is.data.frame(prop_list)) {
    mat <- as.matrix(prop_list)
    dims <- dim(mat)
    # Accept either categories x 24 (e.g. K x 24) OR 24 x K (transposed)
    if (dims[2] == 24) {
      # rows = categories, cols = variables (standard)
      out <- lapply(1:24, function(j) {
        v <- as.numeric(mat[, j])
        if (any(v < 0)) stop("Negative value in proportions matrix.")
        if (abs(sum(v) - 1) > 1e-6) stop(paste0("Column ", j, " of prop matrix does not sum to 1."))
        v
      })
      names(out) <- colnames(mat)
      return(out)
    } else if (dims[1] == 24) {
      # transposed: 24 rows (vars) x K columns -> convert each row
      out <- lapply(1:24, function(j) {
        v <- as.numeric(mat[j, ])
        if (any(v < 0)) stop("Negative value in proportions matrix.")
        if (abs(sum(v) - 1) > 1e-6) stop(paste0("Row ", j, " of prop matrix does not sum to 1."))
        v
      })
      names(out) <- rownames(mat)
      return(out)
    } else {
      stop("Proportions matrix must have 24 columns (categories x 24 variables) or 24 rows (24 variables x categories).")
    }
  }
  
  stop("prop_list must be either a named list length 24 or a matrix/data.frame with 24 columns or 24 rows.")
}

# -------------------------
# Helper: compute thresholds from category probabilities
# -------------------------
compute_thresholds_for_list <- function(prop_list) {
  # returns named list of threshold vectors (length = K+1 with -Inf .. Inf)
  thr_list <- lapply(prop_list, function(p) {
    # p is vector of category probs (categories in order 0,1,2,...)
    cum_p <- cumsum(as.numeric(p))
    # thresholds: -Inf, qnorm(cum_p[-K]), Inf
    c(-Inf, qnorm(cum_p[-length(cum_p)]), Inf)
  })
  thr_list
}

# -------------------------
# Map one latent vector to observed mixed variables
# -------------------------
latent_row_to_observed <- function(z_row, cont_count, thr_list, cont_names, cat_names) {
  p <- length(z_row)
  out <- numeric(p)
  
  # continuous: first cont_count columns -> identity (already std)
  if (cont_count >= 1) {
    out[1:cont_count] <- z_row[1:cont_count]
  }
  
  # ordinal/binary: remaining columns
  for (j in seq_len(p - cont_count)) {
    col_idx <- cont_count + j
    thr <- thr_list[[j]]  # thr_list should be in order of cat_names (j=1 -> variable 3)
    # number of categories = length(thr) - 1
    # Count how many thresholds are strictly less than z value:
    observed_cat <- sum(z_row[col_idx] > thr) - 1
    out[col_idx] <- observed_cat
  }
  
  # assign names
  names(out) <- c(cont_names, cat_names)
  out
}

# -------------------------
# Main generator
# -------------------------
simulate_population_2cont_24cat <- function(n, Sigma_hat, prop_list,
                                            cont_names = c("cont1","cont2"),
                                            cat_names = paste0("v", 3:26)) {
  
  # Basic checks
  if (!is.matrix(Sigma_hat) || any(dim(Sigma_hat) != c(26,26))) {
    stop("Sigma_hat must be a 26 x 26 numeric matrix.")
  }
  if (!is.numeric(n) || length(n) != 1 || n <= 0) stop("n must be a positive integer.")
  n <- as.integer(n)
  
  # normalize prop list (returns list length 24)
  prop_list2 <- normalize_prop_list(prop_list)
  # enforce ordering: if prop_list2 has names and they match cat_names, reorder
  if (!is.null(names(prop_list2)) && all(names(prop_list2) %in% cat_names)) {
    prop_list2 <- prop_list2[cat_names]  # place in requested order (may produce NA if mismatch)
    if (any(sapply(prop_list2, is.null))) stop("prop_list names do not fully match provided cat_names.")
  }
  
  if (length(prop_list2) != 24) stop("After normalization, prop_list must contain 24 elements.")
  
  # compute thresholds (list length 24) in same order
  thr_list <- compute_thresholds_for_list(prop_list2)
  
  # Draw latent Gaussians
  Z <- MASS::mvrnorm(n = n, mu = rep(0, 26), Sigma = Sigma_hat)
  
  # Map rows
  cont_count <- 2
  out_mat <- t(apply(Z, 1, latent_row_to_observed,
                     cont_count = cont_count,
                     thr_list = thr_list,
                     cont_names = cont_names,
                     cat_names = cat_names))
  
  # Convert types: continuous numeric, ordinal integer
  out_df <- as.data.frame(out_mat, stringsAsFactors = FALSE)
  # Ensure continuous columns numeric
  out_df[ , 1:cont_count] <- lapply(out_df[ , 1:cont_count, drop = FALSE], as.numeric)
  # Ensure categorical columns integer
  out_df[ , (cont_count+1):ncol(out_df)] <- lapply(out_df[ , (cont_count+1):ncol(out_df), drop = FALSE], function(x) as.integer(as.numeric(x)))
  
  # Return
  rownames(out_df) <- NULL
  out_df
}

regularise_correlation <- function(Sigma, eps = 1e-6) {
  if (!is.matrix(Sigma)) stop("Sigma must be a matrix.")
  if (nrow(Sigma) != ncol(Sigma)) stop("Sigma must be square.")
  
  # force symmetry
  A <- 0.5 * (Sigma + t(Sigma))
  
  # eigen-decomposition
  eig <- eigen(A, symmetric = TRUE)
  vals <- eig$values
  vecs <- eig$vectors
  
  # clip eigenvalues
  vals_clipped <- pmax(vals, eps)
  
  # reconstruct
  A_pd <- vecs %*% diag(vals_clipped) %*% t(vecs)
  
  # renormalise to correlation matrix (unit diagonal)
  d <- sqrt(diag(A_pd))
  A_cor <- A_pd / (d %o% d)
  
  A_cor
}


cidi_counts = readRDS("./cidi.counts.rds")
Sigma_hat = readRDS("./mixed.cidi.cor.all.rds")

set.seed(1443)

N = sum(cidi_counts[[1]])

prop_list = list()

for (i in seq(length(cidi_counts))){
  prop_list[[i]] = cidi_counts[[i]]/N
}

Sigma_hat_reg <- regularise_correlation(Sigma_hat)

synthetic_cidi <- simulate_population_2cont_24cat(
  n = N,
  Sigma_hat = Sigma_hat_reg,
  prop_list = prop_list,
  cont_names = c("age_std", "score_std"),   # optional
  cat_names = paste0("CIDI_", 1:24)         # optional names for the 24 items
)

saveRDS(synthetic_cidi, "synthetic_cidi.rds")

