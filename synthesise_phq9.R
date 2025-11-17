# ============================================================
# SYNTHETIC PHQ-9 GENERATOR
# Inputs:
#   phq_props : list of length 9, each vector length 4 (proportions)
#   Sigma_hat : 9x9 latent (polychoric) correlation matrix
#   n         : number of synthetic rows to generate
# Output:
#   n x 9 matrix of ordinal values {0,1,2,3}
# ============================================================

library(MASS)

# ============================================================
# Normalize proportions input to a list of 9 numeric vectors
# Accepts:
#   - list of length 9 (each length 4)
#   - data frame or matrix 9×4
#   - data frame or matrix 4×9  <-- your case
# ============================================================
normalize_phq_props <- function(phq_props) {
  
  # Case 1: already a list
  if (is.list(phq_props) && !is.data.frame(phq_props)) {
    if (length(phq_props) != 9)
      stop("List form phq_props must have length 9.")
    return(phq_props)
  }
  
  # Case 2: matrix/data frame
  if (is.data.frame(phq_props) || is.matrix(phq_props)) {
    
    dims <- dim(phq_props)
    
    # Case 2a: expected orientation 9×4
    if (all(dims == c(9, 4))) {
      out <- lapply(1:9, function(i) as.numeric(phq_props[i, ]))
    }
    
    # Case 2b: your orientation 4×9
    else if (all(dims == c(4, 9))) {
      # columns are items → transpose to 9×4
      tmp <- t(phq_props)                 # now 9×4
      out <- lapply(1:9, function(i) as.numeric(tmp[i, ]))
    }
    
    else {
      stop("Proportions must be 9×4 or 4×9 for PHQ-9.")
    }
    
    # check sums
    for (j in 1:9) {
      if (abs(sum(out[[j]]) - 1) > 1e-6)
        stop(paste("Item", j, "proportions do not sum to 1."))
    }
    
    return(out)
  }
  
  stop("phq_props must be list, 9×4 df/matrix, or 4×9 df/matrix.")
}

# ------------------------------------------------------------
compute_thresholds <- function(phq_list) {
  lapply(phq_list, function(p) {
    cum_p <- cumsum(p)
    c(-Inf, qnorm(cum_p[-length(cum_p)]), Inf)
  })
}

latent_to_ordinal <- function(z, thresholds) {
  out <- numeric(length(z))
  for (j in seq_along(z)) {
    out[j] <- sum(z[j] > thresholds[[j]]) - 1
  }
  out
}

simulate_phq9 <- function(n, phq_props, Sigma_hat) {
  
  if (!all(dim(Sigma_hat) == c(9, 9)))
    stop("Sigma_hat must be a 9×9 matrix.")
  
  phq_list <- normalize_phq_props(phq_props)
  thresholds <- compute_thresholds(phq_list)
  
  Z <- MASS::mvrnorm(n, mu = rep(0, 9), Sigma = Sigma_hat)
  X <- t(apply(Z, 1, latent_to_ordinal, thresholds = thresholds))
  
  colnames(X) <- c(
    "f.20514.0.0","f.20510.0.0","f.20517.0.0",
    "f.20519.0.0","f.20511.0.0","f.20507.0.0",
    "f.20508.0.0","f.20518.0.0","f.20513.0.0"
  )
  
  X
}

phq9_counts = readRDS("./phq9.counts.rds")
Sigma_hat = readRDS("./mixed.phq9.cor.all.rds")

N = sum(phq9_counts[,1])

phq9_props = phq9_counts/N

set.seed(1408)

 synthetic_phq <- simulate_phq9(
   n = N,
   phq_props = phq9_props,
   Sigma_hat = Sigma_hat
 )
 
saveRDS(synthetic_phq, "synthetic_phq9.rds")
#
# head(synthetic_phq)
