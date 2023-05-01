# Header -----------------------------------------------------------------------
# Proj: Experimental Design Final Project
# Author: Tyler Mansfield (tyler.mansfield96@gmail.com)
# Desc: Implements the classic test statistics for matched pair experiments


# Various test-statistics --------------------------------------------------

simple_mean <- function(tau_i)
{
  mean(tau_i)
}

studentized_simple_mean <- function(tau_i)
{
  tau_bar <- mean(tau_i)
  n <- length(tau_i)
  
  tau_bar / sqrt(1/(n*(n-1)) * sum((tau_i - tau_bar)^2))
}

wilcoxon <- function(tau_i)
{
  ranks <- rank(tau_i)
  sum(as.numeric(tau_i > 0) * ranks)
}

ks_like <- function(tau_i)
{
  n <- length(tau_i)
  counts <- table(tau_i) # Identify any repeated values
  unique_values <- as.numeric(names(counts))
  ecdf_vals <- ecdf(tau_i)(unique_values) # hat{F}(x)
  ecdf_neg_vals <- ecdf(tau_i)(-1 * unique_values) # hat{F}(-x)
  
  # Now calculate K(x) = hat{F}(x) + hat{F}(-x) - P(X = x) - 1
  ks_fxn <- ecdf_vals + ecdf_neg_vals - counts / n - 1
  
  #plot(unique_values, abs(ks_fxn))
  
  # Test statistic is maximum value of |K(x)|
  max(abs(ks_fxn))
}
