###############################################################################
# Base Definitions (as before)
###############################################################################
pqnumber <- function(sign, p, q, nums) {
  if (!(sign %in% c(1, -1))) stop("Sign must be either 1 or -1.")
  if (!is.numeric(p) || !is.numeric(q) || p < 0 || q < 0)
    stop("p and q must be non-negative integers.")
  if (length(nums) != (p + q + 1))
    stop("nums must have length p + q + 1.")
  if (any(nums < 0 | nums > 9))
    stop("nums must contain only digits between 0 and 9.")
  
  structure(list(sign = sign, p = p, q = q, nums = nums), class = "pqnumber")
}

is_pqnumber <- function(x) {
  inherits(x, "pqnumber")
}

as_numeric <- function(x) {
  if (!is_pqnumber(x)) stop("Not a pqnumber object.")
  exponents <- seq(-x$p, x$q)
  value <- sum(x$nums * 10^exponents) * x$sign
  # Force a string with exactly x$p decimals, then convert to numeric.
  as.numeric(sprintf(paste0("%.", x$p, "f"), value))
}

print.pqnumber <- function(x, DEC = FALSE) {
  if (!is_pqnumber(x)) stop("Not a pqnumber object.")
  
  p <- x$p
  q <- x$q
  nums <- x$nums
  sign <- x$sign
  
  if (DEC) {
    decimal_value <- as_numeric(x)
    formatted_decimal <- format(decimal_value, nsmall = x$p)
    
    powers <- seq(-p, q)
    term_strings <- paste(nums, "× 10^", powers, sep = "")
    formatted_terms <- paste(term_strings, collapse = " + ")
    
    cat("p =", p, "and q =", q, "with nums =", paste(nums, collapse = " "),
        "has the decimal value\n")
    cat(formatted_terms, "=", formatted_decimal, "\n")
  } else {
    cat("sign =", sign, "\n")
    cat("p =", p, "\n")
    cat("q =", q, "\n")
    cat("nums =", paste(nums, collapse = " "), "\n")
  }
}

###############################################################################
# as_pqnumber: Convert a numeric value to a pqnumber.
# The conversion scales the absolute value by 10^p so that the ones digit falls at position p+1.
###############################################################################
as_pqnumber <- function(x, p, q) {
  if (!is.numeric(x)) stop("x must be numeric.")
  
  sign_val <- if (x < 0) -1 else 1
  x_abs <- abs(x)
  
  total_len <- p + q + 1
  # Scale by 10^p and round.
  y_val <- round(x_abs * 10^p)
  
  # Use sprintf with "%.0f" to obtain the full integer value as a string.
  y_str <- sprintf("%.0f", y_val)
  
  # Pad with leading zeros if necessary.
  if(nchar(y_str) < total_len){
    y_str <- paste0(strrep("0", total_len - nchar(y_str)), y_str)
  }
  if(nchar(y_str) > total_len){
    stop("Number has more digits than allowed by p and q.")
  }
  
  digits <- as.integer(strsplit(y_str, "")[[1]])
  if(any(is.na(digits))) stop("Conversion error: NA values detected in nums.")
  
  # Reverse the digits so that index 1 corresponds to 10^(-p)
  nums <- rev(digits)
  
  pqnumber(sign = sign_val, p = p, q = q, nums = nums)
}

###############################################################################
# expand_pq: Expand a pqnumber to new p and new q.
###############################################################################
expand_pq <- function(x, new_p, new_q) {
  if(new_p < x$p || new_q < x$q)
    stop("expand_pq: new p and q must be >= current p and q.")
  
  pad_left  <- new_p - x$p    # extra digits for lower exponents
  pad_right <- new_q - x$q    # extra digits for higher exponents
  
  new_nums <- c(rep(0, pad_left), x$nums, rep(0, pad_right))
  pqnumber(x$sign, new_p, new_q, new_nums)
}

###############################################################################
# carry_over: Propagate carries in a digit vector (little-endian)
###############################################################################
carry_over <- function(digits) {
  carry <- 0
  n <- length(digits)
  for(i in 1:n) {
    total <- digits[i] + carry
    digits[i] <- total %% 10
    carry <- total %/% 10
  }
  while(carry > 0) {
    digits <- c(digits, carry %% 10)
    carry <- carry %/% 10
  }
  digits
}

###############################################################################
# borrow_over: Subtract subtrahend from minuend (digit vectors in little-endian)
###############################################################################
borrow_over <- function(minuend, subtrahend) {
  n <- length(minuend)
  result <- integer(n)
  borrow <- 0
  for(i in 1:n) {
    diff <- minuend[i] - subtrahend[i] - borrow
    if(diff < 0) {
      diff <- diff + 10
      borrow <- 1
    } else {
      borrow <- 0
    }
    result[i] <- diff
  }
  result
}

###############################################################################
# compare_abs: Compare absolute values of two pqnumber objects (assumed same representation)
# Returns 1 if |x| > |y|, 0 if equal, -1 if |x| < |y|
###############################################################################
compare_abs <- function(x, y) {
  n <- length(x$nums)
  for(i in n:1) {
    if(x$nums[i] > y$nums[i]) return(1)
    if(x$nums[i] < y$nums[i]) return(-1)
  }
  0
}

###############################################################################
# add: Add two pqnumber objects using carry_over()
###############################################################################
add <- function(x, y) {
  if (!is_pqnumber(x) || !is_pqnumber(y))
    stop("add: x and y must be pqnumber objects.")
  
  # Unify representations: use maximum p and q.
  new_p <- max(x$p, y$p)
  new_q <- max(x$q, y$q)
  if(x$p != new_p || x$q != new_q) x <- expand_pq(x, new_p, new_q)
  if(y$p != new_p || y$q != new_q) y <- expand_pq(y, new_p, new_q)
  
  n <- new_p + new_q + 1
  
  # If signs differ, delegate to subtraction.
  if(x$sign != y$sign) {
    neg_y <- pqnumber(sign = -y$sign, p = y$p, q = y$q, nums = y$nums)
    return(subtract(x, neg_y))
  }
  
  raw_sum <- x$nums + y$nums
  result_digits <- carry_over(raw_sum)
  
  result_sign <- x$sign
  desired_len <- new_p + new_q + 1
  L_result <- length(result_digits)
  if(L_result > desired_len) {
    new_q <- L_result - new_p - 1
  } else if(L_result < desired_len) {
    result_digits <- c(result_digits, rep(0, desired_len - L_result))
  }
  pqnumber(result_sign, new_p, new_q, result_digits)
}

###############################################################################
# subtract: Subtract two pqnumber objects using borrow_over()
###############################################################################
subtract <- function(x, y) {
  if (!is_pqnumber(x) || !is_pqnumber(y))
    stop("subtract: x and y must be pqnumber objects.")
  
  new_p <- max(x$p, y$p)
  new_q <- max(x$q, y$q)
  if(x$p != new_p || x$q != new_q) x <- expand_pq(x, new_p, new_q)
  if(y$p != new_p || y$q != new_q) y <- expand_pq(y, new_p, new_q)
  
  n <- new_p + new_q + 1
  
  if(x$sign != y$sign) {
    neg_y <- pqnumber(sign = -y$sign, p = y$p, q = y$q, nums = y$nums)
    return(add(x, neg_y))
  }
  
  cmp <- compare_abs(x, y)
  if(cmp == 0) {
    return(pqnumber(1, new_p, new_q, rep(0, n)))
  }
  
  if(cmp > 0) {
    result_digits <- borrow_over(x$nums, y$nums)
    result_sign <- x$sign
  } else {
    result_digits <- borrow_over(y$nums, x$nums)
    result_sign <- if(x$sign == 1) -1 else 1
  }
  pqnumber(result_sign, new_p, new_q, result_digits)
}

###############################################################################
# multiply_by_digit: Multiply a pqnumber by a single digit (0-9) via digit multiplication.
###############################################################################
multiply_by_digit <- function(x, d) {
  if (d == 0) {
    total_len <- x$p + x$q + 1
    return(pqnumber(1, x$p, x$q, rep(0, total_len)))
  }
  new_digits <- carry_over(x$nums * d)
  L <- length(new_digits)
  expected <- x$p + x$q + 1
  new_q <- x$q
  if(L > expected) {
    new_q <- L - x$p - 1
  } else if(L < expected) {
    new_digits <- c(new_digits, rep(0, expected - L))
  }
  pqnumber(x$sign, x$p, new_q, new_digits)
}

###############################################################################
# shift_pq: Multiply a pqnumber by 10^k and convert to target representation.
# If the integer part grows, adjust target_q accordingly.
###############################################################################
shift_pq <- function(x, k, target_p, target_q) {
  new_value <- as_numeric(x) * (10^k)
  res <- tryCatch({
    as_pqnumber(new_value, target_p, target_q)
  }, error = function(e) {
    # Adjust target_q if necessary
    y_val <- round(abs(new_value) * 10^target_p)
    n_digits <- nchar(sprintf("%.0f", y_val))
    desired_len <- target_p + target_q + 1
    if(n_digits > desired_len) {
      extra <- n_digits - desired_len
      as_pqnumber(new_value, target_p, target_q + extra)
    } else {
      stop(e)
    }
  })
  res
}

###############################################################################
# multiply: Multiply two pqnumber objects using add() to sum partial products.
# Final product: p_new = x$p + y$p,  q_new = x$q + y$q + 1.
###############################################################################
multiply <- function(x, y) {
  if (!is_pqnumber(x) || !is_pqnumber(y))
    stop("multiply: x and y must be pqnumber objects.")
  
  target_p <- x$p + y$p
  target_q <- x$q + y$q + 1  # extra integer digit for overflow
  
  total_len <- target_p + target_q + 1
  result <- pqnumber(1, target_p, target_q, rep(0, total_len))
  
  for(j in seq_along(y$nums)) {
    d <- y$nums[j]
    if(d == 0) next
    partial <- multiply_by_digit(x, d)
    shift_amount <- -y$p + (j - 1)
    partial_shifted <- shift_pq(partial, shift_amount, target_p, target_q)
    result <- add(result, partial_shifted)
  }
  
  result$sign <- x$sign * y$sign
  result
}

###############################################################################
# Example Usage
###############################################################################
a <- pqnumber(1, 3, 4, c(1, 2, 3, 4, 5, 6, 7, 8))
b <- pqnumber(1, 6, 0, c(3, 9, 5, 1, 4, 1, 3))
c <- pqnumber(-1, 5, 1, c(2, 8, 2, 8, 1, 7, 2))
is_pqnumber(a)
is_pqnumber(b)
is_pqnumber(c)
is_pqnumber(3.14)
print(a)
print(b)
print(c)
print(a, DEC = TRUE)
print(b, DEC = TRUE)
print(c, DEC = TRUE)
as_pqnumber(3.14, 3, 4)
as_pqnumber(34.123, 3, 4)
as_numeric(pq_b) ###
as_numeric(b)
as_numeric(c)
pq_a <- as_pqnumber(3.14, 3, 4)
pq_b <- as_pqnumber(2.778, 3, 4)
add(pq_a, pq_b)
add(a, b)
subtract(a, b)
multiply(a, b)
multiply(pq_a, pq_b)
subtract(pq_b, pq_a)
a <-

.Machine$integer.max
