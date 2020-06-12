# Yoinked directly from stats4::mle help file
## More generalizable functions (require >= R 4.0.0)
LM_mll <- function(formula, data = environment(formula))
{
     y <- model.response(model.frame(formula, data))
     X <- model.matrix(formula, data)
     b0 <- numeric(NCOL(X))
     names(b0) <- colnames(X)
     function(b=b0, sigma=1)
         -sum(dnorm(y, X %*% b, sigma, log=TRUE))
}

# FOR REFERENCE: the simplified version, with irrelevant terms
## (Still written s.t. inside sum() is the contribution from a single obsv.)
LM_mll2 <- function(formula, data = environment(formula))
{
     y <- model.response(model.frame(formula, data))
     X <- model.matrix(formula, data)
     b0 <- numeric(NCOL(X))
     names(b0) <- colnames(X)
     function(b=b0, sigma=1)
         -sum( -log(sigma) - 0.5*log(2*pi) - (2*sigma^2)^-1 * (y - X%*%b)^2)
}

# FOR REFERENCE: the simplified version, without irrelevant terms
## (Still written s.t. inside sum() is the contribution from a single obsv.)
LM_mll3 <- function(formula, data = environment(formula))
{
     y <- model.response(model.frame(formula, data))
     X <- model.matrix(formula, data)
     b0 <- numeric(NCOL(X))
     names(b0) <- colnames(X)
     function(b=b0, sigma=1)
         -sum( -log(sigma) - (2*sigma^2)^-1 * (y - X%*%b)^2)
}

# ALSO FOR REFERENCE: the more svelte simplified version
LM_mll4 <- function(formula, data = environment(formula))
{
     y <- model.response(model.frame(formula, data))
     X <- model.matrix(formula, data)
     b0 <- numeric(NCOL(X))
     names(b0) <- colnames(X)
     function(b=b0, sigma=1)
        -(length(y) * -log(sigma) - (2*sigma^2)^-1 * sum((y - X%*%b)^2))
}