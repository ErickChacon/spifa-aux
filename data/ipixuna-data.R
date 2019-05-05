#############################################################################
## Title: SIMULATE IPIXUNA DATA
## Desc: Simulate data for the package based on Ipixuna
## Status: running
## Author: Erick A. Chacon-Montalvan
## Date: 02 May 2019
#############################################################################


# CLEAN WORKSPACE AND LOAD PACKAGES --------------------------------------------

rm(list = ls())
library(sf)
library(tidyverse)
devtools::load_all("../../datasim")
# library(datasim)

load("ipixuna_boundary.RData")

# SIMULATE LATENT FACTORS: THETA -----------------------------------------------

# parameters for multivariate random effect "v"
Corr <- matrix(c(1, -0.3, -0.3, 1), nrow = 2)
sigmas <- rep(0.4^0.5, 2)
Cov <- sigmas * t(sigmas * Corr)

# parameters for multivariate Gaussian process "w"
beta <- c(-0.5, 0)
A <- diag(c(0.6, 0.6))
cor.params <- list(list(phi = 300), list(phi = 150))

# formula for the latent factors "theta"
f_theta <- list(
  mean ~ mfe(x1, beta = get("beta")) +
    mre(factor(id), sigma = get("Cov")) +
    mgp(list(long, lat), A = get("A"), cor.model = "exp_cor",
        cor.params = get("cor.params"), geom = get("ipixuna_boundary")),
  sd ~ I(0)
  )

# simulate latent factors "theta"
n <- 200
data_geo <- sim_model(formula = f_theta, n = n, responses = 2, seed = 9)
data_geo_wide <- data_geo %>%
  dplyr::select(- (mre.factor.mean:sd)) %>%
  dplyr::mutate(response_label = paste0("theta", response_label)) %>%
  tidyr::spread(response_label, response)

# SIMULATE RESPONSE ITEMS ------------------------------------------------------

# parameters for item factor analysis
set.seed(1)
q <- 10
easiness <- matrix((1:q - 5)/10 * 2, nrow = 1)
discrimination1 <- seq(0.4, 1.5, length.out = q)
discrimination2 <- runif(q, 0, 2)
# restrictions
discrimination1[c(1, 3, 5, 8)] <- c(1, 0, 0, 0)
discrimination2[c(1:2, 4, 5, 10)] <- c(0, 1, 0, 0, 0)

# formula for item factor analysis
f <- list(
  prob ~ mi(beta = get("easiness")) +
    mfe(theta1, beta = get("discrimination1")) +
    mfe(theta2, beta = get("discrimination2")),
  size ~ I(1)
  )

# simulate items
data_spifa <- sim_model(formula = f,
                        link_inv = list(pnorm, identity),
                        generator = rbinom,
                        responses = paste("item", 1:10),
                        n = n,
                        init_data = data_geo_wide
                        )




# VISUALIZE SPIFA DATA ---------------------------------------------------------

(ggplot(data_geo, aes(long, lat)) +
  geom_point(aes(size = mgp.list.mean, col = mgp.list.mean)) +
  facet_grid(~ response_label) +
  scale_colour_distiller(palette = "RdBu")
  ) %>%
print()

(ggplot(data_spifa, aes(long, lat)) +
  geom_point(aes(col = factor(response))) +
  facet_wrap(~ response_label)
  ) %>%
print()

ggplot(data_geo, aes(x1, mean)) +
  geom_point(aes(col = factor(response_label))) +
  facet_wrap(~ response_label)





