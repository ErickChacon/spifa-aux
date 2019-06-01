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
library(datasim)

load("ipixuna_boundary.RData")

# SIMULATE LATENT FACTORS: THETA -----------------------------------------------

# parameters for multivariate random effect "v"
Corr <- matrix(c(1, -0.3, -0.3, 1), nrow = 2)
sigmas <- rep(0.4 ^ 0.5, 2)
Cov <- sigmas * t(sigmas * Corr)

# parameters for multivariate Gaussian process "w"
beta <- c(-0.5, 0)
A <- diag(c(0.6, 0.6))
cor.params <- list(list(phi = 300), list(phi = 150))

# formula for the latent factors "theta"
f_theta <- list(
  mean ~ mfe(x1, beta = get("beta")) +
    mre(factor(id), sigma = get("Cov")) +
    mgp(list(lon, lat), A = get("A"), cor.model = "exp_cor",
        cor.params = get("cor.params"), geom = get("ipixuna_boundary")),
  sd ~ I(0)
  )

# simulate latent factors "theta"
n <- 200
data_geo <- sim_model(formula = f_theta, n = n, responses = 2, seed = 123)
data_geo_wide <- data_geo %>%
  dplyr::select(- (mre.factor.mean:sd)) %>%
  dplyr::mutate(response_label = paste0("theta", response_label)) %>%
  tidyr::spread(response_label, response)

# SIMULATE RESPONSE ITEMS ------------------------------------------------------

# parameters for item factor analysis
q <- 10
easiness <- matrix((1:q - 5) / 10 * 1.7, nrow = 1)
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
ipixuna <- sim_model(formula = f, link_inv = list(pnorm, identity), generator = rbinom,
                     responses = paste("item", 1:10), n = n, init_data = data_geo_wide)

# SAVE SIMULATED IPIXUNA DATA --------------------------------------------------

# add parameters to the data
attr(ipixuna, "parameters") <-
  list(Cov = Cov, beta = beta, A = A, cor.params = cor.params, n = n,
       q = q, easiness = easiness,
       discrimination = unname(cbind(discrimination1, discrimination2)))


save(ipixuna, file = "ipixuna.RData")

# VISUALIZE SPIFA DATA ---------------------------------------------------------

theme_options <- theme(legend.position = "bottom")

ggplot(data_geo, aes(lon, lat)) +
  geom_point(aes(size = mgp.list.mean, col = mgp.list.mean), size = 3) +
  facet_grid(~ response_label) +
  scale_colour_distiller(palette = "RdBu") +
  coord_sf(crs = st_crs(ipixuna_boundary)) +
  labs(x = NULL, y = NULL) +
  theme_options

ggplot(data_geo, aes(x1, mean)) +
  geom_point(aes(col = factor(response_label)), size = 3) +
  geom_smooth(method = lm, se = FALSE) +
  facet_wrap(~ response_label) +
  theme_options

ggplot(ipixuna, aes(lon, lat)) +
  geom_point(aes(col = factor(response))) +
  facet_wrap(~ response_label, ncol = 5) +
  coord_sf(crs = st_crs(ipixuna_boundary)) +
  labs(x = NULL, y = NULL) +
  theme_options

