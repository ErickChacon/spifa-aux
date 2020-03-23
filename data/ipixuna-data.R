#############################################################################
## Title: SIMULATE IPIXUNA DATA
## Desc: Simulate data for the package based on Ipixuna city
## Status: running
## Author: Erick A. Chacon-Montalvan
## Date: 01 Jun 2019
#############################################################################

# CLEAN WORKSPACE AND LOAD PACKAGES --------------------------------------------

rm(list = ls())
library(dplyr)
library(ggplot2)
library(sf)
library(datasim)

load("ipixuna_boundary.RData")

# SIMULATE LATENT FACTORS: THETA -----------------------------------------------

# parameters for multivariate random effect "v"
Corr <- matrix(c(1, -0.3, -0.3, 1), nrow = 2)
sigmas <- rep(0.3 ^ 0.5, 2)
Cov <- sigmas * t(sigmas * Corr)

# parameters for multivariate Gaussian process "w"
beta <- c(-0.5, 0.5)
A <- diag(c(0.45 ^ 0.5, 0.45 ^ 0.5))
cor.params <- list(list(phi = 300), list(phi = 150))

# formula for the latent factors "theta"
f_theta <- list(
  mean ~ mfe(x1, beta = get("beta")) +
    mre(factor(id), sigma = get("Cov")) +
    mgp(coords, A = get("A"), cor.model = "exp_cor",
        cor.params = get("cor.params"), geom = get("ipixuna_boundary")),
  sd ~ I(0)
  )

# simulate latent factors "theta" in long format
n <- 200
data_theta <- sim_model(formula = f_theta, n = n, responses = paste0("Theta", 1:2),
                      seed = 123) %>%
  dplyr::select(-sd)

# simulated latent factors "theta" in wide format
data_theta_wide <- data_theta %>%
  dplyr::select(-(mre.factor.mean:mean)) %>%
  tidyr::spread(response_label, response)

# # check variances
# data_theta %>%
#   mutate(beta = rep(beta, each = n)) %>%
#   group_by(response_label) %>%
#   summarize(var(x1 * beta), var(mgp.coords.mean), var(mre.factor.mean), var(mean))

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
f_items <- list(
  prob ~ mi(beta = get("easiness")) +
    mfe(Theta1, beta = get("discrimination1")) +
    mfe(Theta2, beta = get("discrimination2")),
  size ~ I(1)
  )

# simulate items in long format
ipixuna <- sim_model(f_items, link_inv = list(pnorm, identity), generator = rbinom, n = n,
                     responses = paste("Item", 1:10), init_data = data_theta_wide) %>%
  dplyr::select(-size)

# simulated items in wide format
ipixuna_wide <- ipixuna %>%
  dplyr::select(-prob) %>%
  tidyr::spread(response_label, response)

# SAVE SIMULATED IPIXUNA DATA --------------------------------------------------

# parameters for simulation
parameters <- list(n = n, q = q, m = nrow(A), g = ncol(A), easiness = easiness,
                   discrimination = unname(cbind(discrimination1, discrimination2)),
                   beta = beta, Cov = Cov, A = A, cor.params = cor.params)

# add parameters to the data
attr(ipixuna, "parameters") <- parameters
attr(ipixuna_wide, "parameters") <- parameters

# save
save(ipixuna, ipixuna_wide, ipixuna_boundary, file = "ipixuna.RData")

# VISUALIZE IPIXUNA DATA -------------------------------------------------------

theme_options <- theme(legend.position = "bottom")

# mgp
ggplot(data_theta) +
  geom_sf(aes(geometry = coords, col = mgp.coords.mean), size = 3) +
  facet_grid(~ response_label) +
  scale_colour_distiller(palette = "RdBu") +
  coord_sf(crs = st_crs(ipixuna_boundary)) +
  labs(col = "mgp") +
  theme_options

# fixed effects
ggplot(data_theta, aes(x1, mean)) +
  geom_point(aes(col = factor(response_label)), size = 3) +
  geom_smooth(method = lm, se = FALSE) +
  facet_wrap(~ response_label) +
  theme(legend.position = "none")

# items
ipixuna %>%
  bind_cols(., setNames(as.data.frame(sf::st_coordinates(.$coords)), c("lon", "lat"))) %>%
  ggplot(aes(lon, lat)) +
  geom_point(aes(col = factor(response))) +
  facet_wrap(~ response_label, ncol = 5) +
  coord_sf(crs = st_crs(ipixuna_boundary)) +
  theme_options

