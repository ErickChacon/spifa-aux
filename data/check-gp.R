
# CLEAN WORKSPACE AND LOAD PACKAGES --------------------------------------------

rm(list = ls())
library(sf)
library(ggplot2)
library(datasim)
load("ipixuna_boundary.RData")

# SIMULATE UNIVARIATE GAUSSIAN PROCESS ------------------------------------------

formula <- list(
  mean ~ gp(list(lon, lat), cor.params = list(phi = 200), geom = get("ipixuna_boundary")),
  sd ~ I(0)
  )

data_geo <- sim_model(formula, n = 500)

ggplot(data_geo) +
  geom_point(aes(lon, lat, col = response), size = 5) +
  scale_colour_distiller(palette = "RdBu") +
  coord_sf(crs = st_crs(ipixuna_boundary))

# SIMULATE MULTIVARIATE GAUSSIAN PROCESS ---------------------------------------

formula <- list(
  mean ~ mgp(list(lon, lat), A = diag(2, 2),
             cor.params = list(list(phi = 300), list(phi = 150)),
             geom = get("ipixuna_boundary")),
  sd ~ I(0)
  )

data_geo <- sim_model(formula = formula, n = 100, responses = 2, seed = 9)

ggplot(data_geo) +
  geom_point(aes(lon, lat, colour = response), size = 5) +
  scale_colour_distiller(palette = "RdBu") +
  facet_wrap(~ response_label) +
  coord_sf(crs = st_crs(ipixuna_boundary)) +
  theme(legend.position = "bottom")
