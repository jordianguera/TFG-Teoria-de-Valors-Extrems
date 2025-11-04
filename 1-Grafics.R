library(ggplot2)
library(ggfortify)
library(VGAM)

set.seed(1714)

ParametresGumbel <- list(
  list(mu = 0, beta = 1),
  list(mu = 0, beta = 2),
  list(mu = 1, beta = 1))

ParametresFrechet <- list(
  list(alpha = 0.5, beta = 1),
  list(alpha = 1, beta = 1),
  list(alpha = 1, beta = 2))


ParametresWeibull <- list(
  list(shape = 1.5, scale = 1),
  list(shape = 2, scale = 1),
  list(shape = 1.5, scale = 2))

n <- 50000

SimulaGumbel <- function(mu, beta, n) {
  rgumbel(n, location = mu, scale = beta)
}

SimulaFrechet <- function(alpha, beta, n) {
  rfrechet(n, shape = alpha, scale = beta)
}

SimulaWeibull <- function(shape, scale, n) {
  rweibull(n, shape = shape, scale = scale)
}

LlistaGumbel <- lapply(ParametresGumbel, function(p) {
  data.frame(Valor = SimulaGumbel(p$mu, p$beta, n), 
             Distribucio = paste("Gumbel", "mu =", p$mu, ", beta =", p$beta))
})
DfGumbel <- do.call(rbind, LlistaGumbel)

LlistaFrechet <- lapply(ParametresFrechet, function(p) {
  data.frame(Valor = SimulaFrechet(p$alpha, p$beta, n),
             Distribucio = paste("Frechet", "alpha =", p$alpha, ", beta =", p$beta))
})
DfFrechet <- do.call(rbind, LlistaFrechet)

LlistaWeibull <- lapply(ParametresWeibull, function(p) {
  data.frame(Valor = SimulaWeibull(p$shape, p$scale, n),
             Distribucio = paste("Weibull", "shape =", p$shape, ", scale =", p$scale))
})
DfWeibull <- do.call(rbind, LlistaWeibull)

PlotGumbel <- ggplot(DfGumbel, aes(x = Valor, color = Distribucio, fill = Distribucio)) +
  geom_density(alpha = 0.3) +
  labs(title = "Distribució Gumbel segons paràmetre",
       subtitle = "(només per a fins il·lustratius)",
       x = "Valor de la variable aleatòria", y = "Funció de densitat") +
  theme_minimal() +
  scale_fill_manual(values = c("skyblue", "lightblue", "blue")) +
  scale_color_manual(values = c("darkblue", "darkcyan", "darkslateblue"))

PlotFrechet <- ggplot(DfFrechet, aes(x = Valor, color = Distribucio, fill = Distribucio)) +
  geom_density(alpha = 0.3) +
  labs(title = "Distribució Frechet segons paràmetre",
       subtitle = "(només per a fins il·lustratius)",
       x = "Valor de la variable aleatòria", y = "Funció de densitat") +
  theme_minimal() +
  scale_fill_manual(values = c("orange", "darkorange", "orangered")) +
  scale_color_manual(values = c("darkorange", "orange", "orangered"))+
  xlim(0, 20)

PlotWeibull <- ggplot(DfWeibull, aes(x = Valor, color = Distribucio, fill = Distribucio)) +
  geom_density(alpha = 0.3) +
  labs(title = "Distribució Weibull segons paràmetre",
       subtitle = "(només per a fins il·lustratius)",
       x = "Valor de la variable aleatòria", y = "Funció de densitat") +
  theme_minimal() +
  scale_fill_manual(values = c("green", "lightgreen", "darkgreen")) +
  scale_color_manual(values = c("darkgreen", "forestgreen", "darkolivegreen"))



PlotGumbel
PlotFrechet
PlotWeibull