library(ggplot2)
# observacion_a <- c(4, 4.5, 4, 7.5, 7, 6, 5, 5.5, 5, 6)
# observacion_b <- c(4, 4.5, 4, 7.5, 7, 6, 5, 5.5, 5, 6) + 4
# datos <- data.frame(observacion = rep(c("a", "b"), each = 10),
#                     valor = c(observacion_a, observacion_b),
#                     predictor = 1:10)
# ggplot(data = datos, aes(x = as.factor(predictor), y = valor,
#                          colour = observacion)) +
#   geom_path(aes(group = observacion)) +
#   geom_point() +
#   geom_line(aes(group = predictor), colour = "firebrick", linetype = "dashed") +
#   labs(x = "predictor", caption="La distancia euclídea entre las dos observaciones equivale a la raíz cuadrada de la suma de las longitudes de los segmentos rojos que unen cada par de puntos") +
#   theme_bw() +
#   theme(legend.position = "none")


datos <- data.frame(observacion = c("a", "b"), x = c(2,7), y = c(2,7))
manhattan <- data.frame(
  x = rep(2:6, each = 2),
  y = rep(2:6, each = 2) + rep(c(0,1), 5),
  xend = rep(2:6, each = 2) + rep(c(0,1), 5),
  yend = rep(3:7, each = 2))

manhattan_2 <- data.frame(
  x = c(2, 5, 5, 7),
  y = c(2, 2, 4, 4),
  xend = c(5, 5, 7, 7),
  yend = c(2, 4, 4, 7))

distancias <- ggplot(data = datos, aes(x = x, y = y)) +
  geom_segment(aes(x = 2, y = 2, xend = 7, yend = 7), color = "blue", size = 1.2) +
  geom_segment(data = manhattan, aes(x = x, y = y, xend = xend, yend = yend),
               color = "red", size = 1.2) +
  geom_segment(data = manhattan_2, aes(x = x, y = y, xend = xend, yend = yend),
               color = "green3", size = 1.2) +
  geom_point(size = 3) +
  labs(title="Distancias Euclídea y Manhattan") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(size = 2),
        panel.background = element_rect(fill = "gray",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"))
  
print(distancias)
