# Cargar paquetes
library(glmmTMB)
library(DHARMa)
library(emmeans)
library (car)

# Cargar datos
datos <- read.csv("presion_alimenticia.csv")

# Asegurar que las variables sean factores
datos$Species <- factor(datos$Species)
datos$Depth <- factor(datos$Depth, levels = c("Shallow", "Intermediate", "Deep"))

# Modelo ZINB (con inflación de ceros)
modelo_zinf <- glmmTMB(Feeding_pressure ~ Species * Depth,
                       ziformula = ~1,
                       family = nbinom2,
                       data = datos)
summary(modelo_zinf)
Anova(modelo_zinf)

sim <- simulateResiduals(modelo_zinf)
plot(sim)
emmeans(modelo_zinf, pairwise ~ Species | Depth, type = "response")
library(ggplot2)

# Predicciones marginales
pred <- emmeans(modelo_zinf, ~ Species * Depth, type = "response")
df_pred <- as.data.frame(pred)

# Gráfico refinado y listo para publicaciós
Feeding_pressure_plot <- ggplot(df_pred, aes(x = Depth, y = response, 
                                    group = Species, color = Species, fill = Species)) +
  
  # Barras de error (IC 95%)
  geom_errorbar(
    aes(ymin = asymp.LCL, ymax = asymp.UCL),
    position = position_dodge(width = 0.25),
    width = 0.2,
    linewidth = 0.6
  ) +
  
  # Puntos con relleno por especie
  geom_point(
    position = position_dodge(width = 0.25),
    size = 3.5,
    shape = 21,
    stroke = 0.5
  ) +
  
  labs(
    y = "Feeding pressure (expected mean ± 95% CI)",
    x = "Reef zone",
    color = NULL,
    fill = NULL
  ) +
  
  # Escalas de color y relleno personalizadas
  scale_color_manual(
    values = c("S. acapulcoensis" = "#C16540",
               "S. flavilatus"    = "#00C1C8"),
    breaks = c("S. acapulcoensis", "S. flavilatus"),
    labels = c(expression(italic(Stegastes~acapulcoensis)),
               expression(italic(Stegastes~flavilatus)))
  ) +
  scale_fill_manual(
    values = c("S. acapulcoensis" = "#C16540",
               "S. flavilatus"    = "#00C1C8"),
    breaks = c("S. acapulcoensis", "S. flavilatus"),
    labels = c(expression(italic(Stegastes~acapulcoensis)),
               expression(italic(Stegastes~flavilatus)))
  ) +
  
  # Margen visual inferior
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.1))) +
  
  theme_classic(base_family = "Arial") +
  theme(
    axis.text  = element_text(size = 10, color = "black"),
    axis.title = element_text(size = 11, color = "black"),
    legend.position = "top",
    legend.direction = "horizontal",
    legend.title = element_blank(),
    legend.text = element_text(family = "Arial", size = 10),
    axis.line = element_line(linewidth = 0.4),
    axis.ticks = element_line(linewidth = 0.4),
    plot.margin = margin(10, 10, 10, 10)
  )

print(Feeding_pressure_plot)


# Guardar figura
ggsave(
  filename = "F:/Doctorado/TESIS version 3/capitulo 2.- corregido/figuras/presion.tiff",
  plot = Feeding_pressure_plot,
  device = "tiff",
  dpi = 500,
  compression = "lzw",
  width = 6, height = 6, units = "in"
)
