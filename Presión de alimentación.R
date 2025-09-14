# Cargar paquetes
library(glmmTMB)
library(DHARMa)
library(emmeans)
library (car)

# Cargar datos
datos <- read.csv("presion_alimenticia.csv")

# Asegurar que las variables sean factores
datos$especie <- factor(datos$especie)
datos$profundidad <- factor(datos$profundidad, levels = c("Somero", "Intermedio", "Profundo"))

# Modelo ZINB (con inflación de ceros)
modelo_zinf <- glmmTMB(presion ~ especie * profundidad,
                       ziformula = ~1,
                       family = nbinom2,
                       data = datos)
summary(modelo_zinf)
Anova(modelo_zinf)

sim <- simulateResiduals(modelo_zinf)
plot(sim)
emmeans(modelo_zinf, pairwise ~ especie | profundidad, type = "response")
library(ggplot2)

pred <- emmeans(modelo_zinf, ~ especie * profundidad, type = "response")
df_pred <- as.data.frame(pred)
df_pred$grupo <- interaction(df_pred$profundidad, df_pred$especie, sep = "_")

# Gráfico con barras separadas (mejorado)
presion <- ggplot(df_pred, aes(x = profundidad, y = response, fill = especie)) +
  geom_col(position = position_dodge(width = 0.9), color = "black", width = 0.8) +
  # 1) La barra de error inicia donde termina la columna (ymin = response)
  geom_errorbar(
    aes(ymin = response, ymax = asymp.UCL),
    position = position_dodge(width = 0.9),
    width = 0.2
  ) +
  labs(
    y = "Presión de alimenatación\n(media esperada ± IC 95%)",
    x = "Profundidad",
    fill = NULL   # 3) Eliminar el título de la leyenda
  ) +
  # 4) Nombres en itálicas en la leyenda
  scale_fill_manual(
    values = c("S. acapulcoensis" = "#C16540",
               "S. flavilatus"    = "#00C1C8"),
    breaks = c("S. acapulcoensis", "S. flavilatus"),
    labels = c(expression(italic(Stegastes~acapulcoensis)),
               expression(italic(Stegastes~flavilatus)))
  ) +
  theme_classic(base_family = "Arial") +
  theme(
    axis.text  = element_text(size = 10, color = "black"),
    axis.title = element_text(size = 10),
    # 2) Leyenda en la parte superior
    legend.position = "top",
    # 3) Asegurar que no haya título en la leyenda
    legend.title = element_blank(),
    legend.text  = element_text(size = 10)
  )

print(presion)

# Guardar figura
ggsave(
  filename = "F:/Doctorado/TESIS version 3/capitulo 3.- corregido/figuras/presion.tiff",
  plot = presion,
  device = "tiff",
  dpi = 500,
  compression = "lzw",
  width = 7, height = 7, units = "in"
)