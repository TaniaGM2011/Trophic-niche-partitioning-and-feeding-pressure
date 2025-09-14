
# Cargar paquetes necesarios
library(glmmTMB)
library(DHARMa)
library(tidyr)
library(dplyr)
library(ggplot2)
library(car)
library(emmeans)

# Leer y preparar datos
datos <- read.csv("frecuencia_mordidas.csv")

# Convertir a formato largo
datos_largo <- datos %>%
  pivot_longer(cols = c(Cesped, Coral, Sedimento, Esponja, Columna),
               names_to = "item", values_to = "frecuencia") %>%
  mutate(
    especie = as.factor(especie),
    profundidad = factor(profundidad, levels = c("Somero", "Intermedio", "Profundo")),
    item = factor(item, levels = c("Cesped", "Coral", "Esponja", "Sedimento", "Columna")),
    territorio = as.factor(territorio)
  )
# --- Cantidad de ceros observados ---
cat("\n--- Ceros globales ---\n")
datos_largo |>
  summarise(zeros = sum(frecuencia == 0),
            n = n(),
            prop_zeros = zeros / n) |>
  print()


# Ajustar modelo sin triple interacción #### 
modelo_sin_triple <- glmmTMB(
  frecuencia ~ especie * profundidad + especie * item + profundidad * item + (1 | territorio),
  data = datos_largo,
  family = tweedie(link = "log")
)

# Resumen del modelo y prueba tipo III
summary(modelo_sin_triple)
Anova(modelo_sin_triple, type = 3)

# Evaluación de residuos
residuos <- simulateResiduals(modelo_sin_triple)
plot(residuos)

# Comparaciones post-hoc por especie dentro de cada ítem y profundidad
comparaciones <- emmeans(modelo_sin_triple, pairwise ~ especie | item * profundidad, type = "response")
comparaciones

# Comparaciones post-hoc por especie dentro de cada ítem y profundidad
comparaciones1 <- emmeans(modelo_sin_triple, pairwise ~ especie | item, type = "response")
comparaciones1

# Comparaciones post-hoc por especie dentro de cada ítem y profundidad
comparaciones2 <- emmeans(modelo_sin_triple, pairwise ~ especie | profundidad, type = "response")
comparaciones2


# Promedios observados (media ± SE) para visualización
datos_resumen <- datos_largo %>%
  group_by(especie, profundidad, item) %>%
  summarise(
    media = mean(frecuencia),
    sd = sd(frecuencia),
    n = n(),
    se = sd / sqrt(n),
    .groups = "drop"
  )

# Estimaciones marginales (en escala original)
em_full <- emmeans(modelo_sin_triple, ~ especie * profundidad * item)
em_df <- summary(em_full, type = "response")  # Incluye IC en escala original



# Gráfico de puntos ± IC con estimaciones del modelo
frecuencia_mordidas <- ggplot(em_df, aes(x = item, y = response, color = especie, shape = especie)) +
  geom_pointrange(
    aes(ymin = asymp.LCL, ymax = asymp.UCL),
    position = position_dodge(width = 0.5),
    linewidth = 0.9
  ) +
  facet_wrap(~ profundidad) +
  # 2) etiquetas de leyenda en itálicas (para color y para forma)
  scale_color_manual(
    values = c("S. acapulcoensis" = "#C16540", "S. flavilatus" = "#00C1C8"),
    breaks = c("S. acapulcoensis", "S. flavilatus"),
    labels = c(expression(italic(Stegastes~acapulcoensis)),
               expression(italic(Stegastes~flavilatus)))
  ) +
  scale_shape_manual(
    values = c("S. acapulcoensis" = 16, "S. flavilatus" = 17),
    breaks = c("S. acapulcoensis", "S. flavilatus"),
    labels = c(expression(italic(Stegastes~acapulcoensis)),
               expression(italic(Stegastes~flavilatus)))
  ) +
  labs(
    x = "Sitio de forrajeo",
    y = "Frecuencia estimada de mordidas ± IC",
    color = NULL,   # 1) sin título de leyenda
    shape = NULL    # 1) sin título de leyenda
  ) +
  theme_classic() +
  theme(
    # 3) fuente Arial, tamaño 12, color negro para TODO el texto
    text = element_text(family = "Arial", size = 11, colour = "black"),
    axis.title = element_text(family = "Arial", size = 11, colour = "black"),
    axis.text  = element_text(family = "Arial", size = 11, colour = "black"),
    strip.text = element_text(family = "Arial", size = 11, colour = "black"),
    legend.text  = element_text(family = "Arial", size = 11, colour = "black"),
    legend.position = "top"
  )

print(frecuencia_mordidas)


# Guardar figura
ggsave(
  filename = "F:/Doctorado/TESIS version 3/capitulo 3.- corregido/figuras/frecuencia_mordidas.tiff",
  plot = frecuencia_mordidas,
  device = "tiff",
  dpi = 500,
  compression = "lzw",
  width = 12, height = 7, units = "in"
)
