##############################
# Cargar paquetes necesarios
library(glmmTMB)
library(DHARMa)
library(tidyr)
library(dplyr)
library(ggplot2)
library(car)
library(emmeans)

# Leer y preparar datos
datos <- read.csv("frecuencia_mordidas.csv", check.names = FALSE)

# Convertir a formato largo
datos_largo <- datos %>%
  pivot_longer(
    cols = c(Algal_turf, Coral, Sponge, Sediment, Water_column),
    names_to = "item",
    values_to = "frecuencia"
  ) %>%
  mutate(
    Species = as.factor(Species),
    Depth = factor(Depth, levels = c("Deep", "Intermediate", "Shallow")),
    item = factor(
      item,
      levels = c("Algal_turf", "Coral", "Sponge", "Sediment", "Water_column"),
      labels = c("AT", "C", "Sp", "S", "WC")
    ),
    Territory = as.factor(Territory)
  )

# Ajustar modelo sin triple interacción
modelo_sin_triple <- glmmTMB(
  frecuencia ~ Species * Depth + Species * item + Depth * item + (1 | Territory),
  data = datos_largo,
  family = tweedie(link = "log")
)

# Estimaciones marginales del modelo
em_full <- emmeans(modelo_sin_triple, ~ Species * Depth * item)
em_df <- summary(em_full, type = "response")

# Asegurar el orden de factores en el data frame del modelo
em_df <- em_df %>%
  mutate(
    Depth = factor(Depth, levels = c("Deep", "Intermediate", "Shallow")),
    item = factor(item, levels = c("AT", "C", "Sp", "S", "WC")),
    Species = factor(Species, levels = c("S. acapulcoensis", "S. flavilatus"))
  )

# Heatmap
heatmap_bites <- ggplot(em_df, aes(x = item, y = Depth, fill = response)) +
  geom_tile(color = "white", linewidth = 0.6) +
  geom_text(
    aes(label = round(response, 2)),
    family = "Arial",
    size = 3.2,
    color = "black"
  ) +
  facet_wrap(~ Species, labeller = labeller(
    Species = c(
      "S. acapulcoensis" = "Stegastes acapulcoensis",
      "S. flavilatus" = "Stegastes flavilatus"
    )
  )) +
  scale_fill_gradient2(
    name = "Estimated\nbite rate",
    low = "white",
    mid = "#DCEBE8",
    high = "#0B6F69",
    midpoint = median(em_df$response)
  ) +
  labs(
    x = "Foraging site",
    y = "Depth"
  ) +
  theme_classic() +
  theme(
    text = element_text(family = "Arial", size = 11, colour = "black"),
    axis.title = element_text(family = "Arial", size = 11, colour = "black"),
    axis.text.x = element_text(family = "Arial", size = 10, colour = "black"),
    axis.text.y = element_text(
      family = "Arial",
      size = 10.2,
      colour = "black",
      angle = 90,
      vjust = 0.5,
      hjust = 0.5
    ),
    strip.text = element_text(face = "italic", family = "Arial", size = 11, colour = "black"),
    legend.title = element_text(family = "Arial", size = 10, colour = "black"),
    legend.text = element_text(family = "Arial", size = 10, colour = "black"),
    panel.grid = element_blank(),
    legend.position = "right"
  )

print(heatmap_bites)

# Guardar figura
ggsave(
  filename = "F:/Doctorado/TESIS version 3/capitulo 2.- corregido/figuras/heatmap_bite_rate.tiff",
  plot = heatmap_bites,
  device = "tiff",
  dpi = 500,
  compression = "lzw",
  width = 8,
  height = 6,
  units = "in"
)

