# ============================================================
# Density plot de longitud estándar (SL) en damiselas
# Conversión de mm a cm
# ============================================================

# Cargar paquetes
library(ggplot2)
library(readr)
library(dplyr)

# Leer base de datos
LE <- read_csv("LE.csv")

# Revisar estructura
str(LE)

# Convertir Species a factor
LE$Species <- as.factor(LE$Species)

# Convertir mm a cm
LE <- LE %>%
  mutate(SL_cm = `Standard Length` / 10)

# Colores para especies
cols <- c(
  "S. acapulcoensis" = "#C16540",
  "S. flavilatus" = "#00C1C8"
)

# Crear density plot
p <- ggplot(LE, aes(x = SL_cm, fill = Species, color = Species)) +
  
  geom_density(alpha = 0.35, linewidth = 1) +
  
  scale_fill_manual(
    values = cols,
    labels = c(
      expression(italic("Stegastes acapulcoensis")),
      expression(italic("Stegastes flavilatus"))
    )
  ) +
  
  scale_color_manual(
    values = cols,
    labels = c(
      expression(italic("Stegastes acapulcoensis")),
      expression(italic("Stegastes flavilatus"))
    )
  ) +
  
  labs(
    x = "Standard length (cm)",
    y = "Density"
  ) +
  
  theme_classic() +
  
  theme(
    text = element_text(family = "Arial", size = 11),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10, colour = "black"),
    legend.title = element_blank(),
    legend.text = element_text(family = "Arial", size = 10),
    legend.position = "top"
  ) +
  
  # Extender el eje hasta 13
  coord_cartesian(xlim = c(min(LE$SL_cm), 13))

# Mostrar gráfico
p

# Guardar figura
ggsave(
  "F:/Doctorado/TESIS version 3/Capitulo 2.- corregido/figuras/density_standard_length_damselfish.tiff",
  plot = p,
device = "tiff",
dpi = 500,
compression = "lzw",
width = 6, height = 6, units = "in"
)
