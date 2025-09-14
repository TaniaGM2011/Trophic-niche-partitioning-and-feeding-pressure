# Cargar librerías necesarias
library(ggplot2)
library(dplyr)
library(ggpubr)
library(car)

# Cargar datos
datos <- read.csv("F:/Doctorado/TESIS version 3/capitulo 3.- dieta/bases de datos/LI.csv", 
                  header = TRUE, sep = ",", stringsAsFactors = FALSE)

# Calcular el Índice de Longitud Intestinal (ILI)
datos <- datos %>%
  mutate(ILI = LI / LE)

# Evaluar normalidad por especie
ggplot(datos, aes(x = ILI, fill = especie)) +
  geom_histogram(alpha = 0.6, bins = 15, position = "identity", color = "black") +
  theme_classic() +
  facet_wrap(~ especie) +
  labs(title = "Distribución del ILI por especie")

# Prueba de Shapiro-Wilk por especie
cat("Shapiro-Wilk por especie:\n")
by(datos$ILI, datos$especie, shapiro.test)

# Evaluar homogeneidad de varianzas
cat("\nPrueba de Levene:\n")
leveneTest(ILI ~ especie, data = datos)

# Prueba estadística
# t de Student asumiendo varianzas iguales
t.test(ILI ~ especie, data = datos, var.equal = TRUE)

# Visualización con boxplot
# Asegurarse de que especie es factor con niveles correctos
datos$especie <- factor(datos$especie, levels = c("S. acapulcoensis", "S. flavilatus"))

# Gráfico
ILI <-ggplot(datos, aes(x = especie, y = ILI, fill = especie)) +
  geom_boxplot(alpha = 0.6, color = "black") +
  #stat_compare_means(method = "t.test", label = "p", 
                     #method.args = list(var.equal = TRUE)) +  # Mostrar p-valor sin asteriscos
  scale_fill_manual(values = c(
    "S. acapulcoensis" = "#C16540", 
    "S. flavilatus" = "#00C1C8"
  )) +
  scale_x_discrete(labels = c(
    "S. acapulcoensis" = expression(italic("S. acapulcoensis")),
    "S. flavilatus" = expression(italic("S. flavilatus"))
  )) +
  labs(x = NULL, y = "Índice de Longitud Intestinal (ILI)") +
  theme_classic(base_family = "Arial") +
  theme(
    text = element_text(size = 12, color = "black", family = "Arial"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    legend.position = "none"
  )
print(ILI)
ggsave(
  filename = "F:/Doctorado/TESIS version 3/capitulo 3.- dieta/figures/ILI.tiff",
  plot = ILI,
  device = "tiff",
  dpi = 500,
  compression = "lzw",
  width = 18, height = 14, units = "cm"
)
