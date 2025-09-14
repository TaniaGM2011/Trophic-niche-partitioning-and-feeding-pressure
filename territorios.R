# Instalar paquetes si no los tienes
# install.packages(c("tidyverse", "car", "vegan", "ggpubr"))

library(tidyverse)
library(car)
library(vegan)
library(ggpubr)

# === 1. Cargar datos ===

datos <- read.csv("territorio.csv")  # usa sep="," si es CSV con coma
colnames(datos) <- c("Especie", "Profundidad", "Area")  # aseguramos nombres uniformes
datos$Especie <- as.factor(datos$Especie)
datos$Profundidad <- as.factor(datos$Profundidad)

# === 2. Verificar normalidad por grupo ===
ggqqplot(datos, x = "Area", facet.by = c("Especie", "Profundidad"))

shapiro_test <- datos %>%
  group_by(Especie, Profundidad) %>%
  summarise(p = shapiro.test(Area)$p.value)

print(shapiro_test)

# === 3. Homogeneidad de varianzas ===
leveneTest(Area ~ Especie * Profundidad, data = datos)

# === 4. Visualizar datos ===
ggboxplot(datos, x = "Profundidad", y = "Area", color = "Especie", palette = "jco")

#######################
# Instalar si es necesario
# install.packages(c("dplyr", "ggplot2", "emmeans", "car"))

library(dplyr)
library(ggplot2)
library(car)
library(emmeans)


# Renombrar columnas si es necesario
colnames(datos) <- c("Especie", "Profundidad", "Area")

# Revisar estructura
str(datos)

# Asegurar que Especie y Profundidad sean factores
datos$Especie <- as.factor(datos$Especie)
datos$Profundidad <- as.factor(datos$Profundidad)

# Modelo GLM con distribución Gamma y enlace log
modelo_glm <- glm(Area ~ Especie * Profundidad, 
                  data = datos, 
                  family = Gamma(link = "log"))

# Resumen del modelo
summary(modelo_glm)

# ANOVA tipo II para evaluar efectos principales e interacción
Anova(modelo_glm)

# Calcular los emmeans
emmeans_glm <- emmeans(modelo_glm, ~ Especie * Profundidad)

# Comparaciones por pares por profundidad
contrast(emmeans_glm, method = "pairwise", by = "Profundidad", adjust = "tukey")

# Comparaciones por pares por especie
contrast(emmeans_glm, method = "pairwise", by = "Especie", adjust = "tukey")

# Comparaciones entre todas las combinaciones especie*profundidad
#pairs(emmeans_glm, adjust = "tukey")

# Visualización
plot(emmeans_glm)

library(DHARMa)
sim <- simulateResiduals(fittedModel = modelo_glm)
plot(sim)
#########################################

# Paquetes necesarios
library(emmeans)
library(ggplot2)
library(dplyr)

# Calcular emmeans del modelo
emm <- emmeans(modelo_glm, ~ Especie * Profundidad, type = "response")

# Convertir a data frame
emm_df <- as.data.frame(emm)

# Opcional: ordenar profundidad si es necesario
emm_df$Profundidad <- factor(emm_df$Profundidad, levels = c("Shallow", "Intermediate", "Deep"))

# Asignar nombres en itálica
emm_df$Especie_label <- ifelse(emm_df$Especie == "S. acapulcoensis", 
                               expression(italic("S. acapulcoensis")),
                               expression(italic("S. flavilatus")))

# Gráfico
ggplot(emm_df, aes(x = Profundidad, y = response, fill = Especie)) +
  geom_bar(stat = "identity", position = position_dodge(0.8), width = 0.7) +
  
  # Barras de error ajustadas para salir desde la parte superior de la barra
  geom_errorbar(aes(ymin = response,                     # Comienzan donde termina la barra
                    ymax = response + SE),              # Terminan en media + SE
                position = position_dodge(0.8),
                width = 0.2,
                color = "black") +
  
  scale_fill_manual(values = c("S. acapulcoensis" = "#C16540", 
                               "S. flavilatus" = "#00C1C8"),
                    labels = c(expression(italic("S. acapulcoensis")), 
                               expression(italic("S. flavilatus")))) +
  labs(x = "Depth", y = "Adjusted territory size (m²)") +
  theme_classic(base_size = 12, base_family = "Arial") +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 11, family = "Arial", color = "black"),
    axis.text = element_text(size = 11, color = "black", family = "Arial"),
    axis.title = element_text(size = 12, color = "black", family = "Arial")
  )
#ggsave("F:/Doctorado/TESIS version 3/capitulo 1.- corregido/figuras/territorio_emmeans.tiff",
 #      dpi = 500,
  #     width = 6,
   #    height = 6,
    #   units = "in",
     #  compression = "lzw")

# Gráfico
ggplot(emm_df, aes(x = Profundidad, y = response, fill = Especie)) +
  geom_bar(stat = "identity", position = position_dodge(0.8), width = 0.7) +
  
  # Barras de error ajustadas para salir desde la parte superior de la barra
  geom_errorbar(aes(ymin = response,                     # Comienzan donde termina la barra
                    ymax = response + SE),              # Terminan en media + SE
                position = position_dodge(0.8),
                width = 0.2,
                color = "black") +
  
  scale_fill_manual(values = c("S. acapulcoensis" = "#C16540", 
                               "S. flavilatus" = "#00C1C8"),
                    labels = c(expression(italic("S. acapulcoensis")), 
                               expression(italic("S. flavilatus")))) +
  labs(x = "Depth", y = "Adjusted territory size (m²)") +
  theme_classic(base_size = 12, base_family = "Arial") +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 11, family = "Arial", color = "black"),
    axis.text = element_text(size = 11, color = "black", family = "Arial"),
    axis.title = element_text(size = 12, color = "black", family = "Arial")
  )
#ggsave("F:/Doctorado/TESIS version 3/capitulo 1.- corregido/figuras/territorio_emmeans.tiff",
#      dpi = 500,
#     width = 6,
#    height = 6,
#   units = "in",
#  compression = "lzw")

