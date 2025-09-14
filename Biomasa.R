# Versión de glmmTMB (opcional para registro)
packageVersion("glmmTMB")

# Paquetes esenciales
library(glmmTMB)
library(DHARMa)
library(emmeans)
library(ggeffects)
library(ggplot2)
library(dplyr)
library(car)

# Leer la base
datos <- read.csv("biomasa.csv")

# Ver datos (en RStudio)
# View(datos)

# Factores y niveles 
datos <- datos %>%
  mutate(
    especie     = factor(especie, levels = c("S. acapulcoensis", "S. flavilatus")),
    profundidad = factor(profundidad, levels = c("Shallow", "Intermediate", "Deep")),
    transecto   = factor(transecto)
  )

# Chequeo rápido
datos %>%
  group_by(especie, profundidad) %>%
  summarise(n = n(), suma = sum(biomasa), .groups = "drop")

# Proporción de ceros
prop_ceros <- mean(datos$biomasa == 0); prop_ceros  # ~ 0.37

# Histograma general
ggplot(datos, aes(x = biomasa)) +
  geom_histogram(binwidth = 1, fill = "gray70", color = "black") +
  theme_classic()

# MODELO TWEEDIE
modelo_tweedie <- glmmTMB(
  biomasa ~ especie * profundidad + (1 | transecto),
  family = tweedie(link = "log"),
  data = datos
)

summary(modelo_tweedie)
car::Anova(modelo_tweedie, type = "II")  

# Diagnóstico
res <- simulateResiduals(modelo_tweedie, n = 1000)
plot(res)

# Comparaciones marginales en escala de respuesta
emm <- emmeans(modelo_tweedie, ~ especie | profundidad, type = "response")
pairs(emm)

# Predicciones para gráfico
pred <- ggpredict(modelo_tweedie, terms = c("profundidad", "especie"))  # x=profundidad, group=especie

# Posición para desplazar puntos y barras de error
pd <- position_dodge(width = 0.5)

biomasa <- ggplot(pred, aes(x = x, y = predicted, color = group)) +
  geom_point(position = pd, size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                position = pd, width = 0.2, linewidth = 0.8) +
  labs(x = "Reef zone", y = "Estimated biomass (g/m²)", color = NULL) +
  scale_color_manual(
    values = c("S. acapulcoensis" = "#C16540", "S. flavilatus" = "#00C1C8"),
    breaks = c("S. acapulcoensis", "S. flavilatus"),
    labels = c(expression(italic("Stegastes acapulcoensis")), 
               expression(italic("Stegastes flavilatus")))
  ) +
  scale_y_continuous(limits = c(0, 6)) +
  theme_classic(base_family = "Arial") +
  theme(
    legend.position = "top",
    text = element_text(size = 11, color = "black"),
    axis.text = element_text(size = 11, color = "black"),
    axis.title = element_text(size = 11, color = "black"),
    strip.text = element_text(size = 11, color = "black")
  )

# Mostrar el gráfico
print(biomasa)

# Guardar figura
ggsave(
  filename = "F:/Doctorado/TESIS version 3/capitulo 3.- corregido/figuras/biomasa.tiff",
  plot = biomasa,
  device = "tiff",
  dpi = 500,
  compression = "lzw",
  width = 6, height = 6, units = "in"
)


