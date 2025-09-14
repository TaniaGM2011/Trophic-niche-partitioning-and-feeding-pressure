# Paquetes
library(tidyverse)
library(ggpubr)

# 1) Leer datos
datos <- read.csv("metricas_crudos.csv", check.names = FALSE)

# Orden sugerido para la leyenda
datos$especie <- factor(datos$especie,
                        levels = c("S. acapulcoensis", "S. flavilatus"))

# 2) Variables morfológicas
variables <- c("DO","PSO","APC","PAC","M")
stopifnot(all(variables %in% names(datos)))

# 3) Shapiro–Wilk por variable y especie (manejo de n<3)
cat("=== Shapiro–Wilk por variable y especie ===\n")
for (var in variables) {
  cat("\n---", var, "---\n")
  for (sp in levels(datos$especie)) {
    x <- datos %>% filter(especie == sp) %>% pull(var)
    x <- x[is.finite(x)]
    if (length(x) < 3L) {
      cat("Especie:", sp, "- n <", 3, " (Shapiro no aplicable)\n")
    } else {
      sw <- tryCatch(shapiro.test(x), error = function(e) NULL)
      if (is.null(sw)) {
        cat("Especie:", sp, "- error en Shapiro\n")
      } else {
        cat("Especie:", sp,
            "- W =", round(unname(sw$statistic), 3),
            "- p =", signif(sw$p.value, 3), "\n")
      }
    }
  }
}

# 4) Histogramas y QQ-plots por variable (facet por especie)
for (var in variables) {
  p_hist <- ggplot(datos, aes(x = .data[[var]])) +
    geom_histogram(bins = 15, fill = "#4C72B0", color = "white") +
    facet_wrap(~ especie, nrow = 1) +
    labs(title = paste("Histograma de", var), x = var, y = "Frecuencia") +
    theme_minimal(base_family = "Arial") +
    theme(plot.title = element_text(hjust = 0.5))
  print(p_hist)
  
  p_qq <- ggplot(datos, aes(sample = .data[[var]])) +
    stat_qq() + stat_qq_line() +
    facet_wrap(~ especie, nrow = 1) +
    labs(title = paste("QQ plot de", var), x = "Cuantiles teóricos", y = "Cuantiles muestrales") +
    theme_minimal(base_family = "Arial") +
    theme(plot.title = element_text(hjust = 0.5))
  print(p_qq)
}

# 5) Datos en formato largo + cambio de etiquetas de variables
datos_largo <- datos %>%
  pivot_longer(cols = all_of(variables),
               names_to = "Variable", values_to = "Valor") %>%
  mutate(
    # Recodificación solicitada: DO→2, APC→4, PAC→5, M→6, PSO→3
    Variable_lbl = recode(Variable,
                          "DO"  = "2",
                          "APC" = "4",
                          "PAC" = "5",
                          "M"   = "6",
                          "PSO" = "3"),
    Variable_lbl = factor(Variable_lbl, levels = c("2","3","4","5","6"))
  )

# 6) Wilcoxon por variable (comparando especies)
wilcox_df <- datos_largo %>%
  group_by(Variable_lbl) %>%
  summarise(
    p = tryCatch(wilcox.test(Valor ~ especie)$p.value, error = function(e) NA_real_),
    W = tryCatch(unname(wilcox.test(Valor ~ especie)$statistic), error = function(e) NA_real_),
    y.position = max(Valor, na.rm = TRUE) * 1.05,
    group1 = "S. acapulcoensis",
    group2 = "S. flavilatus",
    .groups = "drop"
  )

# 7) Gráfico combinado por métrica con leyenda renombrada
pal_especie <- c("S. acapulcoensis" = "#C16540", "S. flavilatus" = "#00C1C8")
labs_especie <- c("S. acapulcoensis" = "Stegastes acapulcoensis",
                  "S. flavilatus"   = "Stegastes flavilatus")

p <- ggplot(datos_largo, aes(x = Variable_lbl, y = Valor, fill = especie)) +
  geom_boxplot(alpha = 0.8, color = "black", width = 0.7,
               position = position_dodge(width = 0.8), outlier.shape = 21) +
  stat_summary(aes(group = especie),
               fun = mean, geom = "point",
               position = position_dodge(width = 0.8),
               shape = 21, size = 3, color = "black", fill = "white") +
  scale_fill_manual(values = pal_especie,
                    breaks = levels(datos$especie),
                    labels = labs_especie) +
  labs(y = "Longitud (mm)", x = "Métrica morfológica") +
  theme_classic(base_family = "Arial") +
  theme(
    text = element_text(family = "Arial", size = 11, color = "black"),
    axis.title = element_text(size = 11, color = "black", face = "plain"),
    axis.text  = element_text(size = 11, color = "black"),
    legend.title = element_blank(),
    legend.text  = element_text(face = "italic", size = 11, color = "black"),
    legend.position = "top",
    legend.margin = margin(t = 5),
    plot.margin = margin(10, 10, 10, 10)
  )

print(p)

# Guardar la figura combinada
ggsave(
  filename = "F:/Doctorado/TESIS version 3/capitulo 3.- corregido/figuras/metricas.tiff",
  plot = p,
  device = "tiff",
  dpi = 500,
  compression = "lzw",
  width = 8, height = 6, units = "in"
)
wilcox_df %>%
  mutate(p = signif(p, 3)) %>%
  select(Variable_lbl, W, p) %>%
  print(n = Inf)
