# =====================================================
# ANOSIM entre S. acapulcoensis y S. flavilatus
# =====================================================

# 1) Cargar paquetes
library(readr)
library(dplyr)
library(vegan)

# 2) Leer datos
datos <- read_csv("ITEM_dos_sp.csv", show_col_types = FALSE)

# 3) Revisar estructura
str(datos)
# Las primeras dos columnas son "especie" y "Estomago"
# El resto son ítems alimentarios en formato presencia (1) / ausencia (0)

# 4) Preparar matriz de datos y vector de grupos
mat_items <- datos %>% select(-especie, -Estomago)
grupos <- datos$especie

# 5) Calcular matriz de disimilitud (índice de Jaccard)
dist_jaccard <- vegdist(mat_items, method = "jaccard", binary = TRUE)

# 6) Ejecutar ANOSIM
set.seed(123)  # reproducibilidad
anosim_res <- anosim(dist_jaccard, grouping = grupos, permutations = 9999)

# 7) Resultados
summary(anosim_res)
plot(anosim_res, main = "ANOSIM - Composición de ítems dietarios")

# 8) Interpretación rápida
cat("\nInterpretación:\n")
cat("R > 0.5  → alta disimilitud entre especies\n")
cat("R ≈ 0    → composiciones similares\n")
cat("R < 0    → mayor similitud entre especies que dentro de ellas (inusual)\n")

# ============================================================
# SIMPER entre S. acapulcoensis y S. flavilatus (corte al 70%)
# ============================================================

# 1) Cargar paquetes
library(vegan)
library(dplyr)
library(tibble)

# 2) Ejecutar SIMPER
set.seed(123)  # reproducibilidad
simper_res <- simper(mat_items, grupos, permutations = 9999)

# 3) Verificar niveles del factor
levels(grupos)
# Debe mostrar: "S. acapulcoensis" "S. flavilatus"

# 4) Extraer resultados del contraste entre especies
# ------------------------------------------------------------
# Buscar automáticamente el contraste correcto
contraste <- names(simper_res)[grepl("acapulcoensis", names(simper_res))]

# Convertir a data.frame y calcular porcentajes
simper_tab <- as.data.frame(simper_res[[contraste]]) %>%
  rownames_to_column("Item") %>%
  mutate(across(c(average, ava, avb), as.numeric)) %>%
  mutate(
    Contribucion_pct = (average / sum(average)) * 100,
    Acumulado = cumsum(Contribucion_pct),
    Especie_dom = ifelse(ava > avb, "S. acapulcoensis", "S. flavilatus")
  )

# 5) Filtrar ítems que explican hasta el 70% de la disimilitud total
simper_70 <- simper_tab %>%
  filter(Acumulado <= 70)

# 6) Exportar tabla del corte 70%
write.csv(simper_70, "SIMPER_corte70_dieta.csv", row.names = FALSE)

# ============================================================
# Gráfico de contribución de ítems (SIMPER corte 70%)
# ============================================================

library(ggplot2)

ggplot(simper_70, aes(x = reorder(Item, Contribucion_pct),
                      y = Contribucion_pct,
                      fill = Especie_dom)) +
  geom_col(color = "black", width = 0.7) +
  coord_flip() +
  scale_fill_manual(values = c("#C16540", "#00C1C8"),
                    labels = c(expression(italic("S. acapulcoensis")),
                               expression(italic("S. flavilatus")))) +
  labs(x = NULL,
       y = "Contribución a la disimilitud (%)",
       fill = NULL) +
  theme_classic(base_family = "Arial") +
  theme(
    axis.text = element_text(size = 10, color = "black"),
    axis.title = element_text(size = 11, color = "black"),
    legend.position = "top",
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10)
  )




