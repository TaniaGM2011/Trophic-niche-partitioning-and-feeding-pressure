# --- Paquetes ---
library(vegan)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tidytext) 

# Leer datos
items <- read.csv("items.csv", check.names = FALSE)

# Establecer factores y matrices 
items$Especie <- factor(items$Especie)
mat_items <- dplyr::select(items, -Estomago, -Especie)

# Asegurar proporciones por fila (suma = 1)
rs <- rowSums(mat_items, na.rm = TRUE)
if (any(abs(rs - 1) > 1e-6)) {
  warning("1")
  mat_items <- sweep(as.matrix(mat_items), 1, rs, "/")
  mat_items <- as.data.frame(mat_items)
}

# Transformación square root 
mat_sqrt <- sqrt(mat_items)

# PERMANOVA (Bray–Curtis, 9,999 permutaciones)
set.seed(123)
permanova <- adonis2(mat_sqrt ~ Especie,
                     data = items,
                     method = "bray",
                     permutations = 9999,
                     by = "terms")
print(permanova)

# nMDS (Bray–Curtis sobre square root) + CONVEX HULL
set.seed(123)
nmds <- metaMDS(mat_sqrt, distance = "bray", k = 2, trymax = 200, autotransform = FALSE)

# Coordenadas de sitios
scores_df <- as.data.frame(scores(nmds, display = "sites"))
scores_df$Especie <- items$Especie

# Asegurar niveles esperados (ajusta si tus etiquetas difieren)
items$Especie  <- factor(items$Especie, levels = c("S. acapulcoensis", "S. flavilatus"))
scores_df$Especie <- items$Especie  # mantener consistencia

# Paleta por especie (nombres DEBEN coincidir con los niveles del factor)
pal_especie <- c("S. acapulcoensis" = "#C16540",
                 "S. flavilatus"    = "#00C1C8")


# Convex hull por especie 
hull_df <- scores_df %>%
  dplyr::group_by(Especie) %>%
  dplyr::filter(dplyr::n() >= 3) %>%
  dplyr::slice(chull(NMDS1, NMDS2)) %>%
  dplyr::ungroup()

p_nmds_hull <- ggplot(scores_df, aes(x = NMDS1, y = NMDS2, color = Especie)) +
  # polígonos de convex hull
  geom_polygon(data = hull_df, aes(fill = Especie, group = Especie),
               alpha = 0.20, color = NA, show.legend = FALSE) +
  # puntos
  geom_point(size = 3, alpha = 0.9) +
  # Texto del stress en esquina superior derecha
  annotate("text",
           x = max(scores_df$NMDS1) * 0.95,
           y = max(scores_df$NMDS2) * 1,
           label = paste0("Stress = ", format(round(nmds$stress, 2), nsmall = 2)),
           hjust = 1, vjust = 1, size = 3.5, family = "Arial") +
  scale_color_manual(
    values = pal_especie,
    breaks = c("S. acapulcoensis", "S. flavilatus"),
    labels = c(expression(italic("Stegastes acapulcoensis")), 
               expression(italic("Stegastes flavilatus")))
  ) +
  scale_fill_manual(values = pal_especie) +
  theme_classic(base_family = "Arial") +
  labs(title = NULL,
       x = "NMDS1", y = "NMDS2") +
  scale_y_continuous(limits = c(-0.4, 0.5)) +
  guides(color = guide_legend(title = NULL)) +
  theme(
    legend.position = "top",
    text = element_text(size = 10, family = "Arial", color = "black"),
    axis.text = element_text(size = 10, family = "Arial", color = "black"),
    axis.title = element_text(size = 10, family = "Arial", color = "black"),
    legend.text = element_text(size = 10, family = "Arial", color = "black")
  )

print(p_nmds_hull)


# Guardar figura
ggsave(
  filename = "F:/Doctorado/TESIS version 3/capitulo 3.- dieta/figures/p_nmds_hull.tiff",
  plot = p_nmds_hull,
  device = "tiff",
  dpi = 500,
  compression = "lzw",
  width = 8, height = 8, units = "in"
)

