## =========================================
## PCA (métricas crudas) + biplot (sin leyenda de elipses, ejes -2 a 5)
## metricas_crudos.csv
## =========================================

# Paquetes
pkgs <- c("tidyverse","factoextra","ggrepel","showtext","ragg")
to_install <- pkgs[!pkgs %in% installed.packages()[,"Package"]]
if (length(to_install)) install.packages(to_install, dependencies = TRUE)
invisible(lapply(pkgs, library, character.only = TRUE))

# Fuente (opcional)
# try({
#   showtext::font_add("Arial", "arial.ttf")
#   showtext::showtext_auto()
# }, silent = TRUE)

# 1) Datos
df <- read.csv("metricas_crudos.csv", check.names = FALSE, stringsAsFactors = FALSE)
vars <- c("2","3","4","5","6","7")
stopifnot(all(c("individuo","especie", vars) %in% names(df)))

df <- df |>
  mutate(especie = factor(especie, levels = c("S. acapulcoensis","S. flavilatus"))) |>
  drop_na(all_of(vars))

# 2) PCA
pca <- prcomp(df[, vars], center = TRUE, scale. = TRUE)

# puntuaciones individuales (scores)
scores_ind <- as.data.frame(pca$x) |>
  mutate(individuo = df$individuo, especie = df$especie)
print(scores_ind)

# Varianza explicada
ve <- (pca$sdev^2) / sum(pca$sdev^2)
lab_pc1 <- paste0("PC1 (", round(100 * ve[1], 1), "%)")
lab_pc2 <- paste0("PC2 (", round(100 * ve[2], 1), "%)")

print(pca)

# 3) Colores y etiquetas
col_especies <- c("S. acapulcoensis" = "#C16540",
                  "S. flavilatus"    = "#00C1C8")
labs_largos <- c(expression(italic("Stegastes acapulcoensis")),
                 expression(italic("Stegastes flavilatus")))

# 4) Coordenadas para elipses
ind <- get_pca_ind(pca)
ind_df <- as.data.frame(ind$coord) |>
  mutate(especie = df$especie)

# 5) Biplot
biplot_crudo <- fviz_pca_biplot(
  pca,
  geom        = "point",
  habillage   = df$especie,
  addEllipses = FALSE,
  label       = "var",
  repel       = TRUE,
  col.var     = "black",
  pointshape  = 16,
  pointsize   = 3,
  palette     = col_especies
) +
  stat_ellipse(
    data = ind_df,
    aes(x = Dim.1, y = Dim.2, color = especie, fill = especie),
    type = "t", level = 0.95,
    linewidth = 0.8, alpha = 0.18,
    show.legend = FALSE
  ) +
  scale_color_manual(values = col_especies, labels = labs_largos) +
  scale_fill_manual(values = col_especies, guide = "none") +
  labs(title = NULL, x = lab_pc1, y = lab_pc2) +
  xlim(-5, 5.5) +
  ylim(-5, 5.5) +
  theme_classic(base_family = "Arial") +
  theme(
    text = element_text(size = 11, family = "Arial"),
    axis.title = element_text(size = 11, family = "Arial"),
    axis.text  = element_text(size = 11, family = "Arial", color = "black"),  # más grandes los valores de los ejes
    legend.position = "top",
    legend.title = element_blank(),
    legend.text  = element_text(size = 11, family = "Arial")
  ) +
  guides(
    shape = "none",
    color = guide_legend(override.aes = list(size = 3))  # círculos más pequeños en la leyenda
  )

print(biplot_crudo)

# Cargas de las variables
round(pca$rotation[, 1:2], 3)

# 6) Exportar en alta resolución

ggsave(
  filename = "F:/Doctorado/TESIS version 3/capitulo 2.- corregido/figuras/ACP.tiff",
  plot = biplot_crudo ,
  device = "tiff",
  dpi = 500,
  compression = "lzw",
  width = 6, height = 6, units = "in"
)
