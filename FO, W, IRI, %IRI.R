# Cargar Paquetes 
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(ggplot2)
library(scales)
library(ggrepel)
library(grid)

# --------------------------
# Parámetros
# --------------------------
eps <- 0          # umbral de presencia para %FO
top_n <- 10       # número de ítems a graficar

# --------------------------
# 1) Leer y limpiar datos
# --------------------------
df0 <- read_csv("items_crudos.csv", show_col_types = FALSE, na = c("", "NA"))
names(df0) <- str_trim(names(df0))

df0 <- df0 %>% 
  mutate(Species = str_trim(Species)) %>%
  filter(!(is.na(Species) | Species == ""))

# Identificar columnas de ítems
cols_items <- setdiff(names(df0), c("Stomach", "Species"))

# Eliminar Microplastics si existe
cols_items <- cols_items[cols_items != "Microplastics"]

# Convertir columnas a numéricas
df0 <- df0 %>% 
  mutate(across(all_of(cols_items), ~ suppressWarnings(as.numeric(.x))))

# Mantener solo estómagos con contenido
df <- df0 %>%
  mutate(total_fila = rowSums(across(all_of(cols_items)), na.rm = TRUE)) %>%
  filter(total_fila > 0)

# --------------------------
# 2) Función %FO y %W
# --------------------------
calc_FO_W_raw <- function(d_species, items, eps = 0) {
  n_est <- nrow(d_species)
  
  # %FO
  FO <- colSums(as.matrix(d_species[, items]) > eps, na.rm = TRUE) / n_est * 100
  
  # %W
  item_sums <- colSums(d_species[, items, drop = FALSE], na.rm = TRUE)
  total_sum <- sum(item_sums, na.rm = TRUE)
  W <- item_sums / total_sum * 100
  
  tibble(
    Item = names(FO),
    FO_pct = as.numeric(FO),
    W_pct  = as.numeric(W),
    N_stomachs = n_est
  )
}

# --------------------------
# 3) %FO, %W, IRI_mod
# --------------------------
res_tab <- df %>%
  group_split(Species) %>%
  map_dfr(~{
    sp <- unique(.x$Species)
    calc_FO_W_raw(.x, cols_items, eps = eps) %>%
      mutate(Species = sp, .before = 1)
  }) %>%
  group_by(Species) %>%
  mutate(
    IRI_mod     = FO_pct * W_pct,
    IRI_mod_pct = IRI_mod / sum(IRI_mod, na.rm = TRUE) * 100
  ) %>%
  ungroup()

# Chequeo de %W
check_W <- res_tab %>%
  group_by(Species) %>%
  summarise(W_total = sum(W_pct, na.rm = TRUE), .groups = "drop")
print(check_W)

# Guardar tabla final
write_csv(
  res_tab %>% arrange(Species, desc(IRI_mod_pct)),
  "FO_W_IRImod_by_species.csv"
)

# --------------------------
# 4) Usar TODOS los ítems
# --------------------------
res_top <- res_tab

# Orden de TODOS los ítems por IRI_total ascendente
orden_items <- res_top %>%
  group_by(Item) %>%
  summarise(IRI_total = sum(IRI_mod, na.rm = TRUE), .groups = "drop") %>%
  arrange(IRI_total) %>%
  pull(Item)

# Colores para especies
col_species <- c("S. acapulcoensis" = "#C16540",
                 "S. flavilatus"    = "#00C1C8")

# =========================
# COSTELLO
# =========================

# --------------------------
# 1) Cargar datos otra vez
# --------------------------
df0 <- read_csv("items_crudos.csv", show_col_types = FALSE, na = c("", "NA"))
names(df0) <- str_trim(names(df0))

df0 <- df0 %>% 
  mutate(Species = str_trim(Species)) %>%
  filter(!(is.na(Species) | Species == ""))

cols_items <- setdiff(names(df0), c("Stomach", "Species"))

# Excluir ítems si existen
excluir_items <- c("Chordata", "Microplastics")
cols_items <- setdiff(cols_items, excluir_items[excluir_items %in% cols_items])

df0 <- df0 %>% 
  mutate(across(all_of(cols_items), ~ suppressWarnings(as.numeric(.x))))

df <- df0 %>%
  mutate(total_fila = rowSums(across(all_of(cols_items)), na.rm = TRUE)) %>%
  filter(total_fila > 0)

# --------------------------
# 2) Función Costello (FO y Pi)
# --------------------------
eps <- 0

calc_costello <- function(d_species, items, eps = 0) {
  n_est <- nrow(d_species)
  X <- as.matrix(d_species[, items, drop = FALSE])
  X[is.na(X)] <- 0
  
  FO <- colSums(X > eps) / n_est * 100
  
  Pi <- sapply(seq_along(items), function(k) {
    pres <- X[, k] > eps
    if (!any(pres)) return(NA_real_)
    num <- sum(X[pres, k], na.rm = TRUE)
    den <- sum(rowSums(X[pres, , drop = FALSE], na.rm = TRUE))
    if (den == 0) return(NA_real_)
    (num / den) * 100
  })
  
  tibble(Item = items, FO_pct = FO, Pi_pct = Pi)
}

res_costello <- df %>%
  group_split(Species) %>%
  map_dfr(~{
    sp <- unique(.x$Species)
    calc_costello(.x, cols_items, eps = eps) %>%
      mutate(Species = sp, .before = 1)
  }) %>%
  filter(!is.na(Pi_pct) & FO_pct > 0)

# --------------------------
# 3) Convertir %FO y %Pi a proporciones (0–1)
# --------------------------
res_costello <- res_costello %>%
  mutate(
    FO_prop = FO_pct / 100,
    Pi_prop = Pi_pct / 100
  )

# --------------------------
# 4) Agregar %IRI_mod al objeto de Costello
# --------------------------
res_costello <- res_costello %>%
  left_join(
    res_tab %>% dplyr::select(Species, Item, IRI_mod_pct),
    by = c("Species", "Item")
  )

# Poner NAs en 0
res_costello <- res_costello %>%
  mutate(IRI_mod_pct = ifelse(is.na(IRI_mod_pct), 0, IRI_mod_pct))

# Etiquetas bonitas para facetas
res_costello <- res_costello %>%
  mutate(
    Species_lab = dplyr::recode(
      Species,
      "S. acapulcoensis" = "Stegastes acapulcoensis",
      "S. flavilatus"    = "Stegastes flavilatus"
    )
  )

# --------------------------
# 5) Colores
# --------------------------
pal_species <- c("S. acapulcoensis" = "#C16540",
                 "S. flavilatus"    = "#00C1C8")

# Límite común para la escala de tamaño
max_iri <- max(res_costello$IRI_mod_pct, na.rm = TRUE)

# Data para las etiquetas A y B
ann_df <- data.frame(
  Species_lab = c("Stegastes acapulcoensis", "Stegastes flavilatus"),
  x           = c(0.93, 0.93),
  y           = c(98, 98),
  label       = c("bold('A')", "bold('B')")
)

# --------------------------
# 6) Gráfico Costello
# --------------------------
p_costello <- ggplot(
  res_costello,
  aes(
    x     = FO_pct / 100,
    y     = Pi_pct,
    fill  = Species,
    size  = IRI_mod_pct,
    label = Item
  )
) +
  geom_point(
    shape       = 21,
    colour      = "grey30",
    stroke      = 0.4,
    alpha       = 0.9,
    show.legend = TRUE
  ) +
  geom_text_repel(
    family        = "Arial",
    size          = 3,
    max.overlaps  = Inf,
    box.padding   = 0.25,
    point.padding = 0.1,
    segment.alpha = 0.4,
    colour        = "black",
    show.legend   = FALSE
  ) +
  geom_vline(xintercept = 0.5, linetype = "dashed", colour = "grey40") +
  geom_hline(yintercept = 50,  linetype = "dashed", colour = "grey40") +
  geom_text(
    data        = ann_df,
    aes(x = x, y = y, label = label),
    inherit.aes = FALSE,
    family      = "Arial",
    size        = 4.5,
    parse       = TRUE
  ) +
  facet_wrap(~ Species_lab, nrow = 1) +
  scale_x_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.2),
    labels = number_format(accuracy = 0.1)
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, by = 10),
    labels = number_format(accuracy = 1)
  ) +
  scale_size_continuous(
    name   = "%IRI_mod",
    range  = c(2.5, 8),
    limits = c(0, max_iri),
    breaks = c(1, 5, 10, 20, 40),
    labels = function(x) paste0(x, "%"),
    guide  = guide_legend(
      override.aes = list(
        shape  = 21,
        fill   = NA,
        colour = "black",
        stroke = 0.4
      )
    )
  ) +
  scale_fill_manual(
    values = pal_species,
    limits = c("S. acapulcoensis", "S. flavilatus"),
    drop   = FALSE,
    guide  = "none"
  ) +
  labs(
    x    = "Frequency of occurrence (Proportion)",
    y    = "Percent weight (%W)",
    size = "%IRI_mod"
  ) +
  theme_classic(base_family = "Arial") +
  theme(
    text       = element_text(size = 11, family = "Arial"),
    axis.text  = element_text(size = 10, colour = "black"),
    axis.title = element_text(size = 11, colour = "black"),
    strip.text = element_text(size = 11, family = "Arial", face = "italic"),
    legend.position      = "top",
    legend.justification = "center",
    legend.box           = "horizontal",
    legend.key.size      = unit(0.4, "cm"),
    legend.text          = element_text(size = 10, family = "Arial"),
    panel.spacing.x      = unit(0.8, "lines")
  )

print(p_costello)

# --------------------------

# Guardar figura
ggsave(
  filename = "F:/Doctorado/TESIS version 3/capitulo 2.- corregido/figuras/costello_prop_facets_singlelegend_IRI.tiff",
  plot = p_costello,
  device = "tiff",
  dpi = 500,
  compression = "lzw",
  width = 8, height = 6, units = "in"
)


