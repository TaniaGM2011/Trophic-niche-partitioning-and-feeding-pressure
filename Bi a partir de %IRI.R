# Cargar Paquetes 
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(ggplot2)
library(scales)

# --------------------------
# Parámetros
# --------------------------
eps <- 0          
top_n <- 10       

# --------------------------
# 1) Leer y limpiar datos
# --------------------------
df0 <- read_csv("items_crudos.csv", show_col_types = FALSE, na = c("", "NA"))
names(df0) <- str_trim(names(df0))
df0 <- df0 %>% 
  mutate(Especie = str_trim(Especie)) %>%
  filter(!(is.na(Especie) | Especie == ""))

# *** EXCLUIR 'Microplasticos' DEL ANÁLISIS ***
cols_items <- setdiff(names(df0), c("Estomago", "Especie", "Microplasticos"))

# (si quisieras algo más robusto por si cambia el nombre, podrías usar:)
# mp_cols <- grep("microplastic", names(df0), ignore.case = TRUE, value = TRUE)
# cols_items <- setdiff(names(df0), c("Estomago", "Especie", mp_cols))

df0 <- df0 %>% 
  mutate(across(all_of(cols_items), ~ suppressWarnings(as.numeric(.x))))

df <- df0 %>%
  mutate(total_fila = rowSums(across(all_of(cols_items)), na.rm = TRUE)) %>%
  filter(total_fila > 0)

# --------------------------
# 2) Función %FO y %W
# --------------------------
calc_FO_W_raw <- function(d_especie, items, eps = 0) {
  n_est <- nrow(d_especie)
  FO <- colSums(as.matrix(d_especie[, items]) > eps, na.rm = TRUE) / n_est * 100
  item_sums <- colSums(d_especie[, items, drop = FALSE], na.rm = TRUE)
  total_sum <- sum(item_sums, na.rm = TRUE)
  W <- item_sums / total_sum * 100
  tibble(
    Item       = names(FO),
    FO_pct     = as.numeric(FO),
    W_pct      = as.numeric(W),
    N_estomagos = n_est
  )
}

# --------------------------
# 3) %FO, %W + IRI_mod y %IRI_mod
# --------------------------
res_tab <- df %>%
  group_split(Especie) %>%
  map_dfr(~{
    sp <- unique(.x$Especie)
    calc_FO_W_raw(.x, cols_items, eps = eps) %>%
      mutate(Especie = sp, .before = 1)
  }) %>%
  group_by(Especie) %>%
  mutate(
    IRI_mod     = FO_pct * W_pct,
    IRI_mod_pct = IRI_mod / sum(IRI_mod, na.rm = TRUE) * 100
  ) %>%
  ungroup()

# --------------------------
# === NUEVO: Calcular Bi por especie ===
# --------------------------
Bi_tab <- res_tab %>%
  group_by(Especie) %>%
  summarise(
    n_items = n(),
    suma_p2 = sum((IRI_mod_pct / 100)^2, na.rm = TRUE),
    Bi      = (1 / (n_items - 1)) * ((1 / suma_p2) - 1)
  )

print(Bi_tab)

# Guardar tabla de Bi
write_csv(Bi_tab, "Bi_por_especie.csv")

# === NUEVO: Gráfico del índice de amplitud trófica ===
ggplot(Bi_tab, aes(x = Especie, y = Bi, fill = Especie)) +
  geom_col(width = 0.6, color = "black") +
  scale_fill_manual(values = c("#C16540", "#00C1C8")) +
  theme_classic(base_family = "Arial") +
  labs(
    title = "Índice de amplitud trófica (Bi)",
    x = "Especie",
    y = expression(B[i])
  ) +
  theme(
    text       = element_text(size = 11, family = "Arial"),
    axis.text  = element_text(size = 11, color = "black"),
    axis.title = element_text(size = 11, color = "black"),
    legend.position = "none"
  )

# --------------------------
# 4) Gráficos originales
# --------------------------
res_top <- res_tab  

orden_items <- res_top %>%
  group_by(Item) %>%
  summarise(IRI_total = sum(IRI_mod, na.rm = TRUE), .groups = "drop") %>%
  arrange(IRI_total) %>%
  pull(Item)

col_especie <- c(
  "S. acapulcoensis" = "#C16540",
  "S. flavilatus"    = "#00C1C8"
)

# === Gráfico de %IRI (ya existente) ===
pIRI <- ggplot(res_top, aes(x = factor(Item, levels = orden_items),
                            y = IRI_mod_pct, fill = Especie)) +
  geom_col(position = position_dodge(0.8), width = 0.75, color = "grey20") +
  coord_flip() +
  scale_fill_manual(
    values = col_especie,
    labels = c(
      expression(italic("Stegastes acapulcoensis")),
      expression(italic("Stegastes flavilatus"))
    )
  ) +
  scale_y_continuous(labels = function(x) formatC(x, format = "f", digits = 0)) +
  labs(
    x   = NULL,
    y   = "Índice de importancia relativa modificado (%IRI_mod)",
    fill = NULL
  ) +  
  theme_classic(base_family = "Arial") +
  theme(
    legend.position = "top",
    text            = element_text(size = 11, family = "Arial"),  
    axis.text       = element_text(size = 11, family = "Arial", colour = "black"),
    axis.title      = element_text(size = 11, family = "Arial"),
    legend.text     = element_text(size = 11, family = "Arial")
  )

print(pIRI)



######costello#####

library(ggplot2)
library(ggrepel)
library(dplyr)

# 1) Nombres largos en itálica para facetas / colores -----------------
res_tab <- res_tab %>%
  mutate(
    Especie_lab = case_when(
      Especie == "S. acapulcoensis" ~ "italic('Stegastes acapulcoensis')",
      Especie == "S. flavilatus"    ~ "italic('Stegastes flavilatus')"
    )
  )

# 2) Preparar tabla para el gráfico -----------------------------------
#    - FO en proporción
#    - y_lab: etiqueta con pequeño desplazamiento vertical
res_tab_plot <- res_tab %>%
  mutate(FO_prop = FO_pct / 100) %>%
  group_by(Especie_lab) %>%
  mutate(
    low_W   = W_pct < 8,                                   # ítems "bajos"
    orden_x = rank(FO_prop, ties.method = "first"),        # orden en el eje x
    # patrón de offsets: 0, 1, 2, 0, 1, 2, ...
    grupo_offset = ifelse(low_W, (orden_x %% 3), 0),
    # cada grupo se levanta un poquito más (ajusta 1.2 si quieres)
    y_lab = ifelse(low_W, W_pct + grupo_offset * 1.2, W_pct)
  ) %>%
  ungroup()

# 3) Coordenadas para "a)" y "b)" -------------------------------------
facet_labels_xy <- data.frame(
  Especie_lab = c("italic('Stegastes acapulcoensis')",
                  "italic('Stegastes flavilatus')"),
  FO_prop = c(1, 1),
  W_pct   = c(55, 55),
  label   = c("a)", "b)")
)

# 4) Gráfico Costello --------------------------------------------------
pCostello_auto <- ggplot(
  res_tab_plot,
  aes(x = FO_prop, y = W_pct,
      fill = Especie_lab,
      size = IRI_mod_pct)
) +
  # PUNTOS
  geom_point(alpha = 0.9, shape = 21, colour = "black", stroke = 0.5) +
  
  # ETIQUETAS (todas), usando y_lab para separarlas
  geom_text_repel(
    aes(label = Item, force = force_factor, y = y_lab),
    size = 3,
    color = "black",
    family = "Arial",
    box.padding = 0.4,
    point.padding = 0.25,
    segment.color = "grey80",
    direction = "y",        # mueve más en vertical que en horizontal
    max.overlaps = Inf,
    min.segment.length = 0,
    show.legend = FALSE
  ) +
  
  # LÍNEAS DE REFERENCIA
  geom_vline(xintercept = 0.5, linetype = "dashed",
             colour = "grey70", linewidth = 0.6) +
  geom_hline(yintercept = 50, linetype = "dashed",
             colour = "grey70", linewidth = 0.6) +
  
  # FACETS (nombres en itálica)
  facet_wrap(
    ~ Especie_lab,
    scales   = "fixed",
    labeller = label_parsed
  ) +
  
  # a) y b)
  geom_text(
    data = facet_labels_xy,
    aes(x = FO_prop, y = W_pct, label = label),
    inherit.aes = FALSE,
    size = 4,
    family = "Arial",
    color = "black",
    hjust = 0.5, vjust = 0.5
  ) +
  
  # COLORES
  scale_fill_manual(
    values = c(
      "italic('Stegastes acapulcoensis')" = "#C16540",
      "italic('Stegastes flavilatus')"    = "#00C1C8"
    ),
    guide = "none"
  ) +
  
  # TAMAÑOS (%IRImod)
  scale_size_continuous(
    range  = c(2.5, 9),
    name   = "%IRImod",
    breaks = c(1, 5, 10, 20, 40)
  ) +
  
  # EJE X
  scale_x_continuous(
    name   = "Frecuencia de ocurrencia (proporción)",
    limits = c(-0.03, 1.1),
    breaks = seq(0, 1.1, 0.2),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  
  # EJE Y
  scale_y_continuous(
    name   = "Porcentaje en peso (%W)",
    limits = c(-0.03, 60),
    breaks = seq(0, 60, 10),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  
  # LEYENDA DE TAMAÑOS
  guides(size = guide_legend(
    title.position = "right",
    title.hjust    = 0.4,
    direction      = "horizontal",
    label.position = "bottom",
    nrow           = 1,
    override.aes   = list(shape = 21, colour = "black", fill = NA)
  )) +
  
  theme_classic(base_family = "Arial") +
  theme(
    panel.border     = element_rect(colour = "grey70", fill = NA, linewidth = 0.6),
    legend.position  = "top",
    legend.title     = element_text(size = 10, family = "Arial", face = "italic"),
    legend.text      = element_text(size = 10, family = "Arial"),
    axis.text        = element_text(size = 11, colour = "black"),
    axis.title       = element_text(size = 11, colour = "black"),
    panel.spacing    = unit(1.4, "lines"),
    plot.margin      = margin(5, 10, 5, 10)
  )

print(pCostello_auto)

# (opcional) exportar
ggsave(
  filename = "Costello_facet_auto_escalonado.tiff",
  plot = pCostello_auto,
  width = 19, height = 17, units = "cm",
  dpi = 600, compression = "lzw"
)
