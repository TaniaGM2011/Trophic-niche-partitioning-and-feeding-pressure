# cargar Paquetes 
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(scales)
library(purrr)

# Leer y preparar datos

df0 <- read_csv("items_crudos.csv", show_col_types = FALSE, na = c("", "NA"))
names(df0) <- str_trim(names(df0))
df0 <- df0 %>% mutate(Especie = str_trim(Especie)) %>%
  filter(!(is.na(Especie) | Especie == ""))

# columnas de ítems (todo excepto Estomago y Especie)
cols_items <- setdiff(names(df0), c("Estomago", "Especie"))

# Excluir vertebrados si existen
excluir_items <- c("Vertebrados")
cols_items <- setdiff(cols_items, excluir_items[excluir_items %in% cols_items])

# Forzar numérico
df0 <- df0 %>% mutate(across(all_of(cols_items), ~ suppressWarnings(as.numeric(.x))))

# Mantener estómagos con contenido (>0 en algún ítem)
df <- df0 %>%
  mutate(total_fila = rowSums(across(all_of(cols_items)), na.rm = TRUE)) %>%
  filter(total_fila > 0)

# %FO y Pi (pesos crudos)

# eps: umbral mínimo para considerar "presencia" en FO y para seleccionar estómagos en Pi
eps <- 0

calc_costello <- function(d_especie, items, eps = 0) {
  n_est <- nrow(d_especie)
  X <- as.matrix(d_especie[, items, drop = FALSE])
  X[is.na(X)] <- 0
  
  # %FO sobre todos los estómagos con contenido
  FO <- colSums(X > eps) / n_est * 100
  
  # Pi: abundancia específica de la presa (peso) solo entre estómagos donde el ítem está presente
  Pi <- sapply(seq_along(items), function(k) {
    pres <- X[, k] > eps
    if (!any(pres)) return(NA_real_)
    num <- sum(X[pres, k], na.rm = TRUE)
    den <- sum(rowSums(X[pres, , drop = FALSE], na.rm = TRUE))
    if (den == 0) return(NA_real_)
    (num / den) * 100
  })
  
  # %W total (opcional: para tamaño de punto)
  W_total <- colSums(X, na.rm = TRUE)
  W_pct   <- W_total / sum(W_total, na.rm = TRUE) * 100
  
  tibble(Item = items, FO_pct = as.numeric(FO), Pi_pct = as.numeric(Pi), W_pct = as.numeric(W_pct))
}

res_costello <- df %>%
  group_split(Especie) %>%
  map_dfr(~{
    sp <- unique(.x$Especie)
    calc_costello(.x, cols_items, eps = eps) %>%
      mutate(Especie = sp, .before = 1)
  }) %>%
  filter(!is.na(Pi_pct) & FO_pct > 0)   # quitar ítems que nunca aparecen

# --------------------------
# 3) Gráfico de Costello (X = %FO, Y = Pi)
# --------------------------
library(ggrepel)
library(patchwork)

# Paleta por especie
pal_especie <- c("S. acapulcoensis" = "#C16540", "S. flavilatus" = "#00C1C8")

make_costello_plot <- function(dat){
  ggplot(dat, aes(FO_pct, Pi_pct, label = Item, color = Especie)) +
    geom_point(shape = 16, size = 3, alpha = 0.9, show.legend = TRUE) +
    geom_text_repel(
      family = "Arial", size = 3, max.overlaps = Inf,
      box.padding = 0.25, point.padding = 0.1, segment.alpha = 0.4,
      colour = "black", show.legend = FALSE
    ) +
    geom_vline(xintercept = 50, linetype = "dashed", colour = "grey40") +
    geom_hline(yintercept = 50, linetype = "dashed", colour = "grey40") +
    scale_x_continuous(limits = c(0,100),
                       labels = function(x) sprintf("%d", x)) +
    scale_y_continuous(limits = c(0,100),
                       labels = function(x) sprintf("%d", x)) +
    scale_color_manual(
      name = NULL,
      values = pal_especie,
      limits = c("S. acapulcoensis", "S. flavilatus"),
      drop   = FALSE,
      labels = c(expression(italic("Stegastes acapulcoensis")),
                 expression(italic("Stegastes flavilatus")))
    ) +
    labs(x = "Frecuencia de ocurrencia (%FO)", y = "Biomasa (%Pi)") +
    theme_classic(base_family = "Arial") +
    theme(
      text = element_text(size = 12, family = "Arial"),
      axis.text  = element_text(size = 12, family = "Arial", colour = "black"),
      axis.title = element_text(size = 12, family = "Arial", colour = "black"),
      legend.position = "top",
      legend.text = element_text(size = 12, family = "Arial")
    )
}


# --- Ejemplo de uso ---
p_acap <- make_costello_plot(subset(res_costello, Especie == "S. acapulcoensis")) +
  annotate("text", x = 95, y = 95, label = "a)", family = "Arial", size = 5)

p_flav <- make_costello_plot(subset(res_costello, Especie == "S. flavilatus")) +
  annotate("text", x = 95, y = 95, label = "b)", family = "Arial", size = 5)

# Figura con leyenda única en top
p_2col <- (p_acap + p_flav) + plot_layout(ncol = 1, guides = "collect") &
  theme(legend.position = "top")

print(p_2col)

ggsave("costello.tiff", p_2col, width = 10, height = 10, dpi = 600, compression = "lzw")

