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
eps <- 0          # umbral de presencia para %FO (p.ej., 0, 1e-8, 0.001)
top_n <- 10       # número de ítems a graficar

# --------------------------
# 1) Leer y limpiar datos
# --------------------------
df0 <- read_csv("items_crudos.csv", show_col_types = FALSE, na = c("", "NA"))
names(df0) <- str_trim(names(df0))
df0 <- df0 %>% mutate(Especie = str_trim(Especie)) %>%
  filter(!(is.na(Especie) | Especie == ""))

cols_items <- setdiff(names(df0), c("Estomago", "Especie"))
df0 <- df0 %>% mutate(across(all_of(cols_items), ~ suppressWarnings(as.numeric(.x))))

# Mantener solo estómagos con contenido (>0 en al menos un ítem)
df <- df0 %>%
  mutate(total_fila = rowSums(across(all_of(cols_items)), na.rm = TRUE)) %>%
  filter(total_fila > 0)

# --------------------------
# 2) Función %FO y %W (pesos crudos)
# --------------------------
calc_FO_W_raw <- function(d_especie, items, eps = 0) {
  n_est <- nrow(d_especie)
  # %FO: presencia ( > eps )
  FO <- colSums(as.matrix(d_especie[, items]) > eps, na.rm = TRUE) / n_est * 100
  # %W: suma del ítem / suma total (en la especie)
  item_sums <- colSums(d_especie[, items, drop = FALSE], na.rm = TRUE)
  total_sum <- sum(item_sums, na.rm = TRUE)
  W <- item_sums / total_sum * 100
  tibble(Item = names(FO),
         FO_pct = as.numeric(FO),
         W_pct  = as.numeric(W),
         N_estomagos = n_est)
}

# --------------------------
# 3) %FO, %W por especie + IRI_mod y IRI_mod%
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
    IRI_mod   = FO_pct * W_pct,
    IRI_mod_pct = IRI_mod / sum(IRI_mod, na.rm = TRUE) * 100
  ) %>%
  ungroup()

# Chequeo: %W suma ~100 por especie
check_W <- res_tab %>%
  group_by(Especie) %>%
  summarise(W_total = sum(W_pct, na.rm = TRUE), .groups = "drop")
print(check_W)

# Guardar tabla completa
write_csv(res_tab %>% arrange(Especie, desc(IRI_mod_pct)),
          "FO_W_IRImod_por_especie.csv")

# --------------------------
# --------------------------
# 4) Usar TODOS los ítems
# --------------------------
res_top <- res_tab  # ahora res_top = todo

# Orden de TODOS los ítems por IRI_total ascendente (para que el eje quede legible)
orden_items <- res_top %>%
  group_by(Item) %>%
  summarise(IRI_total = sum(IRI_mod, na.rm = TRUE), .groups = "drop") %>%
  arrange(IRI_total) %>%
  pull(Item)

# Colores para especies (puedes conservar los tuyos)
col_especie <- c("S. acapulcoensis" = "#C16540",
                 "S. flavilatus"    = "#00C1C8")

# --------------------------
# 5) Gráficos %W, %FO, IRI_mod% (TODOS)
# --------------------------
pW <- ggplot(res_top, aes(x = factor(Item, levels = orden_items),
                          y = W_pct, fill = Especie)) +
  geom_col(position = position_dodge(0.8), width = 0.75, color = "grey20") +
  coord_flip() +
  scale_fill_manual(values = col_especie) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1, scale = 1)) +
  labs(x = NULL, y = "%W (porcentaje gravimétrico)",
       title = "Importancia gravimétrica por ítem",
       subtitle = "Pesos crudos; todos los ítems") +
  theme_minimal(base_family = "Arial") +
  theme(legend.position = "top")

pFO <- ggplot(res_top, aes(x = factor(Item, levels = orden_items),
                           y = FO_pct, fill = Especie)) +
  geom_col(position = position_dodge(0.8), width = 0.75, color = "grey20") +
  coord_flip() +
  scale_fill_manual(values = col_especie) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1, scale = 1)) +
  labs(x = NULL, y = "%FO (frecuencia de ocurrencia)",
       title = "Frecuencia de ocurrencia por ítem",
       subtitle = "Estómagos con el ítem / con contenido; todos los ítems") +
  theme_minimal(base_family = "Arial") +
  theme(legend.position = "top")

pIRI <- ggplot(res_top, aes(x = factor(Item, levels = orden_items),
                            y = IRI_mod_pct, fill = Especie)) +
  geom_col(position = position_dodge(0.8), width = 0.75, color = "grey20") +
  coord_flip() +
  # Etiquetas de especie en itálica
  scale_fill_manual(values = col_especie,
                    labels = c(expression(italic("Stegastes acapulcoensis")),
                               expression(italic("Stegastes flavilatus")))) +
  # Valores sin decimales en el eje horizontal (que es y antes de coord_flip)
  scale_y_continuous(labels = function(x) formatC(x, format = "f", digits = 0)) +
  labs(x = NULL, y = "Índice de importancia relativa modificado (%IRI_mod)", fill = NULL) +  
  theme_classic(base_family = "Arial") +
  theme(
    legend.position = "top",
    text = element_text(size = 11, family = "Arial"),  
    axis.text = element_text(size = 11, family = "Arial", colour = "black"),
    axis.title = element_text(size = 11, family = "Arial"),
    legend.text = element_text(size = 11, family = "Arial")
  )

print(pW)
print(pFO)
print(pIRI)

# Guardar figura
ggsave(
  filename = "F:/Doctorado/TESIS version 3/capitulo 3.- dieta/figures/pIRI.tiff",
  plot = pIRI ,
  device = "tiff",
  dpi = 500,
  compression = "lzw",
  width = 8, height = 8, units = "in"
)
