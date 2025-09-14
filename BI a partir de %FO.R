# Cargar Paquetes 
library(readr)
library(dplyr)
library(tidyr)
library(stringr)

#  Leer datos 
# Estructura: Estomago, Especie, luego columnas de ítems (pesos húmedos)
df <- read_csv("items.csv", show_col_types = FALSE)

#  Definir columnas e ítems a EXCLUIR 
id_cols <- c("Estomago", "Especie")
excluir_exactos <- c("Vertebrados", "Microplasticos")
pat_excluir <- regex(paste0("^(", paste(excluir_exactos, collapse="|"), ")$"),
                     ignore_case = TRUE)

items_all <- setdiff(names(df), id_cols)
items     <- items_all[!str_detect(items_all, pat_excluir)]
stopifnot(length(items) > 1)   # se necesita n>1 para estandarizar

# Convertir pesos -> presencia/ausencia y calcular %FO 
# Umbral para presencia (por si hay redondeos muy pequeños en peso)
umbral <- 1e-9

# %FO = 100 * (# estómagos con presencia) / (total de estómagos de la especie)
fo_tbl <- df %>%
  select(all_of(c(id_cols, items))) %>%
  pivot_longer(all_of(items), names_to = "Item", values_to = "peso") %>%
  mutate(peso = suppressWarnings(as.numeric(peso)),
         pres = ifelse(is.na(peso), 0, ifelse(peso > umbral, 1, 0))) %>%
  group_by(Especie, Item) %>%
  summarise(
    N_estomagos = n_distinct(Estomago),
    n_pres      = sum(pres, na.rm = TRUE),
    FO          = 100 * n_pres / N_estomagos,
    .groups = "drop"
  )

# Levins estandarizado (solo Bi) a partir de %FO 
# p_ij = FO_ij / sum_j FO_ij (dentro de especie)
# Bi   = [ (1 / sum_j p_ij^2) - 1 ] / (n - 1), con n = #ítems considerados tras la exclusión
n_items <- length(items)

Bi_tbl <- fo_tbl %>%
  group_by(Especie) %>%
  mutate(FO_sum = sum(FO, na.rm = TRUE),
         pij    = ifelse(FO_sum > 0, FO / FO_sum, NA_real_)) %>%
  summarise(
    Bi      = ( (1 / sum(pij^2, na.rm = TRUE)) - 1 ) / (n_items - 1),
    n_items = n_items,
    .groups = "drop"
  ) %>%
  mutate(Bi = round(Bi, 4))

#  Resultados 
print(Bi_tbl)

# (Opcional) guardar:
# write_csv(Bi_tbl, "Levins_Bi_estandarizado_desde_items.csv")
