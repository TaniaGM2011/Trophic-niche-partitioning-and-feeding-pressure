# Cargar Paquetes 
library(readr)
library(dplyr)
library(tidyr)
library(stringr)


# Leer y normalizar nombres

dat <- read_csv("ITEM_dos_sp.csv", show_col_types = FALSE, na = c("", "NA"))

# Estandarizar nombres clave
nms <- names(dat); nms_low <- tolower(nms)
if ("especie" %in% nms_low) names(dat)[match("especie", nms_low)]   <- "Especie"
if ("estomago" %in% nms_low) names(dat)[match("estomago", nms_low)] <- "Estomago"


# Columnas de ítems

id_cols   <- c("Especie", "Estomago")
item_cols <- setdiff(names(dat), id_cols)

# (Opcional) excluir categorías no presa:
# excluir <- c("microplasticos","sedimentos","moni")
# item_cols <- setdiff(item_cols, names(dat)[tolower(names(dat)) %in% excluir])

# Asegurar numérico 
dat <- dat %>%
  mutate(across(all_of(item_cols), ~ suppressWarnings(as.numeric(.x)))) %>%
  mutate(across(all_of(item_cols), ~ tidyr::replace_na(.x, 0)))

# %FO por especie e ítem

fo_wide <- dat %>%
  group_by(Especie) %>%
  summarise(
    N_estomagos = n(),
    across(all_of(item_cols),
           list(
             FO_n   = ~ sum(.x > 0, na.rm = TRUE),
             FO_pct = ~ 100 * sum(.x > 0, na.rm = TRUE) / n()
           ),
           .names = "{.col}__{.fn}"
    ),
    .groups = "drop"
  )


# Formato largo ordenado (fácil de leer/guardar)

fo_long <- fo_wide %>%
  pivot_longer(
    cols = -c(Especie, N_estomagos),
    names_to = c("Item","metric"),
    names_sep = "__",
    values_to = "value"
  ) %>%
  pivot_wider(names_from = metric, values_from = value) %>%
  arrange(Especie, desc(FO_pct))

# Ver resultados
print(fo_long, n = 50)

# (Opcional) guardar
write_csv(fo_long, "FO_por_especie_item.csv")
write_csv(fo_wide, "FO_por_especie_item_wide.csv")

#########################################################

# Cargar Paquetes 
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(stringr)

# Leer FO (%)
fo <- read_csv("FO_por_especie_item.csv", show_col_types = FALSE)


# Filtrar ítems con ≥60% en al menos una especie
cand <- fo %>%
  group_by(Item) %>%
  filter(any(FO_pct >= 60, na.rm = TRUE)) %>%
  ungroup()

# Ordenar ítems por FO máximo (de mayor a menor)
orden_items <- cand %>%
  group_by(Item) %>%
  summarise(FO_max = max(FO_pct, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(FO_max)) %>%
  pull(Item)

# niveles invertidos para que el mayor quede ARRIBA
cand <- cand %>%
  mutate(Item = factor(Item, levels = rev(orden_items)))


# Segmentos grises que conectan los puntos de ambas especies (opcional)
seg <- cand %>%
  group_by(Item) %>%
  summarise(xmin = min(FO_pct, na.rm = TRUE),
            xmax = max(FO_pct, na.rm = TRUE), .groups = "drop")

# Paleta y etiquetas
colores <- c("S. acapulcoensis" = "#C16540", "S. flavilatus" = "#00C1C8")
etqs    <- c(expression(italic("Stegastes acapulcoensis")), expression(italic("Stegastes flavilatus")))

# Gráfico
p <- ggplot() +
  geom_segment(data = seg,
               aes(x = xmin, xend = xmax, y = Item, yend = Item),
               inherit.aes = FALSE, linewidth = 0.5, colour = "grey70") +
  geom_point(data = cand,
             aes(x = FO_pct, y = Item, colour = Especie),
             size = 3) +
  geom_vline(xintercept = 60, linetype = "dashed", colour = "grey50") +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 20),
                     labels = scales::label_number(accuracy = 1))+
  scale_colour_manual(values = colores, labels = etqs) +
  labs(x = "Frecuencia de ocurrencia (% FO)", y = NULL, colour = NULL) +
  theme_classic(base_family = "Arial") +
  theme(
    axis.text.y   = element_text(size = 10, colour = "black"),
    axis.text.x   = element_text(size = 10, colour = "black"),
    axis.title.x  = element_text(size = 11, colour = "black"),
    legend.position = "top",
    legend.text   = element_text(size = 11, colour = "black")
  )

print(p)

# Guardar
ggsave("FO_top60_cleveland.tiff", p, dpi = 600, width = 7, height = 6, units = "in", compression = "lzw")

