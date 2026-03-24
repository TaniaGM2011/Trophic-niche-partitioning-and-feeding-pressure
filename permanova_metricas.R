
# cargar Paquetes 
need <- c("readr","dplyr","tibble","tidyr","vegan","ggplot2","ggrepel","ragg")
to_install <- setdiff(need, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, dependencies = TRUE)
invisible(lapply(need, library, character.only = TRUE))

# Leer datos 
df <- readr::read_csv("metricas.csv", show_col_types = FALSE) |>
  dplyr::mutate(
    especie = factor(especie, levels = c("S. acapulcoensis","S. flavilatus"))
  )

vars <- c("DO","PSO","APC","PAC","M")
stopifnot(all(vars %in% names(df)))

# remover filas con NA en variables morfológicas
df <- df |>
  tidyr::drop_na(dplyr::all_of(vars))

X <- as.data.frame(dplyr::select(df, dplyr::all_of(vars)))

# PERMANOVA con distancia euclidiana 
set.seed(123)
perm <- adonis2(X ~ especie, data = df, method = "euclidean", permutations = 9999)
print(perm)


