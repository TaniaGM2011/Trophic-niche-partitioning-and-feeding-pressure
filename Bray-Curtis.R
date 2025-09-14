# cargar Paquetes 
library(readr)
library(dplyr)
library(vegan)

# Leer datos 
df <- read_csv("items.csv", show_col_types = FALSE)

# Definir columnas de ítems (todas excepto Estomago y Especie)
cols_items <- setdiff(names(df), c("Estomago", "Especie"))

# Comprobación: cada estómago debe sumar 1
sums <- rowSums(df[, cols_items], na.rm = TRUE)
if (any(abs(sums - 1) > 1e-6)) {
  warning("1")
  M <- sweep(as.matrix(df[, cols_items]), 1, sums, "/")
  df[, cols_items] <- as.data.frame(M)
}

# A) BRAY–CURTIS ENTRE ESPECIES (composición promedio) 
# Promedio de proporciones por especie
comp_especie <- df |>
  group_by(Especie) |>
  summarise(across(all_of(cols_items), ~mean(.x, na.rm = TRUE)), .groups = "drop") |>
  as.data.frame()
rownames(comp_especie) <- comp_especie$Especie
comp_especie$Especie <- NULL

# (opcional normalizar por si no suman  1)
comp_especie <- sweep(comp_especie, 1, rowSums(comp_especie, na.rm = TRUE), "/")

# Distancia de Bray–Curtis (0 = idénticas, 1 = muy distintas)
bray_dist_especies <- vegdist(comp_especie, method = "bray") |> as.numeric()

# Similitud Bray–Curtis (1 - distancia)
bray_sim_especies  <- 1 - bray_dist_especies

cat("\nBray–Curtis (entre dietas promedio por especie)\n")
cat("Distancia:", round(bray_dist_especies, 3), "\n")
cat("Similitud:", round(bray_sim_especies,  3), "\n")

#  B) BRAY–CURTIS INDIVIDUO-A-INDIVIDUO CON IC 95% 
A <- df %>% filter(Especie == "S. acapulcoensis") %>% select(all_of(cols_items)) %>% as.matrix()
B <- df %>% filter(Especie == "S. flavilatus")    %>% select(all_of(cols_items)) %>% as.matrix()

# Función de similitud Bray–Curtis entre dos vectores
bray_sim_vec <- function(a, b) 1 - sum(abs(a - b)) / sum(a + b)

# Media de similitudes entre todos los pares A (filas) vs B (filas)
pair_sims <- sapply(1:nrow(A), function(i)
  sapply(1:nrow(B), function(j) bray_sim_vec(A[i, ], B[j, ])))
mean_pair_sim <- mean(pair_sims, na.rm = TRUE)

# Bootstrap de estómagos para IC 95%
set.seed(123)
B_iter <- 9999
boot_vals <- replicate(B_iter, {
  Ai <- A[sample(1:nrow(A), replace = TRUE), , drop = FALSE]
  Bj <- B[sample(1:nrow(B), replace = TRUE), , drop = FALSE]
  mean(sapply(1:nrow(Ai), function(i)
    sapply(1:nrow(Bj), function(j) bray_sim_vec(Ai[i, ], Bj[j, ]))), na.rm = TRUE)
})
IC95 <- quantile(boot_vals, c(0.025, 0.975), na.rm = TRUE)

cat("\nBray–Curtis (media de similitudes individuo-a-individuo)\n")
cat("Similitud media:", round(mean_pair_sim, 3), "\n")
cat("IC 95%:", round(IC95[1], 3), "-", round(IC95[2], 3), "\n")