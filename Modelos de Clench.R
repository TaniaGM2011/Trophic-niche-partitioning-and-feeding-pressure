
# Cargar Paquetes 
need <- c("readr","dplyr","purrr","stringr","ggplot2","vegan",
          "systemfonts","ragg","dfoptim","tibble","tidyr","patchwork")
to_install <- setdiff(need, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, dependencies = TRUE)
invisible(lapply(need, library, character.only = TRUE))

# Leer datos
set.seed(123)
nperm  <- 9999
infile <- "ITEM_dos_sp.csv"
out_dir <- "resultados_accum"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# configuración
safe_font <- function(pref = "Arial"){
  fams <- try(systemfonts::system_fonts()$family, silent = TRUE)
  if (!inherits(fams, "try-error") && any(fams == pref)) return(pref)
  if (.Platform$OS.type == "windows") {
    suppressWarnings(windowsFonts(Arial = windowsFont("Arial")))
    return("Arial")
  }
  "sans"
}
BASE_FAMILY <- safe_font("Arial")

make_safe_id <- function(x){
  x <- iconv(x, to = "ASCII//TRANSLIT")
  x <- stringr::str_replace_all(x, "[^A-Za-z0-9]+", "_")
  x <- stringr::str_replace_all(x, "^_+|_+$", "")
  tolower(x)
}

write_csv_safe <- function(x, path){
  ok <- TRUE; msg <- NULL
  tryCatch(readr::write_csv(x, path),
           error = function(e){ ok <<- FALSE; msg <<- conditionMessage(e) })
  if (!ok){
    ts  <- format(Sys.time(), "%Y%m%d_%H%M%S")
    alt <- file.path(dirname(path),
                     paste0(tools::file_path_sans_ext(basename(path)),
                            "_", ts, ".csv"))
    readr::write_csv(x, alt)
    message("No se pudo escribir '", path, "'. Motivo: ", msg,
            "\nGuardado alternativo en: ", alt)
  }
}

species_color <- function(sp){
  if (sp == "S. acapulcoensis") "#C16540"
  else if (sp == "S. flavilatus") "#00C1C8"
  else "#444444"
}

# ---------- Lectura y saneo de datos ----------
df_raw <- readr::read_csv(infile, show_col_types = FALSE)

cn <- names(df_raw)
especie_col  <- cn[stringr::str_detect(cn, regex("^especie$",  ignore_case = TRUE))][1]
estomago_col <- cn[stringr::str_detect(cn, regex("^estomago$", ignore_case = TRUE))][1]
stopifnot(!is.na(especie_col), !is.na(estomago_col))

df <- df_raw %>%
  dplyr::rename(Especie = dplyr::all_of(especie_col),
                Estomago = dplyr::all_of(estomago_col))

item_cols <- setdiff(names(df), c("Especie","Estomago"))
df[item_cols] <- lapply(df[item_cols], function(x){
  x <- suppressWarnings(as.numeric(x)); x[is.na(x)] <- 0; x
})

# Modelos 
f_clench <- function(n, par){ a <- exp(par[1]); b <- exp(par[2]); (a*n)/(1 + b*n) }
f_negexp <- function(n, par){ a <- exp(par[1]); b <- exp(par[2]); (a*(1 - exp(-b*n)))/b }

slope_clench  <- function(nmax, par){ a <- exp(par[1]); b <- exp(par[2]); a/(1 + b*nmax)^2 }
slope_negexp  <- function(nmax, par){ a <- exp(par[1]); b <- exp(par[2]); a*exp(-b*nmax) }
asympt_common <- function(par){ a <- exp(par[1]); b <- exp(par[2]); a/b }

#  Métricas 
get_aic <- function(sse, n, k){ sse <- max(sse, .Machine$double.eps); n*log(sse/n) + 2*k }
get_aicc_from_aic <- function(aic, n, k){
  if (n - k - 1 <= 0) return(Inf)
  aic + (2*k*(k+1)) / (n - k - 1)
}

#  Envolturas de ajuste 
obj_sse <- function(f_model, x_n, y_sobs){
  function(p){ pred <- f_model(x_n, p); sum((y_sobs - pred)^2) }
}

fit_with_optim_method <- function(x_n, y_sobs, f_model, par0, method){
  obj <- obj_sse(f_model, x_n, y_sobs)
  res <- optim(par = par0, fn = obj, method = method, control = list(maxit = 1e5))
  pred <- f_model(x_n, res$par)
  sse  <- sum((y_sobs - pred)^2); sst <- sum((y_sobs - mean(y_sobs))^2)
  r2   <- 1 - sse/sst
  list(par = res$par, pred = pred, sse = sse, r2 = r2,
       converged = (res$convergence == 0), method_label = method)
}

fit_with_hj <- function(x_n, y_sobs, f_model, par0){
  obj <- obj_sse(f_model, x_n, y_sobs)
  res <- dfoptim::hjkb(par = par0, fn = obj, control = list(maxfeval = 1e5))
  pred <- f_model(x_n, res$par)
  sse  <- sum((y_sobs - pred)^2); sst <- sum((y_sobs - mean(y_sobs))^2)
  r2   <- 1 - sse/sst
  conv <- if (!is.null(res$convergence)) (res$convergence == 0) else TRUE
  list(par = res$par, pred = pred, sse = sse, r2 = r2,
       converged = conv, method_label = "Hooke-Jeeves")
}

fit_sequential_nm_bfgs <- function(x_n, y_sobs, f_model, par0){
  fm1 <- fit_with_optim_method(x_n, y_sobs, f_model, par0, method = "Nelder-Mead")
  fm2 <- fit_with_optim_method(x_n, y_sobs, f_model, fm1$par, method = "BFGS")
  fm2$method_label <- "Nelder-Mead -> BFGS"
  fm2$converged <- fm1$converged && fm2$converged
  fm2
}

fit_model_strategy <- function(x_n, y_sobs, f_model, par0, strategy){
  if (strategy == "quasi-newton")         return(fit_with_optim_method(x_n, y_sobs, f_model, par0, "BFGS"))
  if (strategy == "simplex")              return(fit_with_optim_method(x_n, y_sobs, f_model, par0, "Nelder-Mead"))
  if (strategy == "simplex+quasi-newton") return(fit_sequential_nm_bfgs(x_n, y_sobs, f_model, par0))
  if (strategy == "hooke-jeeves")         return(fit_with_hj(x_n, y_sobs, f_model, par0))
  stop("Estrategia desconocida")
}

pretty_strategy <- function(s){
  switch(s,
         "quasi-newton" = "Quasi-Newton",
         "simplex" = "Simplex",
         "simplex+quasi-newton" = "Simplex & Quasi-Newton",
         "hooke-jeeves" = "Hooke–Jeeves",
         s)
}

#  Curva de acumulación
accum_for_species <- function(df_sp, nperm = 9999){
  mat <- as.matrix(dplyr::select(df_sp, dplyr::all_of(item_cols)))
  acc <- vegan::specaccum(mat, method = "random", permutations = nperm)
  tibble::tibble(
    n     = seq_along(acc$richness),
    Sobs  = as.numeric(acc$richness),
    sd    = as.numeric(acc$sd),
    lwr   = pmax(Sobs - 1.96*sd, 0),
    upr   = Sobs + 1.96*sd
  )
}

# ---------- Pipeline por especie ----------
run_all_for_species <- function(sp_name){
  message("Procesando: ", sp_name)
  sp_id  <- make_safe_id(sp_name)
  sp_dir <- file.path(out_dir, sp_id)
  if (!dir.exists(sp_dir)) dir.create(sp_dir, recursive = TRUE)
  
  df_sp <- df %>% dplyr::filter(Especie == sp_name) %>% dplyr::arrange(Estomago)
  stopifnot(nrow(df_sp) > 1)
  
  acc_df <- accum_for_species(df_sp, nperm = nperm)
  x_n <- acc_df$n; y <- acc_df$Sobs
  nmax <- max(x_n); S_last <- tail(y, 1)
  
  # Colores por especie (para puntos/banda/observado)
  sp_col <- species_color(sp_name)
  
  # --- Etiquetas y límites de leyenda compartidos ---
  obs_label <- if (sp_name == "S. acapulcoensis") {
    "Observado – S. acapulcoensis"
  } else {
    "Observado – S. flavilatus"
  }
  legend_limits <- c("Modelo de Clench",
                     "Observado – S. acapulcoensis",
                     "Observado – S. flavilatus")
  
  # ----- Inicios -----
  k <- min(5, length(y))
  slope0 <- max(sum((1:k) * y[1:k]) / sum((1:k)^2), 1e-3)
  A0     <- max(S_last * 1.1, slope0 * 3)
  b0     <- max(slope0 / A0, 1e-4)
  a0     <- max(slope0, 1e-4)
  
  par0_clench <- log(c(a0, b0))
  par0_negexp <- log(c(a0, b0))
  
  strategies <- c("quasi-newton","simplex","simplex+quasi-newton","hooke-jeeves")
  all_rows <- list()
  
  for (s in strategies){
    fitC <- fit_model_strategy(x_n, y, f_clench, par0_clench, s)
    fitE <- fit_model_strategy(x_n, y, f_negexp, par0_negexp, s)
    
    models <- list(
      Clench = list(fit = fitC, f = f_clench,
                    slope = function(p) slope_clench(nmax, p),
                    asympt = asympt_common),
      NegExp = list(fit = fitE, f = f_negexp,
                    slope = function(p) slope_negexp(nmax, p),
                    asympt = asympt_common)
    )
    
    res_tbl_s <- purrr::imap_dfr(models, function(m, name){
      par  <- m$fit$par
      pred <- m$f(x_n, par)
      sse  <- sum((y - pred)^2); sst <- sum((y - mean(y))^2)
      r2   <- 1 - sse/sst
      aic  <- get_aic(sse, length(y), k = 2)
      aicc <- get_aicc_from_aic(aic, length(y), k = 2)
      slope_end <- m$slope(par)
      asympt <- m$asympt(par)
      completeness <- S_last / asympt
      
      tibble::tibble(
        Especie = sp_name,
        Especie_id = sp_id,
        Estrategia = pretty_strategy(s),
        Estrategia_key = s,
        Modelo = name,
        Metodo_ajuste = m$fit$method_label,
        Convergio = m$fit$converged,
        par1_log = par[1],
        par2_log = par[2],
        a_par = exp(par[1]),
        b_par = exp(par[2]),
        SSE = sse,
        R2  = r2,
        AIC = aic,
        AICc = aicc,
        S_asintota = asympt,
        S_obs_final = S_last,
        Completitud = completeness,
        Pendiente_final = slope_end
      )
    })
    
    all_rows[[s]] <- res_tbl_s
  }
  
  # ---- Tabla completa (8 filas por especie) ----
  res_tbl_all <- dplyr::bind_rows(all_rows)
  
  # ---- Mejor global por SSE (una fila) ----
  best_overall <- res_tbl_all %>% dplyr::slice_min(SSE, n = 1, with_ties = FALSE)
  
  # ---- Curva del mejor ajuste ----
  model_funs <- list(Clench = f_clench, NegExp = f_negexp)
  best_fun <- model_funs[[ best_overall$Modelo[[1]] ]]
  best_par <- c(best_overall$par1_log[[1]], best_overall$par2_log[[1]])
  curve_best_df <- tibble::tibble(n = x_n, S_hat = best_fun(x_n, best_par))
  
  # ---- Datos para leyenda y líneas (etiquetas compartidas) ----
  obs_line_df <- acc_df %>% dplyr::transmute(n, y = Sobs, Serie = obs_label)
  fit_line_df <- curve_best_df %>% dplyr::transmute(n, y = S_hat, Serie = "Modelo de Clench")
  
  # ---- Gráfico: SOLO el mejor modelo ----
  p <- ggplot2::ggplot(acc_df, aes(n, Sobs)) +
    ggplot2::geom_ribbon(aes(ymin = lwr, ymax = upr),
                         fill = sp_col, alpha = 0.15, show.legend = FALSE) +
    ggplot2::geom_point(size = 2, color = sp_col, show.legend = FALSE) +
    ggplot2::geom_line(data = obs_line_df,
                       aes(n, y, color = Serie, linetype = Serie), linewidth = 0.7) +
    ggplot2::geom_line(data = fit_line_df,
                       aes(n, y, color = Serie, linetype = Serie), linewidth = 1.0) +
    ggplot2::scale_color_manual(
      limits = legend_limits,
      values = c("Modelo de Clench"             = "black",
                 "Observado – S. acapulcoensis" = "#C16540",
                 "Observado – S. flavilatus"    = "#00C1C8")
    ) +
    ggplot2::scale_linetype_manual(
      limits = legend_limits,
      values = c("Modelo de Clench"             = "dashed",
                 "Observado – S. acapulcoensis" = "solid",
                 "Observado – S. flavilatus"    = "solid")
    ) +
    ggplot2::labs(
      x = "Número de estómagos", y = "Presas acumuladas",
      color = NULL, linetype = NULL
    ) +
    ggplot2::theme_classic(base_size = 11, base_family = BASE_FAMILY) +
    ggplot2::theme(legend.position = "top") +
    # ---- TAG dentro del panel (no lo tapa la leyenda) ----
  ggplot2::annotate("text", x = Inf, y = Inf,
                    label = if (sp_name == "S. acapulcoensis") "a)" else "b)",
                    hjust = 1.2, vjust = 1.6,
                   family = BASE_FAMILY, size = 4)
  
  # ---- Guardar por especie ----
  png_path <- file.path(sp_dir, paste0("curva_", sp_id, "_mejor.png"))
  pdf_path <- file.path(sp_dir, paste0("curva_", sp_id, "_mejor.pdf"))
  ggsave(png_path, p, width = 7, height = 5, dpi = 300, device = ragg::agg_png, bg = "white")
  ggsave(pdf_path, p, width = 7, height = 5, device = cairo_pdf, bg = "white")
  
  write_csv_safe(res_tbl_all,  file.path(sp_dir, paste0("resumen_modelos_", sp_id, ".csv")))
  write_csv_safe(best_overall, file.path(sp_dir, paste0("mejor_global_",    sp_id, ".csv")))
  
  list(all = res_tbl_all, best_overall = best_overall, plot = p, dir = sp_dir)
}

# ---------- Ejecutar por especie ----------
especies <- unique(df$Especie)
resultados <- purrr::map(especies, run_all_for_species)
names(resultados) <- especies

# ---------- Combinar resúmenes ----------
resumen_total <- dplyr::bind_rows(purrr::map(resultados, "all"))
write_csv_safe(resumen_total, file.path(out_dir, "resumen_modelos_todas_especies.csv"))

mejor_global <- dplyr::bind_rows(purrr::map(resultados, "best_overall"))
write_csv_safe(mejor_global, file.path(out_dir, "mejor_global_todas_especies.csv"))

# ---------- Figura conjunta (dos columnas, SIN títulos por panel) ----------
plots_sin_titulo <- purrr::imap(resultados, ~ .x$plot)
plots_sin_titulo <- plots_sin_titulo[sort(names(plots_sin_titulo))]

fig_2col <- patchwork::wrap_plots(plots_sin_titulo, ncol = 2) +
  patchwork::plot_layout(guides = "collect")

# leyenda única arriba
fig_2col <- fig_2col & ggplot2::theme(legend.position = "top")


print(fig_2col)


# ---------- Guardar figura combinada (TIFF con ragg) ----------
ggplot2::ggsave(
  filename = "F:/Doctorado/TESIS version 3/Capitulo 3.- corregido/figuras/modelo_clench.tiff",
  plot     = fig_2col,
  device   = ragg::agg_tiff,
  width    = 20, height = 15, units = "cm", dpi = 500,
  compression = "lzw",
  bg = "white"
)

message("\nListo. Carpeta raíz: ", normalizePath(out_dir))


