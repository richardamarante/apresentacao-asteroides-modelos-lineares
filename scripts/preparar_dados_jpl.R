# Preparação dos dados de asteroides (base JPL/SBDB) para a apresentação.
# Lê o CSV bruto e o dicionário, monta a base de modelagem (casos completos),
# e pré-computa: matriz de correlação, remoção iterativa por FIV, Box-Cox e
# seleção forward por ganho de R² ajustado. Salva tudo em um .rds.

library(data.table)
library(dplyr)
library(forcats)
library(car)
library(tibble)

origem      <- Sys.getenv("ORIGEM_CSV", "trabalho/data/dataset_asteroides.csv")
dicionario  <- Sys.getenv("DIC_CSV",    "trabalho/data/dicionario_dataset_asteroides.csv")
destino     <- "trabalho/data/asteroides_modelo.rds"

# ---------------------------------------------------------------------------
# 1. Leitura (fread por mmap quebra em /mnt/c no WSL -> copia para /tmp antes)
# ---------------------------------------------------------------------------
ler_csv <- function(caminho, ...) {
  abs <- normalizePath(caminho, mustWork = TRUE)
  if (grepl("^/mnt/", abs)) {
    tmp <- tempfile(fileext = ".csv")
    file.copy(abs, tmp, overwrite = TRUE)
    on.exit(unlink(tmp), add = TRUE)
    fread(tmp, ...)
  } else {
    fread(abs, ...)
  }
}

num_cols <- c(
  "diametro", "magnitude_absoluta", "albedo", "excentricidade_orbita",
  "tamanho_orbita", "distancia_perielio", "distancia_afelio",
  "inclinacao_orbita", "orientacao_orbita", "periodo_orbita",
  "vel_media_angular_orbita", "dist_minima_terra"
)

dt <- ler_csv(origem, na.strings = c("", "NA", "null", "NaN"), showProgress = FALSE)
for (col in intersect(num_cols, names(dt))) {
  set(dt, j = col, value = suppressWarnings(as.numeric(dt[[col]])))
}

dados <- as_tibble(dt) |>
  mutate(
    neo           = dplyr::recode(neo, "sim" = "Sim", "nao" = "Não", .default = neo),
    pha           = dplyr::recode(pha, "sim" = "Sim", "nao" = "Não", .default = pha),
    classe_orbita = na_if(classe_orbita, "")
  )

# ---------------------------------------------------------------------------
# 2. Dicionário (tabela única: Variável | Unidade de medida | Descrição)
# ---------------------------------------------------------------------------
dic <- ler_csv(dicionario, na.strings = c("", "NA")) |> as_tibble()
estrutura <- dic |>
  transmute(
    `Variável`          = .data[[names(dic)[1]]],
    `Unidade de medida` = .data[[names(dic)[2]]],
    `Descrição`         = .data[[names(dic)[3]]]
  )

tipos <- vapply(dados, function(x) if (is.numeric(x)) "Numérica" else "Categórica/texto",
                character(1))

# ---------------------------------------------------------------------------
# 3. Resumo bruto da base
# ---------------------------------------------------------------------------
n_total        <- nrow(dados)
n_com_diametro <- sum(!is.na(dados$diametro) & dados$diametro > 0)
n_sem_diametro <- n_total - n_com_diametro

na_estrutura <- tibble(
  `Variável` = names(dados),
  `NAs`      = vapply(dados, function(x) sum(is.na(x)), numeric(1)),
  `% NAs`    = 100 * vapply(dados, function(x) sum(is.na(x)), numeric(1)) / n_total
)

resumo_bruto <- list(
  n_total        = n_total,
  n_com_diametro = n_com_diametro,
  n_sem_diametro = n_sem_diametro,
  pct_sem_diametro = 100 * n_sem_diametro / n_total,
  n_variaveis    = ncol(dados),
  n_numericas    = sum(tipos == "Numérica"),
  n_categoricas  = sum(tipos != "Numérica"),
  n_neo          = sum(dados$neo == "Sim", na.rm = TRUE),
  n_pha          = sum(dados$pha == "Sim", na.rm = TRUE),
  classes        = dados |> count(classe_orbita, sort = TRUE),
  estrutura      = estrutura,
  nas            = na_estrutura
)

# ---------------------------------------------------------------------------
# 4. Base de modelagem (casos completos)
# ---------------------------------------------------------------------------
dados_modelo <- dados |>
  filter(
    !is.na(diametro), diametro > 0,
    !is.na(albedo), albedo > 0,
    !is.na(magnitude_absoluta),
    !is.na(excentricidade_orbita),
    !is.na(tamanho_orbita), tamanho_orbita > 0,
    !is.na(distancia_perielio), distancia_perielio > 0,
    !is.na(distancia_afelio), distancia_afelio > 0,
    !is.na(inclinacao_orbita),
    !is.na(orientacao_orbita),
    !is.na(periodo_orbita), periodo_orbita > 0,
    !is.na(vel_media_angular_orbita),
    !is.na(dist_minima_terra),
    !is.na(classe_orbita)
  ) |>
  mutate(
    log_diametro = log(diametro),
    log_albedo   = log(albedo),
    classe_orbita = fct_lump_n(factor(classe_orbita), n = 8, other_level = "Outras"),
    neo = factor(neo),
    pha = factor(pha)
  )

set.seed(20260629)
amostra_plot <- dados_modelo |> slice_sample(n = min(25000, nrow(dados_modelo)))

# ---------------------------------------------------------------------------
# 5. Rótulos
# ---------------------------------------------------------------------------
preditores <- c("magnitude_absoluta","albedo","excentricidade_orbita","tamanho_orbita",
  "distancia_perielio","distancia_afelio","inclinacao_orbita","orientacao_orbita",
  "periodo_orbita","vel_media_angular_orbita","dist_minima_terra")

rotulos <- c(
  diametro                 = "Diâmetro",
  magnitude_absoluta       = "Magnitude absoluta",
  albedo                   = "Albedo",
  log_albedo               = "log(albedo)",
  excentricidade_orbita    = "Excentricidade",
  tamanho_orbita           = "Tamanho da órbita",
  distancia_perielio       = "Distância do periélio",
  distancia_afelio         = "Distância do afélio",
  inclinacao_orbita        = "Inclinação",
  orientacao_orbita        = "Orientação",
  periodo_orbita           = "Período orbital",
  vel_media_angular_orbita = "Velocidade angular",
  dist_minima_terra        = "Distância mínima à Terra"
)
rotular <- function(v) ifelse(is.na(rotulos[v]), v, rotulos[v])

# ---------------------------------------------------------------------------
# 6. Matriz de correlação (variáveis numéricas)
# ---------------------------------------------------------------------------
vars_corr <- c("diametro", preditores)
mat <- cor(dados_modelo[vars_corr], use = "complete.obs")
ordem_lab <- unname(rotular(vars_corr))
correlacao <- as.data.frame(as.table(mat)) |>
  setNames(c("v1", "v2", "cor")) |>
  mutate(
    x = factor(unname(rotular(as.character(v1))), levels = ordem_lab),
    y = factor(unname(rotular(as.character(v2))), levels = rev(ordem_lab))
  )

# ---------------------------------------------------------------------------
# 7. Remoção iterativa por FIV (escala original)
# ---------------------------------------------------------------------------
atuais <- preditores
fiv_passos <- list()
fiv_removidos <- c()
repeat {
  f <- as.formula(paste("diametro ~", paste(atuais, collapse = " + ")))
  v <- car::vif(lm(f, data = dados_modelo))
  remover <- if (max(v) > 5 && length(atuais) > 2) names(v)[which.max(v)] else NA_character_
  fiv_passos[[length(fiv_passos) + 1]] <- tibble(
    `Variável` = unname(rotular(names(v))),
    `FIV`      = as.numeric(v),
    removido   = names(v) == remover
  ) |> arrange(desc(`FIV`))
  if (is.na(remover)) break
  fiv_removidos <- c(fiv_removidos, remover)
  atuais <- setdiff(atuais, remover)
}
reduzido <- atuais   # conjunto final, todos com FIV <= 5

# ---------------------------------------------------------------------------
# 8. Box-Cox (justifica log da resposta) sobre o modelo reduzido
# ---------------------------------------------------------------------------
modelo_reduzido_orig <- lm(
  as.formula(paste("diametro ~", paste(reduzido, collapse = " + "))),
  data = dados_modelo
)
bc <- MASS::boxcox(modelo_reduzido_orig, lambda = seq(-1, 1, by = 0.02), plotit = FALSE)
lambda_otimo <- bc$x[which.max(bc$y)]
boxcox_df <- tibble(lambda = bc$x, log_verossimilhanca = bc$y)

# ---------------------------------------------------------------------------
# 9. Seleção forward por ganho de R² ajustado (resposta log_diametro)
# ---------------------------------------------------------------------------
forward_path <- (function() {
  dentro <- c(); fora <- reduzido; r2_ant <- 0; linhas <- list()
  repeat {
    if (!length(fora)) break
    ganhos <- sapply(fora, function(v) {
      f <- as.formula(paste("log_diametro ~", paste(c(dentro, v), collapse = " + ")))
      summary(lm(f, data = dados_modelo))$adj.r.squared
    })
    melhor <- names(which.max(ganhos)); r2 <- max(ganhos)
    linhas[[length(linhas) + 1]] <- tibble(
      Passo = length(linhas) + 1,
      `Variável incluída` = unname(rotular(melhor)),
      `R² ajustado` = r2,
      `Ganho` = r2 - r2_ant
    )
    dentro <- c(dentro, melhor); fora <- setdiff(fora, melhor); r2_ant <- r2
  }
  bind_rows(linhas)
})()
# limiar de parada: ganho < 1%
limiar_ganho <- 0.01
n_selec <- which(forward_path$Ganho < limiar_ganho)[1] - 1
if (is.na(n_selec) || n_selec < 1) n_selec <- nrow(forward_path)

# ---------------------------------------------------------------------------
# 10. Empacotamento
# ---------------------------------------------------------------------------
resumo_modelo <- list(
  n_modelo      = nrow(dados_modelo),
  lambda_boxcox = lambda_otimo,
  reduzido      = reduzido,
  reduzido_labels = unname(rotular(reduzido))
)

selecoes <- list(
  correlacao    = correlacao,
  fiv_passos    = fiv_passos,
  fiv_removidos = unname(rotular(fiv_removidos)),
  forward_path  = forward_path,
  forward_limiar = limiar_ganho,
  forward_n_selec = n_selec
)

obj <- list(
  resumo_bruto  = resumo_bruto,
  dados_modelo  = dados_modelo,
  amostra_plot  = amostra_plot,
  boxcox        = boxcox_df,
  resumo_modelo = resumo_modelo,
  selecoes      = selecoes
)

saveRDS(obj, destino)

cat("OK ->", destino, "\n")
cat("n_total:", n_total, "| sem diametro:", n_sem_diametro,
    sprintf("(%.1f%%)", 100 * n_sem_diametro / n_total), "\n")
cat("n_modelo:", nrow(dados_modelo), "| passos FIV:", length(fiv_passos),
    "| reduzido:", length(reduzido), "vars\n")
cat("removidos por FIV:", paste(unname(rotular(fiv_removidos)), collapse = ", "), "\n")
cat("lambda Box-Cox:", round(lambda_otimo, 3), "\n")
cat("forward seleciona", n_selec, "variáveis:\n")
print(as.data.frame(forward_path))
