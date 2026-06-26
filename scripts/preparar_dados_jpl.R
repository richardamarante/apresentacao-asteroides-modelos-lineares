library(data.table)
library(dplyr)
library(forcats)
library(leaps)
library(MASS)
library(tibble)

origem <- "/mnt/c/Users/Richard Amarante/Desktop/ModelosLineares/Trabalho Asteroides/dados/dataset_jpl.csv"
destino <- "trabalho/data/asteroides_modelo.rds"

colunas <- tibble(
  original = c(
    "spkid", "full_name", "pdes", "name", "prefix", "neo", "pha", "H",
    "diameter", "albedo", "diameter_sigma", "orbit_id", "epoch",
    "epoch_mjd", "epoch_cal", "equinox", "e", "a", "q", "i", "om", "w",
    "ma", "ad", "n", "tp", "tp_cal", "per", "per_y", "moid", "moid_ld",
    "sigma_e", "sigma_a", "sigma_q", "sigma_i", "sigma_om", "sigma_w",
    "sigma_ma", "sigma_ad", "sigma_n", "sigma_tp", "sigma_per", "class",
    "rms"
  ),
  nome = c(
    "codigo_jpl", "nome_completo", "designacao_primaria", "nome", "prefixo",
    "objeto_proximo_terra", "potencialmente_perigoso", "magnitude_absoluta",
    "diametro_km", "albedo", "incerteza_diametro_km", "id_orbita",
    "epoca_juliana", "epoca_mjd", "epoca_calendario", "equinocio",
    "excentricidade", "semi_eixo_maior_ua", "distancia_perielio_ua",
    "inclinacao_graus", "longitude_no_ascendente_graus",
    "argumento_perielio_graus", "anomalia_media_graus",
    "distancia_afelio_ua", "movimento_medio_graus_dia",
    "tempo_perielio_juliano", "tempo_perielio_calendario",
    "periodo_orbital_dias", "periodo_orbital_anos",
    "distancia_minima_orbita_terra_ua",
    "distancia_minima_orbita_terra_ld",
    "incerteza_excentricidade", "incerteza_semi_eixo_maior",
    "incerteza_distancia_perielio", "incerteza_inclinacao",
    "incerteza_longitude_no_ascendente", "incerteza_argumento_perielio",
    "incerteza_anomalia_media", "incerteza_distancia_afelio",
    "incerteza_movimento_medio", "incerteza_tempo_perielio",
    "incerteza_periodo_orbital", "classe_orbital", "erro_medio_observacao"
  ),
  descricao = c(
    "Código interno do JPL",
    "Nome completo do objeto",
    "Designação primária",
    "Nome próprio, quando existe",
    "Prefixo de designação",
    "Indica se é objeto próximo da Terra",
    "Indica se é potencialmente perigoso",
    "Magnitude absoluta",
    "Diâmetro estimado em quilômetros",
    "Fração de luz refletida",
    "Incerteza do diâmetro em quilômetros",
    "Identificador da solução orbital",
    "Época orbital em data juliana",
    "Época orbital em MJD",
    "Época orbital em calendário",
    "Referencial/equinócio orbital",
    "Excentricidade orbital",
    "Semi-eixo maior em unidades astronômicas",
    "Distância do periélio em unidades astronômicas",
    "Inclinação orbital em graus",
    "Longitude do nó ascendente em graus",
    "Argumento do periélio em graus",
    "Anomalia média em graus",
    "Distância do afélio em unidades astronômicas",
    "Movimento médio em graus por dia",
    "Tempo do periélio em data juliana",
    "Tempo do periélio em calendário",
    "Período orbital em dias",
    "Período orbital em anos",
    "Distância mínima entre órbitas, em UA",
    "Distância mínima entre órbitas, em distâncias lunares",
    "Incerteza da excentricidade",
    "Incerteza do semi-eixo maior",
    "Incerteza da distância do periélio",
    "Incerteza da inclinação",
    "Incerteza da longitude do nó ascendente",
    "Incerteza do argumento do periélio",
    "Incerteza da anomalia média",
    "Incerteza da distância do afélio",
    "Incerteza do movimento médio",
    "Incerteza do tempo do periélio",
    "Incerteza do período orbital",
    "Classe orbital do objeto",
    "Erro médio quadrático da solução orbital"
  )
)

num_cols <- c(
  "H", "diameter", "albedo", "diameter_sigma", "epoch", "epoch_mjd",
  "e", "a", "q", "i", "om", "w", "ma", "ad", "n", "tp", "per", "per_y",
  "moid", "moid_ld", "sigma_e", "sigma_a", "sigma_q", "sigma_i",
  "sigma_om", "sigma_w", "sigma_ma", "sigma_ad", "sigma_n", "sigma_tp",
  "sigma_per", "rms"
)

dt <- fread(origem, na.strings = c("", "NA", "null"), showProgress = FALSE)

for (col in intersect(num_cols, names(dt))) {
  set(dt, j = col, value = suppressWarnings(as.numeric(dt[[col]])))
}

setnames(dt, colunas$original, colunas$nome)

dados <- as_tibble(dt) |>
  mutate(
    objeto_proximo_terra = recode(objeto_proximo_terra, "Y" = "Sim", "N" = "Não", .default = objeto_proximo_terra),
    potencialmente_perigoso = recode(potencialmente_perigoso, "Y" = "Sim", "N" = "Não", .default = potencialmente_perigoso),
    classe_orbital = na_if(classe_orbital, "")
  )

tipos <- vapply(dados[names(dados) %in% colunas$nome], function(x) {
  if (is.numeric(x)) "Numérica" else "Categórica/texto"
}, character(1))

estrutura <- colunas |>
  transmute(
    Variável = nome,
    Descrição = descricao,
    Tipo = unname(tipos[nome])
  )

na_estrutura <- colunas |>
  transmute(
    Variável = nome,
    `NAs` = vapply(nome, function(x) sum(is.na(dados[[x]])), numeric(1)),
    `% NAs` = 100 * `NAs` / nrow(dados)
  )

resumo_bruto <- list(
  n_total = nrow(dados),
  n_variaveis = length(colunas$nome),
  n_numericas = sum(tipos == "Numérica"),
  n_categoricas = sum(tipos != "Numérica"),
  n_neo = sum(dados$objeto_proximo_terra == "Sim", na.rm = TRUE),
  n_pha = sum(dados$potencialmente_perigoso == "Sim", na.rm = TRUE),
  classes = dados |> count(classe_orbital, sort = TRUE),
  estrutura = estrutura,
  nas = na_estrutura
)

candidatas <- c(
  "magnitude_absoluta", "albedo", "log_albedo",
  "excentricidade", "semi_eixo_maior_ua", "log_semi_eixo_maior",
  "distancia_perielio_ua", "distancia_afelio_ua",
  "inclinacao_graus", "longitude_no_ascendente_graus",
  "argumento_perielio_graus", "anomalia_media_graus",
  "movimento_medio_graus_dia", "periodo_orbital_dias",
  "log_periodo_orbital", "distancia_minima_orbita_terra_ua",
  "log_distancia_minima_orbita_terra", "erro_medio_observacao",
  "classe_orbital", "objeto_proximo_terra", "potencialmente_perigoso"
)

dados_modelo <- dados |>
  dplyr::select(
    diametro_km,
    dplyr::all_of(setdiff(candidatas, c(
      "log_albedo",
      "log_semi_eixo_maior",
      "log_periodo_orbital",
      "log_distancia_minima_orbita_terra"
    )))
  ) |>
  filter(
    diametro_km > 0,
    albedo > 0,
    semi_eixo_maior_ua > 0,
    periodo_orbital_dias > 0,
    distancia_minima_orbita_terra_ua > 0
  ) |>
  mutate(
    log_diametro = log(diametro_km),
    log_albedo = log(albedo),
    log_semi_eixo_maior = log(semi_eixo_maior_ua),
    log_periodo_orbital = log(periodo_orbital_dias),
    log_distancia_minima_orbita_terra = log(distancia_minima_orbita_terra_ua)
  ) |>
  dplyr::select(diametro_km, log_diametro, dplyr::all_of(candidatas)) |>
  filter(if_all(everything(), ~ !is.na(.x))) |>
  mutate(
    classe_orbital = fct_lump_n(factor(classe_orbital), n = 8, other_level = "Outras"),
    objeto_proximo_terra = factor(objeto_proximo_terra),
    potencialmente_perigoso = factor(potencialmente_perigoso)
  )

set.seed(20260626)
amostra_plot <- dados_modelo |>
  slice_sample(n = min(25000, nrow(dados_modelo)))

modelo_boxcox_base <- lm(
  diametro_km ~ magnitude_absoluta + albedo +
    excentricidade + semi_eixo_maior_ua + distancia_perielio_ua +
    distancia_afelio_ua + inclinacao_graus + distancia_minima_orbita_terra_ua,
  data = dados_modelo
)

bc <- boxcox(modelo_boxcox_base, lambda = seq(-1, 1, by = 0.02), plotit = FALSE)
lambda_otimo <- bc$x[which.max(bc$y)]
boxcox_df <- tibble(lambda = bc$x, log_verossimilhanca = bc$y)

modelo_selecao <- lm(
  log_diametro ~ magnitude_absoluta + log_albedo +
    excentricidade + log_semi_eixo_maior + distancia_perielio_ua +
    distancia_afelio_ua + inclinacao_graus +
    log_distancia_minima_orbita_terra + erro_medio_observacao +
    classe_orbital + objeto_proximo_terra + potencialmente_perigoso,
  data = dados_modelo
)

modelo_final <- lm(
  log_diametro ~ magnitude_absoluta + log_albedo,
  data = dados_modelo
)

variaveis_selecao <- c(
  "magnitude_absoluta", "log_albedo", "excentricidade",
  "log_semi_eixo_maior", "distancia_perielio_ua", "distancia_afelio_ua",
  "inclinacao_graus", "log_distancia_minima_orbita_terra",
  "erro_medio_observacao"
)

set.seed(20260626)
dados_selecao <- dados_modelo |>
  dplyr::select(log_diametro, dplyr::all_of(variaveis_selecao)) |>
  slice_sample(n = min(50000, nrow(dados_modelo)))

subconjuntos <- regsubsets(
  log_diametro ~ .,
  data = dados_selecao,
  nvmax = 6,
  method = "exhaustive"
)

sub_resumo <- summary(subconjuntos)
selecionadas <- apply(sub_resumo$outmat, 1, function(x) {
  paste(names(x)[x == "*"], collapse = ", ")
})

selecionadas <- recode(
  selecionadas,
  "magnitude_absoluta" = "Magnitude absoluta",
  "magnitude_absoluta, log_albedo" = "Magnitude absoluta, log(albedo)",
  .default = selecionadas
)

tabela_subconjuntos <- tibble(
  `Número de preditores` = seq_along(selecionadas),
  `Preditores selecionados` = selecionadas,
  BIC = sub_resumo$bic,
  `R² ajustado` = sub_resumo$adjr2
)

selecoes <- list(
  modelo_cheio = modelo_selecao,
  modelo_final = modelo_final,
  subconjuntos = tabela_subconjuntos
)

aic_comparacao <- tibble(
  Modelo = c("Cheio", "Físico simples"),
  Preditores = c(
    "Magnitude, albedo, órbita, classe, NEO e PHA",
    "Magnitude absoluta e log(albedo)"
  ),
  AIC = c(AIC(modelo_selecao), AIC(modelo_final)),
  BIC = c(BIC(modelo_selecao), BIC(modelo_final)),
  `R² ajustado` = c(
    summary(modelo_selecao)$adj.r.squared,
    summary(modelo_final)$adj.r.squared
  )
)

resumo_modelo <- list(
  n_modelo = nrow(dados_modelo),
  lambda_boxcox = lambda_otimo,
  aic_comparacao = aic_comparacao
)

obj <- list(
  resumo_bruto = resumo_bruto,
  dados_modelo = dados_modelo,
  amostra_plot = amostra_plot,
  boxcox = boxcox_df,
  resumo_modelo = resumo_modelo,
  selecoes = selecoes
)

saveRDS(obj, destino)
