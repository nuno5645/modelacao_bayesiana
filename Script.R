# ANALISE DESCRITIVA

library(readxl)
library(ggplot2)
library(corrplot)
library(cowplot)
library(skimr)
library(lavaan)
library(DMwR2)
library(semTools)
library(semPlot)
library(openxlsx)
library(rstan)
library(blavaan)
library(brms)
library(seminr)
library(blavaan)
library(bayesplot)
library(posterior)
library(FactoMineR)  # For PCA

# Definir o caminho do arquivo
file_path <- "/Users/nuno/faculdade/bayesiana/dataset.xlsx"

# Ler o arquivo Excel
dataset <- read_excel(file_path)

# Exibir as primeiras linhas do conjunto de dados
head(dataset)

# Exibir um resumo do conjunto de dados
summary(dataset)

# Exibir a estrutura do conjunto de dados 
str(dataset)

# Exibir as dimensões do conjunto de dados -> 213 observações e 29 variáveis
dim(dataset)

# Exibir os nomes das variáveis
names(dataset)

# Ver onde estão os valores omissos
missing_values <- lapply(dataset, function(x) which(is.na(x)))

# Imprimir os índices dos valores omissos
print(missing_values)

# Substituir os valores omissos por NA
dataset[is.na(dataset)] <- NA

# Verificar dataset após substituição
dataset

head(dataset)
dim(dataset)  # Verificar dimensões
summary(dataset)
sapply(dataset, function(x) length(unique(x)))
sum(is.na(dataset))  # Verificar valores omissos

# Imputar valores omissos usando imputação KNN
dataset <- knnImputation(dataset, k = 5)  # Ajustar k se necessário

# Plotar boxplot para inspeção visual das distribuições das variáveis
boxplot(dataset[,3:29])


likert_vars <- dataset[,c("CI1", "CI2", "CI3", "CI4", "CI5", "CI6",
                          "PE1", "PE2", "PE3", "PE4", "PE5", "PE6",
                          "aut1", "aut2", "aut3", "aut4",
                          "com1", "com2", "com3", "com4",
                          "rel1", "rel2", "rel3", "rel4")]


# Variáveis não-Likert
non_likert_vars <- dataset[,c("Age", "Gender", "ach1", "ach2")]

# Calcular e plotar correlações para variáveis de escala de Likert
likert_correlation <- cor(likert_vars, use = "everything", method = "spearman")  
corrplot(likert_correlation)

# Calcular e plotar correlações para variáveis não-Likert
non_likert_correlation <- cor(non_likert_vars, use = "everything", method = "pearson")  
corrplot(non_likert_correlation)


# Define latent variable groups
latent_groups <- list(
  CI = c("CI1", "CI2", "CI3", "CI4", "CI5", "CI6"),
  PE = c("PE1", "PE2", "PE3", "PE4", "PE5", "PE6"),
  AUT = c("aut1", "aut2", "aut3", "aut4"),
  COM = c("com1", "com2", "com3", "com4"),
  REL = c("rel1", "rel2", "rel3", "rel4")
)


# Applying PCA to Achievement variables
pca_achievement <- PCA(dataset[, c("ach1", "ach2")], graph = FALSE, ncp = 1)

# Adding the first principal component as a new column in the dataset
dataset$PCA_Achievement <- pca_achievement$ind$coord[, 1]

ggplot(dataset, aes(x = PCA_Achievement, fill = CI1, group = CI1)) +
  geom_density(alpha = 0.5) + 
  labs(title = "Density Plot of PCA Achievement by CI1 Group",
       x = "PCA Achievement",
       y = "Density") +
  theme_minimal()


# Plot the density for each group
p_list <- list()
for (group in latent_groups[[1]]) {
  p <- ggplot(dataset, aes(x = PCA_Achievement, fill = !!sym(group), group = !!sym(group))) +
    geom_density(alpha = 0.5) +
    labs(title = paste("Density Plot of PCA Achievement by", group, "Group"),
         x = "PCA Achievement",
         y = "Density") +
    theme_minimal()
  
  p_list[[group]] <- p
}

plot_grid(plotlist = p_list, labels = latent_groups[[1]])







scaled_dataset <- scale(dataset[, 3:29])

# Calcular Z-scores
z_scores <- abs(scaled_dataset) > 3  # Usando 3 como threshold para Z-score

# Identificar índices dos outliers
outlier_indices <- which(z_scores, arr.ind = TRUE)

# Visualizar outliers em um boxplot
boxplot(scaled_dataset, main = "Boxplot with Outliers Highlighted")

#################################################################################################

# ARTIGO

# Carregar o pacote seminr
# 

# mm <- constructs(
#   composite("CI", multi_items("CI", 1:6)),
#   composite("PE", multi_items("PE", 1:6)),
#   composite("AUT", multi_items("aut", 1:4)),
#   composite("COM", multi_items("com", 1:4)),
#   composite("REL", multi_items("rel", 1:4)),
#   composite("Achievement", multi_items("ach", 1:2), weights = mode_B)
# )

# # Definindo o modelo estrutural
# sm <- relationships(
#   paths(from = "AUT", to = c("CI", "PE", "Achievement")),
#   paths(from = "COM", to = c("CI", "PE", "Achievement")),
#   paths(from = "REL", to = c("CI", "PE", "Achievement")),
#   paths(from = "CI", to = "Achievement"),
#   paths(from = "PE", to = "Achievement")
# )

# # Ajustar o modelo PLS-SEM
# modelo_pls <- estimate_pls(data = dataset, measurement_model = mm, structural_model = sm)

# # Sumarizar os resultados
# summary(modelo_pls)

################################################################################

# PARTE 3 - modelo de medida usando Análise Fatorial Confirmatória


# Definição das variáveis latentes
model_cfa <- '
  CI =~ CI1 + CI2 + CI3 + CI4 + CI5 + CI6
  PE =~ PE1 + PE2 + PE3 + PE4 + PE5 + PE6
  AUT =~ aut1 + aut2 + aut3 + aut4
  COM =~ com1 + com2 + com3 + com4
  REL =~ rel1 + rel2 + rel3 + rel4
  Achievement =~ ach1 + ach2
'

# Ajustar o modelo CFA Bayesiano usando bcfa do blavaan
modelfit_cfa <- bcfa(
  model = model_cfa,
  data = dataset,
  std.lv = TRUE,
  n.chains = 3,
  burnin = 5000,
  sample = 1000,
  target = "stan",
  dp = dpriors(b = "normal(0,10)", theta = "gamma(1, 0.5)")
)

# Sumário dos resultados
summary(modelfit_cfa, standardized = TRUE, rsquare = TRUE, neff = TRUE, postmedian = TRUE)

# Plot prior normal para coeficientes
curve(dnorm(x, mean = 0, sd = 10), from = -30, to = 30, main = "Distribuição a priori para Coeficientes (Normal(0,10))", xlab = "Valor do Coeficiente", ylab = "Densidade")

# Plot prior gamma para desvios padrão
curve(dgamma(x, shape = 1, rate = 0.5), from = 0, to = 20, main = "Distribuição a priori para Desvios Padrão (Gamma(1,0.5))", xlab = "Valor do Desvio Padrão", ylab = "Densidade")

posterior_samples <- blavInspect(modelfit_cfa, "mcmc")

# Plot posterior distributions for coefficient parameters
bayesplot::mcmc_areas(posterior_samples, 
                      pars = grep("coef", names(posterior_samples), value = TRUE),
                      main = "Posterior Distribution for Coefficients")

# Plot posterior distributions for parameters that might follow Gamma distribution in the prior
bayesplot::mcmc_areas(posterior_samples, 
                      pars = grep("sigma|tau|sd", names(posterior_samples), value = TRUE),
                      main = "Posterior Distribution for Standard Deviations/Scales")


# # Número total de variáveis
# total_vars <- 26

# # Número de variáveis por plot
# vars_per_plot <- 6

# # Criar uma sequência de gráficos para cada grupo de variáveis
# for (i in seq(1, total_vars, by = vars_per_plot)) {
#   upper_bound <- min(i + vars_per_plot - 1, total_vars)
#   plot(modelfit_cfa, plot.type = "trace", pars = i:upper_bound)
# }

####################################################################################

# Exemplo

# x <- seq(from = -90, to = 90, by = 1)
# data <- dnorm(x, mean = 30, sd = 10)
# prior <- dnorm(x, mean = 10, sd = 5)
# posterior <- 0.5 * dnorm(x, mean = 10, sd = 5) + 0.5 * dnorm(x, mean = 30, sd = 10)

# plot(x, prior, type = "l", col = "red")
# lines(x, posterior, type = "l", col = "green")
# lines(x, data , type = "l", col = "blue")