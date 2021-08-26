#FIA LABDATA - MBA Analytics, Big Data e Data Science
#Módulo Analytics - Aula 1

#Configurando diretório de trabalho
#Importante: Cada analista tem seu próprio local, identifique 
#sua pasta de trablho
setwd("~/FIA/MBA/2. Analytics")

#Instalando pacote para trabalhar com Excel 
#Rodar somente uma vez
install.packages("openxlsx", dependencies = TRUE)

#Ativar o pacote openxlsx
library(openxlsx)

#Lendo o dataset
fia.dataset = read.xlsx("People_Analytics_v2.xlsx")
dados = fia.dataset

#Analisando frequência absoluta (fa) e relativa (fe) por Grau de Instrução
fa.grau.instrucao = table(dados$grau_instrucao)
fa.grau.instrucao
fr.grau.instrucao = prop.table(grau.instrucao)
fr.grau.instrucao

#Plotando  resultado em gráfico de barras, usando frequência absoluta
barplot(fa.grau.instrucao)

#Plotando resultado em gráfico de pizza, usando frequência relativa
pie(fr.grau.instrucao)

# summary apresenta as principais medidas descritivas de uma variável
# min, 1o quartil, mediana, média, 3o quartil, máxima
salario = dados$salario
medidas.resumo.salario = summary(salario)
medidas.resumo.salario

#O comando quantile apresenta os percentis desejados.
#Abaixo foi selecionado os percentis 1% e 99%
quantile(salario, c(0.0, 0.99))

#Exibe gráfico Box Plot para salários
boxplot(salario)

#Instalando e ativando moments
install.packages("moments")
library(moments)

#skewness calcula o coeficiente de assimetria
#Aqui demonstra que existe uma distribuição assimétrica para a direita
skewness(salario)

# Criação da variável filhos
filhos = dados$n_filhos
filhos

#sumarizando filhos
summary(filhos)

#Na presença de missings, a média não será calculada
#necessario ignorar dados NA, veja linha 2
mean(filhos)
mean(filhos, na.rm = TRUE)

#Criação de tabela para análise bivariada de variáveis qualitativas
Tabela <- table(dados$estado_civil, dados$grau_instrucao)

#Plotando gráfico cpm resultado das variáveis qualitativas
plot(Tabela, col = "darkturquoise")

# Box plots do salário, por grau de instrução
boxplot(dados$salario~dados$grau_instrucao, col="darkturquoise")

#Gráfico de dispersão entre idade e salário
plot(dados$idade_anos,dados$salario, col="darkturquoise")
