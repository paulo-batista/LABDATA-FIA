library(readxl)
salario<-read_excel("Fatores_Impacto_Salario.xlsx", sheet="BASE DE DADOS")
View(salario)
round(cor(salario),2)

modelo_salario_v0 <- glm(over10 ~ IDADE,
                      family = binomial(link = "logit"),						
                      data = salario)
summary(modelo_salario_v0)

modelo_salario_v1 <- glm(over10 ~ ANOS_EXPERIENCIA,
                         family = binomial(link = "logit"),						
                         data = salario)
summary(modelo_salario_v1)


modelo_salario_v2 <- glm(over10 ~ .,
                      family = binomial(link = "logit"),						
                      data = salario[,-5])						
summary(modelo_salario_v2)	

library(HH)
vif(modelo_salario_v2)

  
modelo_salario_v3 <- glm(over10 ~ .,
                         family = binomial(link = "logit"),						
                         data = salario[,c(-4,-5)])						
summary(modelo_salario_v3)	

library(HH)
vif(modelo_salario_v3)

