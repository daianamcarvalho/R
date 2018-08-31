title: "Census"
output: html_document


{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)


{r readTrain, results="hide"}
library(readr)

diretorio_padrao <- "C:/Users/freds/Documents/desafiobb/"
train <- paste0(diretorio_padrao,"census.csv")
train <- read_csv(train)


Especificar as variáveis que são categóricas.

train$workclass <- as.factor(train$workclass)
train$education_level <- as.factor(train$education_level)
train$'marital-status' <- as.factor(train$'marital-status')
train$occupation <- as.factor(train$occupation)
train$relationship <- as.factor(train$relationship)
train$race <- as.factor(train$race)
train$sex <- as.factor(train$sex)
train$'native-country' <- as.factor(train$'native-country')
train$Target <- as.factor(train$income)

2º Passo: Análise descritiva das variáveis numéricas:
Age:
  
hist(train$age)
boxplot(train$age)
boxplot(train$age~train$income, outline = FALSE)


capital-gain:
  
(train$'capital-gain')
boxplot(train$'capital-gain')
boxplot(train$'capital-gain'~train$income, outline = FALSE)


capital-loss:
  
  hist(train$'capital-loss')
boxplot(train$'capital-loss')
boxplot(train$'capital-loss'~train$income, outline = FALSE)



hours-per-week:
  
  hist(train$'hours-per-week')
boxplot(train$hours-per-week)
boxplot(train$hours-per-week~train$income, outline = FALSE)


Análise das variáveis categóricas
sex

summary(train$sex)
plot(train$sex)
plot(train$Target~train$sex)


workclass

summary(train$workclass)
plot(train$workclass)
plot(train$Target~train$workclass)

relationship


summary(train$relationship)
plot(train$relationship)
plot(train$Target~train$relationship)

occupation

summary(train$occupation)
plot(train$occupation)
plot(train$Target~train$occupation)

race

summary(train$race)
plot(train$race)
plot(train$Target~train$race)

--Tratando dados

dados <- subset(train, (train$education_level == 'Bachelors') | (train$education_level == 'Doctorate')| (train$education_level == 'Masters')| (train$education_level == 'HS-grad')| (train$education_level == 'Some-college'))

dados2 <- subset(dados,(dados$'capital-gain' >0 )& (dados$age >=30 & dados$age <=30 ))
> print(dados2)

dados2 <- subset(dados,(dados$'capital-gain' >0 )& (dados$age >=30 & dados$age <=60 ))
nrow(dados2)
dados2$income
dados2$grupo <- sample.int(n=2, size=nrow(dados2), replace=TRUE, prob=c(.7, .3))
treino <- subset(dados2, dados2$set == 1]
treino <- subset(dados2, dados2$set == 1)
treino <- subset(dados2, dados2$grupo == 1)
nrow(treino)
teste <- subset(dados2, dados2$grupo == 2)
nrow(teste)
treino$grupo = NULL
teste$grupo = NULL
rpart(income ~ sex + education_level + workclass + age, data = treino, method="class")
arvore = rpart(income ~ sex + education_level + workclass + age, data = treino, method="class")
arvore
arvore$variable.importance
pred <- predict(arvore, teste, type='class')

vp=0
vn=0
fp=0
fn=0

for (i in 1:nrow(teste)) {
  if (as.character(pred[i]) == '>50K' & teste[i, 'income'] == '>50K' ) vp = vp + 1
  if (as.character(pred[i]) == '<=50K' & teste[i, 'income'] == '<=50K' ) vn = vn + 1
  if (as.character(pred[i]) == '>50K' & teste[i, 'income'] == '<=50K' ) fp = fp + 1
  if (as.character(pred[i]) == '<=50K' & teste[i, 'income'] == '>50K' ) fn = fn + 1
}
