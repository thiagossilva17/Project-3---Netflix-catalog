library(dplyr)
library(pacman)
BD <- read.csv2("C:/Users/Thiago Silva/Documents/DataScience/ProcessoSeletivo/EleflowBigData/Netflix.csv", sep="," , header = TRUE , encoding="UTF-8", na.strings ="", stringsAsFactors = FALSE)

head(BD)
glimpse(BD)
dim(BD)

colnames(BD)

#Removendo dados desnecessarios
BD$X <- NULL
BD$show_id <- NULL
#Convertendo rating para tipo inteiro
BD$rating <- as.integer(BD$rating)

#Checando dados faltantes
subs <- BD
data.frame("variable"=c(colnames(subs)), 
           "missing values count"=sapply(subs, function(x) sum(is.na(x))),
           row.names=NULL)
library(naniar)
gg_miss_upset(BD)
#Verificando se ha algum valor NA no dataset
anyNA(BD)
#Porcentagem de valores NAS em cada coluna
NAS <- round(colSums(is.na(BD))*100/nrow(BD), 2)
#VER TODOS NAS
NAS

#Removendo dados faltantes
dim(BD)
BD<- na.omit(BD)
dim(BD)


head(BD)
#Inserindo atributo de filmes produzidos nos Estados Unidos
BD$US <- grepl("United States", BD[,5])
BD$US <- as.character(BD$US)
BD$US <- gsub("TRUE" ,1, BD$US)
BD$US <- gsub("FALSE" ,0, BD$US)

#Categorizando filmes e TV Shows
BD$typemov <- grepl("Movie", BD[,1])
BD$typemov <- as.character(BD$typemov)
BD$typemov <- gsub("TRUE" ,1, BD$typemov)
BD$typemov <- gsub("FALSE" ,0, BD$typemov)

#Calculando e inserindo atributo da m?dia dos diretores
MeanRating <- BD %>% group_by(director) %>% summarise(AvMedia= mean(rating))
BD <- BD %>% 
  left_join(MeanRating, by = c('director'))

#SELECIONANDO ATRIBUTOS A SEREM UTILIZADOS
BCKP <- BD
BD<- BCKP[,c(8,12:14)]

head(BD)
glimpse(BD)
#Determinando avalia??es de producoes aprovadas
BD$rating[BD$rating>=70]<- "SIM"
BD$rating[BD$rating<70]<- "NAO"
table(BD$rating)
prop.table(table(BD$rating))

BD$rating <- as.factor(BD$rating)
BD$US <- as.factor(BD$US)
BD$typemov <- as.factor(BD$typemov)
glimpse(BD)

library(caret)
#Criando matriz com linhas dos dados de treino - 70%
set.seed(1)
#Separando dados de treino e teste
filtro <- createDataPartition(y=BD$rating, p=0.7, list=FALSE)
treino <- BD[filtro,]
teste <- BD[-filtro,]

#Criando o modelo de regressao logistica
set.seed(1)
modelo <- train(rating ~ ., data=treino, method="glmnet", tuneLenght=4, trControl = trainControl(method="cv", number = 5))

#Precisao no modelo de treino
mean(modelo$resample$Accuracy)

#Prevendo dados no modelo de teste
Prev <- predict(modelo, teste)
#View(data.frame(teste$Diagnosis, Prev))

library(gmodels)
#Visualizando matriz de confusao
CrossTable(Prev, teste$rating, dnn = c("Previsto", "Real"), prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE, prop.c = FALSE)

#Verificando acuracia
confusionMatrix(Prev, teste$rating, dnn = c("Previsto", "Real"))

#ROC/ AUC
#Calculando Probabilidades de ser aceito no catalogo ou nao
PrevProb <- predict(modelo, teste, type="prob")

#Visualizando probabilidades
head(round(PrevProb, 2))

#Precisa-se armazenar apenas uma das colunas de vetores, neste caso as probabilidades dos belignos
PrevProb <- PrevProb$SIM

#install.packages("pROC")
library(pROC)

#Sensibilidade = 1
#TPR = sensitivity = TP/ (TP+FN)
#Especificidade = 1
#FPR= 1- Specificity = FP/ (FP+TN)

#Curva ROC
#Calculando os valores com base nos dados de teste em função da probabilidade
ROC <- roc(teste$rating ~ PrevProb, levels= c("NAO", "SIM"))

#?roc
plot(ROC)

ROC$auc

#Criando data frame com sensibilidade, especificidade e thresholds arrendondando pela segunda casa
head(round(data.frame(ROC$sensitivities, ROC$specificities, ROC$thresholds),2))

#Resultado do modelo
confusionMatrix(Prev, teste$rating, dnn=c("Previsto", "Real"))





################################################################################################################
################################################################################################################
#Criando o modelo de arvore de decisao
set.seed(1)

library(rpart)
modelo <- rpart(rating ~., data =treino)

library(caret)
confusionMatrix(predict(modelo, teste, type = "class"), teste$rating)$overall["Accuracy"]

library(rpart.plot)
prp(modelo, extra=1)

confusionMatrix(predict(modelo, teste, type="class"), teste$rating)
confusionMatrix(predict(modelo, teste, type="class"), teste$rating)$overall["Accuracy"]

#Prevendo dados no modelo de teste
Prev <- predict(modelo, teste)
#View(data.frame(teste$Diagnosis, Prev))

#ROC/ AUC
#Calculando Probabilidades de ser aceito no catalogo ou nao
PrevProb <- as.data.frame(predict(modelo, teste, type="prob"))

#Visualizando probabilidades
head(round(PrevProb, 2))

#Precisa-se armazenar apenas uma das colunas de vetores, neste caso as probabilidades dos belignos
PrevProb <- PrevProb$SIM

#install.packages("pROC")
library(pROC)

#Sensibilidade = 1
#TPR = sensitivity = TP/ (TP+FN)
#Especificidade = 1
#FPR= 1- Specificity = FP/ (FP+TN)

#Curva ROC
#Calculando os valores com base nos dados de teste em função da probabilidade
ROC <- roc(teste$rating ~ PrevProb, levels= c("NAO", "SIM"))

#?roc
plot(ROC)

ROC$auc

#Resultado do modelo
confusionMatrix(Prev, teste$rating, dnn=c("Previsto", "Real"))


#####################################################################################################################
#####################################################################################################################
######################## RANDOM FOREST

modelo<- train(rating~., data=treino, method= "rf", ntree=100, trControl= trainControl(method = "cv", number = 5))
mean(modelo$resample$Accuracy)

library(rpart.plot)
prp(modelo, extra=1)


confusionMatrix(predict(modelo, teste), teste$rating)
confusionMatrix(predict(modelo, teste), teste$rating)$overall["Accuracy"]

#Prevendo dados no modelo de teste
Prev <- predict(modelo, teste)
#View(data.frame(teste$Diagnosis, Prev))

#ROC/ AUC
#Calculando Probabilidades de ser aceito no catalogo ou nao
PrevProb <- as.data.frame(predict(modelo, teste, type="prob"))

#Visualizando probabilidades
head(round(PrevProb, 2))

#Precisa-se armazenar apenas uma das colunas de vetores, neste caso as probabilidades dos belignos
PrevProb <- PrevProb$SIM

#install.packages("pROC")
library(pROC)

#Sensibilidade = 1
#TPR = sensitivity = TP/ (TP+FN)
#Especificidade = 1
#FPR= 1- Specificity = FP/ (FP+TN)

#Curva ROC
#Calculando os valores com base nos dados de teste em função da probabilidade
ROC <- roc(teste$rating ~ PrevProb, levels= c("NAO", "SIM"))

#?roc
plot(ROC)

ROC$auc

#Resultado do modelo
confusionMatrix(Prev, teste$rating, dnn=c("Previsto", "Real"))


RegressaoLogistica <- c(91.86)
ArvoreDeDecisao <- c(91.78)
FlorestaAleatoria <- c(91.86)
cbind(RegressaoLogistica, ArvoreDeDecisao, FlorestaAleatoria)

#chart <- as.data.frame(Modelo <- c("RegressaoLogistica","ArvoreDeDecisao","FlorestaAletoria"), Acur?cia <- c(91.86,91.78,91.86))
#chart

Modelos <- c("RegressaoLogistica","ArvoreDeDecisao","FlorestaAletoria")
Acur?cia <- c(91.86,91.78,91.86)
chart<- as.data.frame(cbind(Modelos, Acur?cia))
