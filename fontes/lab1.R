#install.packages("e1071")
#install.packages("mlr")
library("e1071")
library("mlr")
data("Titanic")
?naiveBayes
#Salva os dados em um dataframe
Titanic_df=as.data.frame(Titanic)
Titanic_df
#Criando dados a partir da tabela
repeating_sequence=rep.int(seq_len(nrow(Titanic_df)), Titanic_df$Freq) #Isto irá repetir cada combinação igual à frequência de cada combinação

#Criar o conjunto de dados por repetição de linha criada
Titanic_dataset=Titanic_df[repeating_sequence,]
#Remover a frequencia
Titanic_dataset$Freq=NULL

#Adaptando o modelo Naive Bayes
Naive_Bayes_Model=naiveBayes(Survived ~., data=Titanic_dataset)
#O que o modelo diz? Imprimir o resumo do modelo
Naive_Bayes_Model


#Previsão do conjunto de dados
NB_Predictions=predict(Naive_Bayes_Model,Titanic_dataset)
#Matriz de confusão para verificar a precisão (Acuracia)
table(NB_Predictions,Titanic_dataset$Survived)


#Crie uma tarefa de classificação para aprender no Dataset Titanic e especifique o recurso de destino
task = makeClassifTask(data = Titanic_dataset, target = "Survived")

#Incializa o classificador Naive Bayes
selected_model = makeLearner("classif.naiveBayes")

#Treina o modelo
NB_mlr = train(selected_model, task)

#Lendo o modelo de aprendizado
NB_mlr$learner.model

#Prever no conjunto de dados sem passar pelo recurso de destino
predictions_mlr = as.data.frame(predict(NB_mlr, newdata = Titanic_dataset[,1:3]))

##Matrix de confusão
table(predictions_mlr[,1],Titanic_dataset$Survived)
