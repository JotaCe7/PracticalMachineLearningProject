training = read.csv("training.csv",stringsAsFactors=TRUE)
testing = read.csv("testing.csv")

no.na_names <- names(testing[,!sapply(testing,function(x) all(is.na(x)))])
new_training <-  training[ ,c(no.na_names[-length(no.na_names)] ,"classe")]
new_testing <-  testing[ ,no.na_names[-length(no.na_names) ] ]

new_training <- new_training[,c(-1,-3,-4,-5,-6,-7)]
new_testing  <- new_testing[,c(-1,-3,-4,-5,-6,-7)]

levels(new_training[,1]) = c(1,2,3,4,5,6)
levels(new_testing[,1]) = c(1,2,3,4,5,6)
new_training[1] <- as.numeric(as.character(new_training[,1]))
new_testing[1] <- as.numeric(as.character(new_testing[,1]))

set.seed(915)
inTrain <- createDataPartition(new_training$classe,p=0.7,list=FALSE)
new_sub_training <- new_training[inTrain,]
new_sub_testing  <- new_training[-inTrain,]

M <- abs(cor(new_sub_training[,-length(new_sub_training)]))
diag(M) <- 0
#which(M>0.9,arr.ind=T)

pca <- prcomp(new_sub_training[,-length(new_sub_training)])
preProc <- preProcess(new_sub_training[,-length(new_sub_training)],method="pca",pcaComp=14)


sub_training_pca <- predict(preProc,new_sub_training[,-length(new_sub_training)])
sub_training_pca$classe <- new_sub_training$classe

modFit <- train(classe~.,data=sub_training_pca,method="rf")

sub_testing_pca  <- predict(preProc,new_sub_testing[,-length(new_sub_testing)])
sub_testing_pca$classe <- new_sub_testing$classe

prediction_sub_testing_pca <- predict(modFit,sub_testing_pca)
confusionMatrix(sub_testing_pca$classe, prediction_sub_testing_pca)

prediction









