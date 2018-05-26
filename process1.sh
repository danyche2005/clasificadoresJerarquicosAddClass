#!/bin/bash
DATASETS='glass vehicle zoo vertebral forest dermatology segmentation svmguide4 satimage sports pendigits optdigits'
#satimage vertebral letters
#sports pendigits
#forest dermatology segmentation svmguide4
ESCENARIO='Escenario_1'

for DATASET in $DATASETS 
do
     
Rscript MainExperimentsBase.R $ESCENARIO $DATASET C50 1 2
Rscript MainExperimentsBase.R $ESCENARIO $DATASET C50 6 7

Rscript MainExperimentsBase.R $ESCENARIO $DATASET RPART 1 2
Rscript MainExperimentsBase.R $ESCENARIO $DATASET RPART 10 10

Rscript MainExperimentsBase.R $ESCENARIO $DATASET SVM-EX 5 5
Rscript MainExperimentsBase.R $ESCENARIO $DATASET SVM-EX 6 7

Rscript MainExperimentsBase.R $ESCENARIO $DATASET RF-EX 5 5
Rscript MainExperimentsBase.R $ESCENARIO $DATASET RF-EX 8 9

Rscript MainExperimentsBase.R $ESCENARIO $DATASET knn 3 4
Rscript MainExperimentsBase.R $ESCENARIO $DATASET knn 10 10

Rscript MainExperimentsBase.R $ESCENARIO $DATASET nnet 5 5
Rscript MainExperimentsBase.R $ESCENARIO $DATASET nnet 6 7

#Rscript MainExperimentsBase.R $ESCENARIO $DATASET nb 3 4
#Rscript MainExperimentsBase.R $ESCENARIO $DATASET nb 8 9

done





