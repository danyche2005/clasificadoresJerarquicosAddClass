#!/bin/bash
DATASETS='glass vehicle zoo vertebral forest dermatology segmentation svmguide4 satimage sports pendigits optdigits'
#satimage vertebral letters
#sports pendigits
#forest dermatology segmentation svmguide4
ESCENARIO='Escenario_1'

for DATASET in $DATASETS 
do

Rscript MainExperiments.R $ESCENARIO $DATASET C50 3 4
Rscript MainExperiments.R $ESCENARIO $DATASET C50 10 10

Rscript MainExperiments.R $ESCENARIO $DATASET RPART 3 4
Rscript MainExperiments.R $ESCENARIO $DATASET RPART 8 9

Rscript MainExperiments.R $ESCENARIO $DATASET SVM-EX 3 4
Rscript MainExperiments.R $ESCENARIO $DATASET SVM-EX 8 9

Rscript MainExperiments.R $ESCENARIO $DATASET RF-EX 1 2
Rscript MainExperiments.R $ESCENARIO $DATASET RF-EX 6 7

Rscript MainExperiments.R $ESCENARIO $DATASET knn 1 2
Rscript MainExperiments.R $ESCENARIO $DATASET knn 6 7

Rscript MainExperiments.R $ESCENARIO $DATASET nnet 8 9
Rscript MainExperiments.R $ESCENARIO $DATASET nnet 3 4

#Rscript MainExperiments.R $ESCENARIO $DATASET nb 10 10
#Rscript MainExperiments.R $ESCENARIO $DATASET nb 6 7

done
exit 0
