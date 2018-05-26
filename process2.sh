#!/bin/bash
DATASETS='glass vehicle zoo vertebral forest dermatology segmentation svmguide4 satimage sports pendigits optdigits'
#satimage vertebral letters
#sports pendigits
#forest dermatology segmentation svmguide4
ESCENARIO='Escenario_1'

for DATASET in $DATASETS 
do

Rscript MainExperiments.R $ESCENARIO $DATASET C50 5 5
Rscript MainExperiments.R $ESCENARIO $DATASET C50 8 9

Rscript MainExperiments.R $ESCENARIO $DATASET RPART 5 5
Rscript MainExperiments.R $ESCENARIO $DATASET RPART 6 7

Rscript MainExperiments.R $ESCENARIO $DATASET SVM-EX 1 2
Rscript MainExperiments.R $ESCENARIO $DATASET SVM-EX 10 10

Rscript MainExperiments.R $ESCENARIO $DATASET RF-EX 3 4
Rscript MainExperiments.R $ESCENARIO $DATASET RF-EX 10 10

Rscript MainExperiments.R $ESCENARIO $DATASET knn 5 5
Rscript MainExperiments.R $ESCENARIO $DATASET knn 8 9

Rscript MainExperiments.R $ESCENARIO $DATASET nnet 1 2
Rscript MainExperiments.R $ESCENARIO $DATASET nnet 10 10

#Rscript MainExperiments.R $ESCENARIO $DATASET nb 1 2
#Rscript MainExperiments.R $ESCENARIO $DATASET nb 5 5

done
exit 0


