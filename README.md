# Masterthesis_BikeTrafficForecast

This GitHub repository contains all scripts written for the master thesis on the spatial prediction of urban cycling by Max Weinhold at the University of Münster submitted to Prof. Dr. Gernot Sieg at the Institute of Transport Science.

The author of all scripts is Max Weinhold. Supervisor of the thesis was Dr. Jan Wessel.

Data required for the preparation is not included.

The repository contains four folders:

1. data preparation: This folder contains all scripts that generate uniform data from the raw data. For each city that is later included in the data set, there is a script that combines the data of the respective city. The script Merge_City_Data.R connects all city data with each other. The script Dummy_Creator.R creates dummy variables from text variables later on.

2. MapProjection: This folder contains scripts for creating the graphical model projections. The scripts ProjectionSetGenerator.R and ProjectionSetGenerator2.R create the artificial data sets on which the model forecasts for the entire city area are based. The ProjectionMapGenerator.R script then uses this data set to make a graphical model prediction. The script UnknownCitySet.R creates the artificial data set for the city of Dresden.

3. thesis_german: This folder contains the latex file of the master thesis.

4. validationResults: This folder contains some tables where the validity of the different models are stored.

The folder above contains the scripts that were used to create the models. The abbreviations indicate the following:
-OLS Ordinary Least Square
-SVR Support Vector Regression
-RF Random Forest
-NN Neural Network

Other scripts include:

-Accident Correlation.R: Measures the spatial correlation of cycling accidents to spatially predict cycling through the model.

-Conditional_Validation_Set_Building.R: This script is important for evaluation. It creates 5 different cross validation subsets. It creates the subsets so that the data from each counting station appears in only one subset.

-DataVisualization.R and DataVisualization2.R: These are used for graphical processing of the data. The graphical output is saved directly in the Latex folder and overwritten.

-Dresden_Modell_Predection.R: Contains the predictions for Dresden, a city that does not occur in the training data itself and is only used to evaluate the model.

-Model_Size_Optimisation.R: This script is used to resample the data. It reduces the data set to one million observations, but selects the data in such a way that the individual counting stations are represented as evenly as possible.

-NonSpatialModellEvaluation.R: This script evaluates the model, but uses a conventional validation set approach that does not condition on the counting stations and therefore does not properly test the spatial validity. This validation is only used for comparison with other papers.