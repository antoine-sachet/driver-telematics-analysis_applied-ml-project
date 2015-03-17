Driver Telematics Analysis
==========================

This is a Rstudio project containing scripts to perform predictive analysis for the Kaggle competition ["Driver Telematics Analysis"](http://www.kaggle.com/c/axa-driver-telematics-analysis).

The data should be in a folder named "driver", i.e. the first trip of the first driver should be in "path.to.folder/driver/1/1.csv".

Required packages
=================

The following R packages are required: plyr, foreach, doParallel (or doMC is using an unix system), e1071, quantmod, along with their dependencies obviously.

You should install any missing package with install.packages("package").

The challenge
=============

Source: https://www.kaggle.com/c/axa-driver-telematics-analysis/data.

You are provided a directory containing a number of folders. Each folder represents a driver. Within each folder are 200 .csv files. Each file represents a driving trip. The trips are recordings of the car's position (in meters) every second and look like the following:

x,y
0.0,0.0
18.6,-11.1
36.1,-21.9
...
In order to protect the privacy of the drivers' location, the trips were centered to start at the origin (0,0), randomly rotated, and short lengths of trip data were removed from the start/end of the trip.

A small and random number of false trips (trips that were not driven by the driver of interest) are planted in each driver's folder. These false trips are sourced from drivers not included in the competition data, in order to prevent similarity analysis between the included drivers. You are not given the number of false trips (it varies), nor a labeled training set of true positive trips. You can safely make the assumption that the majority of the trips in each folder do belong to the same driver.

The challenge of this competition is to identify trips which are not from the driver of interest, based on their telematic features. You must predict a probability that each trip was taken by the driver of interest.Algorithm

Implemented solution
====================

For each trip, a number of features (described below) are computed. Only these features are used in the script. From now on, we will refer to these features as the trip itself.

For each driver, we create a labeled dataset of trips he did take and didn't take using the following method:

* We make the assumption that the 200 trips of the driver were indeed taken by him and label them 1. These from the positive samples. 
Some of the trips (the one we want to find) were misclassified.

* We sample 200 (but this is adjustable) random trips from random other drivers and label 0. These are the negative sampàles, the trips our driver did not take.

For each driver, a SVM is trained on this labeled dataset. It is then used on the positive samples only, the hope being that it will pick up which of the trips were misclassified.

In practice, we reach a ROC score of more than 0.84.

The features
============

The features are computed by the function "extractFeatures(trip)" from feature_extraction.R. This is the function to modify to add/modify som features.
It is called in main.R via readFeatures(driver, trip).

These functions return, for a given trip, an array containing 38 double: 

1: length of trip (distance)
2-11: decile of speed (quantile of the speed distribution at 10%, 20%, ... 100%)
12-21: decile of positive (>=0) acceleration
22-31: decile of negative (<0) acceleration
32: duration of trip (time)
33-35: number of right turns in first third, second third and third third of the trip
36-38: number of left turns in in first third, second third and third third of the trip

Once computed, the features are available in the variable feature_data, a list of 2736 arrays of dimension 200x38.

Author
======

Antoine Sachet, 17/03/2015