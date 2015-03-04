Driver Telematics Analysis
==========================

This is a Rstudio project containing scripts to perform predictive analysis for the Kaggle competition ("Driver Telematics Analysis")[http://www.kaggle.com/c/axa-driver-telematics-analysis].

The data should be in a folder named "driver", i.e. the first trip of the first driver should be in "path.to.folder/driver/1/1.csv".

You should install missing package with |install.packages("package")|.

Feature engineering ideas
=========================

* Find a way to match trips together. Most drivers take the same trips regularly.
The problem is that the trips are randomly rotated and a random portion of the trip has been removed at the beginning and at the end.
For now I will try to cluster them by approximate length and number of turns.