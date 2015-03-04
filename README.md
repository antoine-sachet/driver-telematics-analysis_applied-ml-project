Driver Telematics Analysis
==========================

This is a Rstudio project containing scripts to perform predictive analysis for the Kaggle competition ("Driver Telematics Analysis")[http://www.kaggle.com/c/axa-driver-telematics-analysis].

The data should be in a folder named "driver", i.e. the first trip of the first driver should be in "path.to.folder/driver/1/1.csv".

You should install missing package with |install.packages("package")|.

The feature extraction takes a little 10 minutes on my computer. It is very challenging for the disk so be careful (lots of simultaneous access).

Training and applying the model takes a little more than 6 minutes on my computer.  

Ideas
=========================

* Find a way to match trips together. Most drivers take the same trips regularly.
The problem is that the trips are randomly rotated and a random portion of the trip has been removed at the beginning and at the end.
For now I will try to cluster them by approximate length and number of turns.

* If we want to use validation instead of submitting a lot, we can add some false trips as positive samples

* I am having a look at thi sR package that does some GPS trajectory analysis
http://cran.r-project.org/web/packages/adehabitatLT/vignettes/adehabitatLT.pdf

* I am investigating the use of a fourier transform of the speed (my signal processing background finally pays off :p)

