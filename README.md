# Random Regression Analysis

__Maintainer__: *Rostam Abdollahi-Arpanahi*

**Date**:  May 2, 2020

---

Here you can see an example file for running Random Regression Analysis using BLUPf90 family programs, generating Legendre Polynomials coefficients and computing the breeding values based on Test-day milk data in dairy cattle.
Analysis includes three steps as following:

## 1. Download the materials from github repository

```
git clone https://github.com/Rostamabd/Random-Regression-Analysis.git
```

## 2. Run renumf90

Download blupf90 family programs from here:  [BLUPF90](http://nce.ads.uga.edu/~ignacy/newprograms.html)


```
echo ParRenum.par | ./renumf90 | tee ren.log
```

## 3. Run blupf90

The renumf90.par file generated by renumf90 has been modified to meet the RR analysis requirements. Probably, the recent release of renumf90, there are some options to create the RR parameter file by renumf90. Blupf90 wiki page is here: [BLUPf90 programs help](http://nce.ads.uga.edu/wiki/doku.php?id=application_programs)

Download [BLUPF90 family manual](http://nce.ads.uga.edu/wiki/lib/exe/fetch.php?media=blupf90_all7.pdf).

```
echo renf90.par | ./blupf90 | tee blup.log
```

It should be noticed that this is a toy example and the starting values for (co)variance components are arbitrary. In your analysis, I would suggest you run a program such as gibbs1f90, airemlf90 or thrgibbs1f90  for estimating the (co)variances. 

## 4. Post Random Regression Analysis

The estimates of random effects in the solution file generated by blupf90 program are the random regression coefficients. For instance, if the degree of Legendre polynomials is 2, we see three estimates per each individual in the output file. Basically, these three values are the coefficients of RR for intercept (b0), b1 and b2. So, if you are interested in prediction of breeding values, the RR coefficients must convert to breeding values. Interestingly, we can predict the breeding values for each time point or the entire time period based on these values. In the example file, we fitted Legendre polynomials with degrees 2 and 2 for permanent environmental and additive animal effects, respectively.  

Well, now its time to run R script for estimating the breeding values. 

The main Rscript is Post_RR_ANAL.R but it has dependency to legendre_Coeff.R script for deriving the Legendre polynomials function. By default the program computes 6 degree of Legendre polynomials function. 

```
source("Post_RR_ANAL.R")
```

The estimated breeding values are stored in all_RR_BV.txt file. 



## Contact information

Please send your comments and suggestions to rostam7474 at gmail dot com



