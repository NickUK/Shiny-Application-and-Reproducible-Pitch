Course Project: Shiny Application and Reproducible Pitch
========================================================
author: Nick Horne
date: 3rd August 2018
autosize: true



Introduction
========================================================

This presentation is for the Coursera Shiny Application and Reproducible Pitch assignement.

Source code for this project can be found here:
<https://github.com/NickUK/Shiny-Application-and-Reproducible-Pitch>

The following graph shows the survival for male and females of different classes.


```r
subset <- TitanicSurvival[TitanicSurvival$survived == "yes",]
    
ggplot(data=subset, aes(x=sex, fill=passengerClass)) + geom_bar() + labs(y="Survived", x="Gender", fill="Class")
```

![plot of chunk unnamed-chunk-2](pres-figure/unnamed-chunk-2-1.png)

Dataset
========================================================

I used the TitanicSurvival dataset from the `carData` library. 


```r
summary(TitanicSurvival)
```

```
 survived      sex           age          passengerClass
 no :809   female:466   Min.   : 0.1667   1st:323       
 yes:500   male  :843   1st Qu.:21.0000   2nd:277       
                        Median :28.0000   3rd:709       
                        Mean   :29.8811                 
                        3rd Qu.:39.0000                 
                        Max.   :80.0000                 
                        NA's   :263                     
```

```r
str(TitanicSurvival)
```

```
'data.frame':	1309 obs. of  4 variables:
 $ survived      : Factor w/ 2 levels "no","yes": 2 2 1 1 1 2 2 1 2 1 ...
 $ sex           : Factor w/ 2 levels "female","male": 1 2 1 2 1 2 1 2 1 2 ...
 $ age           : num  29 0.917 2 30 25 ...
 $ passengerClass: Factor w/ 3 levels "1st","2nd","3rd": 1 1 1 1 1 1 1 1 1 1 ...
```

Application
========================================================

The app I produced can be found here: 
<https://nickuk.shinyapps.io/Shiny-Application-and-Reproducible-Pitch/>

You can choose the following: 

1. gender
2. age 
3. class 

The web application will  predict your survival chance based on a simple linear model built on the dataset.

Summary
========================================================

The R presentations and shiny applications are both a great way of sharing your findings.

You can find out more about both here:

R Presentations: <https://support.rstudio.com/hc/en-us/articles/200486468-Authoring-R-Presentations>

Shiny: <https://shiny.rstudio.com/>
