How to run KCA and PZA

Do following step by step

1) Run LX-main code
2) Run KCA
   a) first variable is dataset, usually it is data_7Elements from main code
   b) max and min number of competitors in each zone. default are 10 and 3
   c) rad is the searching distance, default 0.8 miles
   d) distype: The way to calculate distance. Include 7 different ways 
          1) "distHaversine"
          2) "distCosine"
          3) "distGeo"
          4) "distMeeus"
          5) "distRhumb"
          6) "distVincentyEllipsoid" (This is really time costing)
          7) "distVincentySphere"
   e) KCA will output a data.frame and write a csv file
   f) I test various threshold such as max.num from 10 to 15 and rad from 0.5 to 10 miles. 
      The running time is always faster than old code.
      Parallel computing can speed up futhur. Detail see below.
   g) threshold: The amount of volumn change that need to be considered as key competitors


3) Run PZA
   a) first variable is dataset, usually it is the output of KCA
   b) When you have company information, you can append that into dataset and choose which brand to highlight
   c) Buffer size: default is 0. Usually between 0.1 and 0.5 mile
   d) Display a map or a shiny.app. Default F(map), could switch to T(app)

For more information, please read code comments and test sample.

Important:
## To use parallel computing, run code below. This require some hardware support. Computation time will reduce significently but may cause error.
# library(furrr) 
# plan(multiprocess)
# change all map function to future_map