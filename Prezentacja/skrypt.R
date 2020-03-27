library(missMDA)
library(FactoMineR)

# Metoda PCA dla zbioru z brakami
data("airquality")

# IMPUTATION
## Step 1. estimate the number of dimensions used in the reconstruction formula 
nb = estim_ncpPCA(airquality, ncp.max=5)
# Important arguments:
   # ncp.min
   # ncp.max
   # method
   # method.cv
   # pNA (for Kfold method.cv)
   # nbsim (for Kfold method.cv)

## Step 2. impute the data set with the impute.PCA function using the number 
##    of dimensions previously calculated
res.comp = imputePCA(airquality,ncp=2, method = 'Regularized', seed = 123)
# Important arguments:
    # ncp
    # method

# METHOD PERFORMANCE
## 3. perform the PCA on the completed data set using the PCA function 
## of the FactoMineR package
res.pca = PCA(res.comp$completeObs) 


# VISUALISING UNCERTAINITY DUE TO MISSING DATA
res.mipca <- MIPCA(airquality, ncp = 2, nboot = 100)
plot.MIPCA(res.mipca, choice = 'dim')
plot.MIPCA(res.mipca, 'var')

# Przydatny link:
## http://factominer.free.fr/course/missing.html
?imputePCA
