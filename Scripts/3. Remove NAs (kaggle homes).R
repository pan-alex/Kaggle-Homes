# Dependencies:
# 2. Read (kaggle homes).R -- For the data
# 1. Functions (kaggle homes).R -- For the functions

require(dplyr)
require(ggplot2)
require(Amelia)
require(mice)
require(lattice)

# Define a new variable for subsequent manipulation (so that the original data sets are left unchanged)
full.data2 <- full.data


## 1. Dealing with NAs ####

### 1.1. Correct variable classes ####

str(full.data2)
table(sapply(full.data2, class))

# Some variables were misclassified.
full.data2 <- mutate(full.data2,
                     MSSubClass = as.factor(MSSubClass),
                     MoSold = as.factor(MoSold)
                     )


### 1.2. Check for NAs ####

# View missing values for all of the variables; plot 20 at a time for overview.
Amelia::missmap(full.data2)


# Call Function 2: show.na() to determine the proportion of NA values in each variable.
show.na(full.data2)

### 1.3. Replace NAs ####

####1.3.1. Replace NAs with "None" ----


# For the following variables, NA values simply mean "None" rather than "missing". There are some other variables that this will apply to, but they are numeric - we will have to deal with them differently.


# Store these variables in the temporary variable temp.
temp <- c("Alley","BsmtQual","BsmtCond","BsmtExposure","BsmtFinType1",
          "BsmtFinType2","FireplaceQu","GarageType","GarageFinish",
          "GarageQual","GarageCond","PoolQC","Fence","MiscFeature"
          )

# temp2 is a temporary storage variable
temp2 <- data.frame( matrix(9999,
                           ncol = length(full.data2),
                           nrow = length(full.data2[[1]]))
                    )
names(temp2) <- names(full.data2)

# Call Function 3: impute.na(), which replaces NAs in variables with the factor level "None".
# The loop runs impute.na() on full.data2 for every variable stored in temp.

for (i in seq_along(full.data2[temp])) {
  temp2[temp[i]] <- impute.na(full.data2[temp], i)
}
# Replace variables containing NA with those containing "None"
full.data2[temp] <- temp2[temp]



#### 1.3.2. Impute NAs using models: ----

# View remaining missing values:
Amelia::missmap(full.data2)
show.na(full.data2)


##### 1.3.2.1. Pick off some oddities:

  ## There is 1 NA for GarageCars and GarageArea; this should be 0, since other homes without a garage have this as 0 instead of NA.
full.data2 <- mutate(full.data2,
                     GarageCars = ifelse(is.na(GarageCars), 0, GarageCars),
                     GarageArea = ifelse(is.na(GarageArea), 0, GarageArea),
                     GarageYrBlt = ifelse(GarageYrBlt == 2207, 2007, GarageYrBlt)
)


mice.data2 <- mice(full.data2, m = 1, maxit = 20, method = "cart")
  ## This step will take a while. Reduce maxit to reduce run-time.

# full.data3 is full.data2 with all of NAs imputed by mice()
full.data3 <- complete(mice.data2)

# Set SalePrice for test set back to NA.
full.data3 <- mutate(full.data3, SalePrice = ifelse(Dataset == "test", NA, SalePrice))


write.csv(full.data3, "../Data/kaggle.homes-na imputed.csv", row.names = FALSE)

