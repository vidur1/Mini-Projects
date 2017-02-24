# Used the rollingsales_manhattan.csv dataset
#   from https://raw.githubusercontent.com/ryanarbow/lessonprojects/master/rollingsales_manhattan.csv 

# Summary: Using the data from the website (listed above), the main goal was to clean the data.
#          Firstly, the dataset was imported, where the first few lines of the dataset had to be deleted as it was of no use.
#             Once done, a few steps were carried out to understand the dataset and its attributes.
#          Secondly, ensuring that the columns had the correct formats, eliminating the '$' signs and commas from columns to 
#             uniform the data were done. 
#          Another step was to ensure that the data was in their respective fields. For instance, it was found that the 
#             apartment number was not in its field but rather part of the house address which distorted the dataset. 
#             The correction had to be made and hence after cleaning the data, addresses with apartment numbers 
#             were in the Apartment field.
#          Fourthly, an error was found, where an apartment number was not in the correct format, which was corrected.
#          Lastly, since there were quite a few NAs in this dataset, if there were more than 5 NAs in a row, the row was removed
#             as having many NAs in many rows would result in skewing the dataset.


# Importing the file (with a few conditions)
df<- read.csv("https://raw.githubusercontent.com/ryanarbow/lessonprojects/master/rollingsales_manhattan.csv", header = T, 
              skip = 4, stringsAsFactors = FALSE, na.strings = c("NA",""), strip.white = TRUE)

# Checking out the data to understand the data imported better. The structure, head, tail and summary
# provide us insight.
str(df)
head(df)
tail(df)
summary(df)

# Making certain columns into factors as the columns imported were either characters or integers. 
# The following columns are converted into a factor (class) as these columns are level based. 
df$BOROUGH=factor(df$BOROUGH)
df$TAX.CLASS.AT.PRESENT=factor(df$TAX.CLASS.AT.PRESENT)
df$BUILDING.CLASS.AT.TIME.OF.SALE = factor(df$BUILDING.CLASS.AT.TIME.OF.SALE)

# Total number in the Ease.Ment column in the dataset
sum(!anyNA(df$EASE.MENT))

# As the whole column is NA in Ease.Ment (as mentioned above), it can be deleted. 
df<- df[,-7]

# Checking for unique values
unique(df$BOROUGH)

# As the 'Borough' column has the same values throughout (as mentioned above), it can be deleted.
df<- df[,-1]

# Check column names to reinforce what one is searching
# or to exmaine purpose of data.
names(df)

# Checking if there aren't NA values in the column Apartment.Number.
sum(!anyNA(df$APARTMENT.NUMBER))

# First checking to see the class of date and then changing the class to Date.
class(df$SALE.DATE)
df$SALE.DATE<- as.Date(df$SALE.DATE, format = "%m/%d/%y")

# Taking out the comma from columns that have it.
df$SALE.PRICE<- gsub(",","", df$SALE.PRICE, fixed = TRUE)
df$LAND.SQUARE.FEET<- gsub(",","", df$LAND.SQUARE.FEET, fixed = TRUE)
df$GROSS.SQUARE.FEET<- gsub(",","", df$GROSS.SQUARE.FEET, fixed = TRUE)

# Taking out $ on the Sales Price column.
df$SALE.PRICE<- gsub("$","", df$SALE.PRICE, fixed = TRUE)

# The following contains the fixing of the  address and apartment column as some of the apartment number were in 
# the address column. This also includes inserting all the apartment numbers in the Apartment.Number column. 
fixaddress<- strsplit(df$ADDRESS, ",")
fixaddress
ad1<- sapply(fixaddress, function(x){x[1]})
ad1<- trimws(ad1)
df$ADDRESS<- ad1
fixapartment<- sapply(fixaddress, function(x){x[2]})
fixapartment<- trimws(fixapartment)
df$APARTMENT.NUMBER<- trimws(df$APARTMENT.NUMBER)

fixapartment[which(is.na(fixapartment))] <- ""
df$APARTMENT.NUMBER[which(is.na(df$APARTMENT.NUMBER))] <- ""
df$APARTMENT.NUMBER <- paste(fixapartment, df$APARTMENT.NUMBER, sep="")
df$APARTMENT.NUMBER[which(df$APARTMENT.NUMBER=="")] <- NA

# Changing class of Sales Price column as the Sales.Price column should be numeric not character.
df$SALE.PRICE<- as.numeric(df$SALE.PRICE)

# Examining the data a little more. The following provide us with some insight in terms of the Sales Price in the
# Borough area. 
mean(df$SALE.PRICE)
median(df$SALE.PRICE)
sd(df$SALE.PRICE)
var(df$SALE.PRICE)
range(df$SALE.PRICE)

# Checking the length and dimsensions of the data se, after all the changes mentioned above.
length(df)
dim(df)

# An apartment number was originally not correctly inputted into the dataset. 
# It was in the scientific notation: 2.20E+03. The change has been made to apartment number 2200.
df[9962,9]<- 2200

# Delete columns if more than 5 variables(i.e.25% of the columns) are  NA. 
# If there are more than 5 variables as NA, it would be wise to delete them as it would be meaningless to have 
# a lot of rows with NA in our 'cleaned' data set.
MissingVal = rowSums(is.na(df))
RemoveNdx = MissingVal>5
dfcleaning = df[!RemoveNdx,]

# To export this dataset into a csv file.
write.csv(df, "R_project.csv")


