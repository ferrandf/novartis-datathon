# novartis-datathon

what we first did is to transform the Date columns as Dates (they were strings) and the indication column as list.

next we found that the maximum price per unit that we have was 535 dollars, only one drug called DRUG_ID_4C4E (11 appearances). As it was a really noticeable outlier we consider that the best was to remove it so it did not add noise to the training phase.

we created a collumn called "Group_Country", it has 4 groups that have been clustered grouping by country and computing the average price (Price_Unit) of each of them. The idea behind this is to group the countries with similar prices

we created a column "Group_Price", it has 4 groups based on the column "Price_Unit". The idea is to group the rows with similar prices, targeting indirectly similar indications, drugs and countries among others.

we created one column based on "Indications" which is basically the length of the array in this column. We believe that the higher number of indications it has, the better price a drug will have.
