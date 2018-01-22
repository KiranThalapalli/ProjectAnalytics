# SUBMITTED BY DDA1710335, KIRAN T
#	Import the Bollywood data set in Rstudio in a variable named bollywood
  #After setting the working directory to import bollywood data we run the below read command
  bollywood <- read.csv("bollywood.csv")

#	When you import a data set, R stores character vectors as factors (by default)
# You can check the structure of the data frame by using str()
  str(bollywood)

# You can change the attribute 'Movie' from factor to character type using the given command
  bollywood$Movie <- as.character(bollywood$Movie)
  #To revert back the column as a factor we use the below command
  #bollywood$Movie <- as.factor(bollywood$Movie)
  
  

#Q1.
#	Access the last 10 movies (from the bottom of the Bollywood data frame) using column bollywood$Movie
# Store the names of those movies in last_10 vector (in the same order)
  
	last_10 <- tail(bollywood$Movie,n = 10L)
	#Tail command is used to fetch the data from the bottom of the data frame by mentioning additional argument n gives us the control of no. of records

#Q2.
#	Find out the total number of  missing values (NA) in the bollywood data frame.
# Store the result in na_bollywood vector
  
	na_bollywood <- length(which(is.na(bollywood)))
	  
#Q3
#	Write the command to find out which movie tops the list in terms of Total Collections
# Store the movie name in variable named top_movie

  top_movie <- bollywood[which.max(bollywood$Tcollection),grep("Movie",colnames(bollywood))]
  # Grep function can be used to get the column index dynamically
  # colnames function is used to fetch the names of the columns in the data frame
 
#Q4
#	Write the command to find out which movie comes second on the list in terms of Total Collections
# Store the movie name in variable named top_2_movie

  bollywood <- bollywood[order(-bollywood$Tcollection),]
  top_2_movie <- bollywood[2,grep("Movie",colnames(bollywood))]
  #Sorting the data in the descending order by Tcollection and hence the second record in the data frame could be considered as the second highest
  

# Now let's find out the movies shot by Shahrukh, Akshay and Amitabh separately.
# subset() function is used for that. The code has already been written for you. 
  
  shahrukh <- subset(bollywood, Lead == "Shahrukh")
  akshay <- subset(bollywood, Lead == "Akshay")
  amitabh <- subset(bollywood, Lead  == "Amitabh")

# You can view what the above data frames look like

#Q5
#	What is the total collection of Shahrukh, Akshay and Amitabh movies individually?
# You can use	a column named 'Tcollection' for this 
 
  shahrukh_collection <- sum(shahrukh$Tcollection)
  akshay_collection <- sum(akshay$Tcollection) 
  amitabh_collection <- sum(amitabh$Tcollection) 
  #Individual Total collections can be pulled from the subset we just created above
	
#Q6  
# Write command/s to find out how many movies are in Flop, Average, Hit and Superhit categories in the entire Bollywood data set.
  
  summary(bollywood$Verdict)
  # A summary function when applied on the verdict which is of factor type gives us the desired results by giving the count of each level
   
#You can use SAPPLY function if you want to apply a function specific columns in a data frame 
#You can write a command to find the maximum value of Ocollection, Wcollection, Fwcollecion and Tcollection using sapply
  
  maximum_value <- sapply(bollywood[,4:7],max,na.rm=TRUE)
  #na.rm is used to remove the NA values in the data frame
  
#Q7 
# Write a command to find the names of the movies which have the maximum Ocollection, Wcollection, Fwcollecion & Tcollection
# Store the names of 4 movies in same sequence in movie_result vector

  movie_result <- bollywood[sapply(bollywood[,4:7],which.max),grep("Movie",colnames(bollywood))]
  #Instead of harcoding column indexs grep function can be used to get the column index dynamically
  #The sapply in the row index gives the desired row which has the max collection in the columns 4,5,6,7 individually
  

   
    


    
    
    