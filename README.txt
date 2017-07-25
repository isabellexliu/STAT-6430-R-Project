
Here are brief descriptions of the movie review data.

moviereviews.txt     
	   -- The full data set, 100000 ratings by 943 reviewers on 1682 movies.
              Each user has rated at least 20 movies.  Reviewers and movies are
              numbered consecutively from 1.  The data is randomly
              ordered. This is a list of the form
	          reviewer id | movie id | rating | timestamp. 
              The time stamps are unix seconds since 1/1/1970 UTC   

movieinfo.txt
           -- Information about the movies; this is a list of the form
              
              movie id | movie title | release date | video release date |
              IMDb URL | unknown | Action | Adventure | Animation |
              Children's | Comedy | Crime | Documentary | Drama | Fantasy |
              Film-Noir | Horror | Musical | Mystery | Romance | Sci-Fi |
              Thriller | War | Western |
              
	      The last 19 fields are the genres, a 1 indicates the movie
              is of that genre, a 0 indicates it is not; movies can be in
              several genres at once.
              The movie ids are the same as in the moviereviews.txt data set.

reviewers.txt
           -- Demographic information about the users; this is a list
              of the form
              user id | age | gender | occupation | zip code
              The reviewer ids are those used in the movie reviews.txt data set.
              Zip codes that include letters are from Canada.  Zip codes
              that are unrecognized should be classified Unknown.
              
zipcodes.csv
	    -- A comma-delimited text file listing information about every US zip
	       code.  The columns are Zipcode | ZipCodeType | City |
	       State | LocationType | Lat | Long | Location | Decommisioned
		   	  
		Zipcode: 5 digit Zipcode or military postal code(FPO/APO)
		ZipCodeType: Standard, PO BOX Only, Unique, Military(implies APO or FPO)
		LocationType:	Primary, Acceptable, Not Acceptable
		Lat: Latitude, if available
		Long: Longitude, if available
		Location: Standard Display (eg Phoenix, AZ ; Pago Pago, AS ; Melbourne, AU )
   		Decommisioned: If Primary location, Yes implies historical Zipcode, No 
   			  	Implies current Zipcode; If not Primary, Yes implies Historical Placename

