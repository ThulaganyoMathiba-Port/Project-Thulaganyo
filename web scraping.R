#We begin to web scraping :
#we are extarcting text, table etc form a website, we save it as a csv file etc. 
#there are no api's , some do have api's 
#we need rverse, dplyr,
#letl us install the packages 
install.packages("rvest")
#once you click on inspect, you go the top left of this code "select an element.."- this gives trhe specific part you are looking at 
#always copy the website itself 
#now we call rvest 
library(rvest)
#now the URL(LINK OF THE WEBSITE) assign the website to a name so we can access the url
url <- https:/www.scrapethissite.com/pages/simple/ #this is from the website we want 
  #now we want to save our web page from read html 
webpage <- read_html(url) #add the website (this is linking )
#we get the data from webpage
#now we call html nodes(portions we want to extract fro the website)
web_data <- webpage %>%
  html_nodes(".country")#this is just the name 

#we need to get a dataframe where our results will be stored 
results <- data.frame(
  Country =character(), #we use the class name from the website as it is!!! from the website
  #keep in mind that we are also converting these classes into characters 
  Capital = character(),
  Population =  numeric(), 
  Area = numeric()
)
#we need to create a false statement to call the data 
for (country in web_data) {
  #we begin which one-where we gettig the data from?
  country_name <- country %>% 
    #from wher? html node 
    html_node(".country-name") %>% #inside we write the country class as it is from the website!!
     html_text(trim = TRUE) #NOW WE ARE DONE WITH COUNTRY NOW WE MOVE, same syntax for the rest 
  
  
  capital_name <- country %>%
    html_node(".country-capital") %>%#please mize the h3, p3 we just said ".name"
    html_text(trim = TRUE)
  
  population_data <- country %>%
    html_node(".country_population") %>%
    html_text(trim= TRUE)
    as.numeric() #as we are dealing with numeric
    
  area_size <- country %>% 
    html_node(".country-area")%>%
    html_text(trim= TRUE)
    as.numeric() #we are dealing with numeric, so call it as a numric 
#now we add the data extracted by the for-loop into a dataframe
results <- rbind(results, data.frame(
  Country = country_name, #assign the country to country name(these are the names assigned from collected data)
  Capital = capital_name,
  Population = population_data,
  Area =area_data #we assign names to the assigned data extracted from website
  #we we want to show data
))    
}

#above we have our 4 elements that we want to extract from website 
#we used a for-loop to fetch the elements
#since we created the dataframe, we want to use rbind into the dataframe (add that before the end of teh } of the loop)
print(results)



library(rvest)

# Load website
url <- "https://www.scrapethissite.com/pages/simple/"
webpage <- read_html(url)

# Extract country data
web_data <- webpage %>%
  html_nodes(".country")

# Create empty dataframe
results <- data.frame(
  Country = character(),
  Capital = character(),
  Population = numeric(),
  Area = numeric()
)

# Loop through each country
for (country in web_data) {
  
  country_name <- country %>% 
    html_node(".country-name") %>% 
    html_text(trim = TRUE)
  
  capital_name <- country %>%
    html_node(".country-capital") %>%
    html_text(trim = TRUE)
  
  population_data <- country %>%
    html_node(".country-population") %>%
    html_text(trim = TRUE) %>%
    as.numeric()
  
  area_size <- country %>% 
    html_node(".country-area") %>%
    html_text(trim = TRUE) %>%
    as.numeric()
  
  # Add to dataframe
  results <- rbind(results, data.frame(
    Country = country_name,
    Capital = capital_name,
    Population = population_data,
    Area = area_size
  ))
}

# Display results
print(results)
write.csv(results, "countries_data.csv", row.names = FALSE)
