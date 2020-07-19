# Define function to extract crime data from Police API using defined date range and bounding box
crime_api = function(date,poly){ # date format e.g "2018-01", poly in 4326

# Extract bounding coordinates from poly
bbox = st_bbox(poly)
# Concat xmin,ymin,xmax,ymax 
ymin_xmin = str_c(bbox[2],bbox[1],sep=',')
ymin_xmax = str_c(bbox[2],bbox[3],sep=',')
ymax_xmin = str_c(bbox[4],bbox[1],sep=',')
ymax_xmax = str_c(bbox[4],bbox[3],sep=',')
# Combine items, which will be used to define the bounding coordinates of liverpool
bbox = str_c(ymin_xmin,ymax_xmin,ymax_xmax,ymin_xmax,sep=':')
  
# Define url
url = 'https://data.police.uk/api/crimes-street/all-crime'
# Define request
request = POST(url=url, query = list(poly=bbox, date=date))

if(request$status_code==200){
  # parse the response and convert to a data frame
  response = content(request, as = 'text', encoding = 'UTF-8') %>%
    fromJSON(flatten = TRUE) %>% # Parse from JSON
    as_tibble() %>% # Convert to tibble
    select(month, category, location.latitude, location.longitude) %>% # Subset columns
    rename_at(vars(contains('location')), ~str_remove(., 'location.')) %>% # Drop location.
    rename(lat=latitude, lng=longitude) # Rename columns
  
  } else {
stop('API call not Valid - check documentation')
}

}

mode <- function(codes){
which.max(tabulate(codes))
}
