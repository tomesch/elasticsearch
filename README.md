#elasticsearch.R

##Installation
You will need the devtools package in order to install this package
````R
install.packages("devtools")
library(devtools)
install_github("tomesch/elasticsearch")
library(elasticsearch)
````
##Examples
For the following series of examples we will be using a simplifed version of the Unicode Character Database. I formated the data so that it can be directly indexed in ElasticSearch with a bulk query. You can download the database [here](https://gist.github.com/Tomesch/dc7e565e81cf74e9d473/download).

###Importing the data
````R
# Read the database
requests = paste(readLines("/Users/tom/ucd.json"), collapse="\n")

# Create the client
es = ElasticSearchClient("http://localhost:9200")
# Index the data
bulk(es, requests)
````

###CRUD operations
````R
# Index a new document
index(es, "ucd", "characters", 1, body='{"cp": "9999", "na": "My new character", "age": "23", "blk": "ASCII"}')
# Get it
get(es, "ucd", "characters", 1)
# Update it
update(es, "ucd", "characters", 1, '{"doc" : { "age" : 24}}')
# Delete it
delete(es, "ucd", "characters", 1)
````

###Search
````R
# Search a character
search(es, body='{"query": {"match": {"na": "pile of poo"}}}')
# Run an aggregation
res = search(es, body='{"aggs" : {"blocks" : {"terms" : { "field" : "blk", "size": 0}}}}')
# And plot it
barplot(res$aggregations$blocks$buckets$doc_count)
````
