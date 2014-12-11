#elasticsearch

Simple Elasticsearch client for R

##Features (WIP !!!)
###Documents API
- connect
- count
- create
- delete
- exists
- get
- getSource
- index
- update

###Search API
- search

###Indices API
- indices.close
- indices.create
- indices.delete
- indices.exists
- indices.get
- indices.open
- indices.optimize
- indices.refresh

##Installation
You will need the devtools package in order to install this package
```R
install.packages("devtools")
library(devtools)
install_github("tomesch/elasticsearch")
```

##Examples
```R
connect("http://localhost:9000/")
#> $status
#> [1] 200
#> $name
#> [1] "Meltdown"
#> $version
#> $version$number
#> [1] "1.3.2"
#> $version$build_hash
#> [1] "dee175dbe2f254f3f26992f5d7591939aaefd12f"
#> $version$build_timestamp
#> [1] "2014-08-13T14:29:30Z"
#> $version$build_snapshot
#> [1] FALSE
#> $version$lucene_version
#> [1] "4.9"
#> $tagline
#> [1] "You Know, for Search"
```

###Create a new index
```R
indices.create("testindex")
#> $acknowledged
#> [1] TRUE

indices.exists("testindex")
#> [1] TRUE
```

###Index a new document
```R
index("testindex", "testtype", document='{"key": "value", "key_1": "value_1"}')
#> $`_index`
#> [1] "testindex"
#> $`_type`
#> [1] "testtype"
#> $`_id`
#> [1] "EpTgKbBnRKa5CV06-j6_IA"
#> $`_version`
#> [1] 1
#> $created
#> [1] TRUE
```

###Search for documents
```R
search("testindex", "testtype", query='{"query": {"term": {"key": "value"}}}')
#> $took
#> [1] 1
#> $timed_out
#> [1] FALSE
#> $`_shards`
#> $`_shards`$total
#> [1] 5
#> $`_shards`$successful
#> [1] 5
#> $`_shards`$failed
#> [1] 0
#> $hits
#> $hits$total
#> [1] 1
#> $hits$max_score
#> [1] 0.3068528
#> $hits$hits
#>      _index    _type                    _id    _score _source.key _source.key_1
#> 1 testindex testtype EpTgKbBnRKa5CV06-j6_IA 0.3068528       value       value_1
```
