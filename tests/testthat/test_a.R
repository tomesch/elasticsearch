es = ElasticSearchClient("http://localhost:9200")

if (indices.exists(es, "test_r_elasticsearch")$exists) {
  indices.delete(es, "test_r_elasticsearch")
}
