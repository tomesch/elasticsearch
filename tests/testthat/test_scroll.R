context("scroll API")

es = ElasticsearchClient("http://localhost:9200")
index(es, "test_r_elasticsearch", "test_scroll", body = '{"test": "testing"}')

