package concurrency

import org.slf4j.LoggerFactory
import kotlin.test.Test
import kotlin.test.assertEquals

class CrawlerWithCoroutinesTest {
    private val logger = LoggerFactory.getLogger(CrawlerWithCoroutinesTest::class.java)
    val urls = listOf("a.com", "b.com", "c.com", "d.com", "e.com", "f.com",
            "g.com", "h.com", "i.com", "j.com", "k.com", "l.com", "n.com")

    @Test
    fun testCrawl() {
        val crawler = CrawlerWithCoroutines(4, 1000L)
        val started = System.currentTimeMillis()
        val res = crawler.crawl(urls);
        val duration = System.currentTimeMillis() - started
        logger.info("CrawlerWithCoroutines Crawled %d urls in %d milliseconds".format(res.childURLs, duration))
        assertEquals(19032, res.childURLs)
    }

    @Test(expected = Exception::class)
    fun testCrawlWithTimeout() {
        val crawler = CrawlerWithCoroutines(1000, 100L)
        crawler.crawl(urls);
    }
}
