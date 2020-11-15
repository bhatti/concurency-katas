package concurrency

import org.junit.Test
import org.slf4j.LoggerFactory
import kotlin.test.assertEquals

class CrawlerAsynTest {
    private val logger = LoggerFactory.getLogger(CrawlerWithCoroutinesTest::class.java)
    val urls = listOf("a.com", "b.com", "c.com", "d.com", "e.com", "f.com",
            "g.com", "h.com", "i.com", "j.com", "k.com", "l.com", "n.com")

    @Test
    fun testCrawl() {
        val crawler = CrawlerWithAsync(4, 1000L)
        val started = System.currentTimeMillis()
        val res = crawler.crawl(urls);
        val duration = System.currentTimeMillis() - started
        logger.info("CrawlerAsync - crawled %d urls in %d milliseconds".format(res.childURLs, duration))
        assertEquals(19032, res.childURLs)
    }

    @Test(expected = Exception::class)
    fun testCrawlWithTimeout() {
        val crawler = CrawlerWithAsync(1000, 100L)
        crawler.crawl(urls);
    }
}
