package concurrency

import org.junit.Test
import org.slf4j.LoggerFactory
import kotlin.test.assertEquals

class CrawlerCancelableTest {
    private val logger = LoggerFactory.getLogger(CrawlerWithCoroutinesTest::class.java)
    val urls = listOf("a.com", "b.com", "c.com", "d.com", "e.com", "f.com",
            "g.com", "h.com", "i.com", "j.com", "k.com", "l.com", "n.com")

    @Test
    fun testCrawlWithCancel() {
        val crawler = CrawlerCancelable(4, 1000L)
        val res = crawler.crawl(urls);
        assertEquals(0, res.childURLs)
    }
}
