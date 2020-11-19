package concurrency

import concurrency.domain.Request
import concurrency.domain.Response
import concurrency.utils.CrawlerUtils
import kotlinx.coroutines.coroutineScope
import kotlinx.coroutines.runBlocking
import kotlinx.coroutines.withTimeout
import org.slf4j.LoggerFactory
import java.util.concurrent.atomic.AtomicInteger

class CrawlerWithCoroutines(val maxDepth: Int, val timeout: Long) : Crawler {
    private val logger = LoggerFactory.getLogger(CrawlerWithCoroutines::class.java)
    val crawlerUtils = CrawlerUtils(maxDepth)

    // public method for crawling a list of urls using co-routines
    override fun crawl(urls: List<String>): Response {
        var res = Response()
        // Boundary for concurrency and it will not return until all
        // child URLs are crawled up to MAX_DEPTH limit.
        runBlocking {
            res.childURLs = crawl(urls, 0).childURLs
        }
        return res
    }

    suspend private fun crawl(urls: List<String>, depth: Int): Response {
        var res = Response()
        if (depth >= maxDepth) {
            return res.failed("Max depth reached")
        }
        var size = AtomicInteger()
        withTimeout(timeout) {
            for (u in urls) {
                coroutineScope {
                    val childURLs = crawlerUtils.handleCrawl(Request(u, depth))
                    // shared
                    size.addAndGet(crawl(childURLs, depth + 1).childURLs + 1)
                }
            }
        }
        return res.completed(size.get())
    }
}

