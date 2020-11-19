package concurrency

import concurrency.domain.Request
import concurrency.domain.Response
import concurrency.utils.CrawlerUtils
import kotlinx.coroutines.Deferred
import kotlinx.coroutines.async
import kotlinx.coroutines.runBlocking
import kotlinx.coroutines.withTimeout
import org.slf4j.LoggerFactory
import java.util.concurrent.atomic.AtomicInteger

class CrawlerCancelable(val maxDepth: Int, val timeout: Long) : Crawler {
    private val logger = LoggerFactory.getLogger(CrawlerWithCoroutines::class.java)
    val crawlerUtils = CrawlerUtils(maxDepth)

    // public method for crawling a list of urls to show cancel operation
    // internal method will call cancel instead of await so this method will
    // fail.
    override fun crawl(urls: List<String>): Response {
        var res = Response()
        // Boundary for concurrency and it will not return until all
        // child URLs are crawled up to MAX_DEPTH limit.
        runBlocking {
            res.childURLs = crawl(urls, 0).childURLs
        }
        return res
    }

    ////////////////// Internal methods
    suspend private fun crawl(urls: List<String>, depth: Int): Response {
        var res = Response()
        if (depth >= maxDepth) {
            return res.failed("Max depth reached")
        }
        var size = AtomicInteger()

        withTimeout(timeout) {
            val jobs = mutableListOf<Deferred<Int>>()
            for (u in urls) {
                jobs.add(async {
                    val childURLs = crawlerUtils.handleCrawl(Request(u, depth))
                    // shared
                    size.addAndGet(crawl(childURLs, depth + 1).childURLs + 1)
                })
            }
            for (j in jobs) {
                j.cancel()
            }
        }
        return res.completed(size.get())
    }
}
