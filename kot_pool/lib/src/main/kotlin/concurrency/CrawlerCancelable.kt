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

    override fun crawl(urls: List<String>): Response {
        var res = Response()
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

        val jobs = mutableListOf<Deferred<Int>>()
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
