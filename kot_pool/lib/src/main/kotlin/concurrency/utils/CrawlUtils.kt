package concurrency.utils

import concurrency.domain.Request
import kotlinx.coroutines.runBlocking

class CrawlerUtils(val maxDepth: Int) {
    val DOMAINS = listOf("ab.com", "bc.com", "cd.com", "de.com", "ef.com", "fg.com",
            "gh.com", "hi.com", "ij.com", "jk.com", "kl.com", "lm.com", "mn.com",
            "no.com", "op.com", "pq.com", "qr.com", "rs.com", "st.com", "tu.cocm",
            "uv.com", "vw.com", "wx.com", "xy.com", "yz.com")
    val MAX_URLS = 11

    // method to crawl a single url
    fun handleCrawl(req: Request): List<String> {
        return runBlocking {
            val contents = download(req.url)
            if (req.depth + 1 < maxDepth &&
                    hasContentsChanged(req.url, contents) &&
                    !isSpam(req.url, contents)) {
                index(req.url, contents)
                return@runBlocking parseURLs(req.url, contents)
            } else {
                return@runBlocking listOf()
            }
        }
    }

    suspend private fun download(url: String): String {
        // TODO check robots.txt and throttle policies
        // TODO add timeout for slow websites and linearize requests to the same domain to prevent denial of service attack
        // invoke jsrender to generate dynamic content
        return jsrender(url, randomString(100))
    }

    suspend private fun jsrender(_url: String, contents: String): String {
        // for SPA apps that use javascript for rendering contents
        return contents
    }

    suspend private fun index(_url: String, _contents: String) {
        // apply standardize, stem, ngram, etc for indexing
    }

    private fun parseURLs(_url: String, _contents: String): List<String> {
        // tokenize contents and extract href/image/script urls
        return (0..MAX_URLS - 1).map { i -> randomURL(i) }
    }

    private fun hasContentsChanged(_url: String, _contents: String): Boolean {
        return true
    }

    private fun isSpam(_url: String, _contents: String): Boolean {
        return false
    }

    private fun randomString(max: Int): String {
        val charPool: List<Char> = ('a'..'z') + ('A'..'Z') + ('0'..'9')
        return (1..max).map { i -> kotlin.random.Random.nextInt(0, charPool.size) }.map(charPool::get).joinToString("")
    }

    private fun randomURL(i: Int): String {
        val i = (0..DOMAINS.size - 1).random()
        val domain = DOMAINS[i]
        return "https://%s/%s_%d".format(domain, randomString(20), i)
    }
}
