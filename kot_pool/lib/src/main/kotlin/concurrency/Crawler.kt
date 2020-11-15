package concurrency

import concurrency.domain.Response

interface Crawler {
    fun crawl(urls: List<String>): Response
}