package concurrency.domain

import java.util.*

data class Request(val url: String, val depth: Int, val createdAt: Date = Date()) {
}