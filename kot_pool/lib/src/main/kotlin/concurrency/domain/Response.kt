package concurrency.domain

import java.util.*

data class Response(var status: State=State.STARTED, var childURLs: Int=0, val startedAt: Date=Date(), var completedAt: Date?=null, var error: String?=null) {
    fun completed(size: Int): Response {
        childURLs = size
        status = State.COMPLETED
        completedAt = Date()
        return this
    }
    fun failed(err: String): Response {
        error = err
        status = State.FAILED
        completedAt = Date()
        return this
    }
}
