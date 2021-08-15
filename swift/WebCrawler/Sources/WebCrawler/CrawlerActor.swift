import Foundation

actor CrawlActor {
    public func handle(_ request: Request) async throws -> Int {
	let contents = try await download(request.url)
	let newContents = try await jsrender(request.url, contents)
  	if hasContentsChanged(request.url, newContents) && !isSpam(request.url, newContents) {
    	    try await index(request.url, newContents)
    	    let urls = try await parseURLs(request.url, newContents)
    	    let childURLs = try await doCrawlWithActors(urls: urls, depth: request.depth + 1, deadline: request.deadline)
    	    return childURLs + 1
  	} else {
    	    return 0
  	}
    }
}
