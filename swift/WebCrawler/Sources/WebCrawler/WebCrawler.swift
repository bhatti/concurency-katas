import Foundation

struct Request {
    let url: String
    let depth: Int
    let deadline: DispatchTime
}

enum CrawlError: Error {
    case timeoutError(String)
}

let MAX_DEPTH = 4
let MAX_URLS = 11
let DOMAINS = [
  "ab.com",
  "bc.com",
  "cd.com",
  "de.com",
  "ef.com",
  "fg.com",
  "gh.com",
  "hi.com",
  "ij.com",
  "jk.com",
  "kl.com",
  "lm.com",
  "mn.com",
  "no.com",
  "op.com",
  "pq.com",
  "qr.com",
  "rs.com",
  "st.com",
  "tu.com",
  "uv.com",
  "vw.com",
  "wx.com",
  "xy.com",
  "yz.com",
];

public func crawl(urls: [String], deadline: DispatchTime) async throws -> Int {
    // Main scope of concurrency begin
    // TODO add timeout using race, e.g. await Task.WhenAny(crawlTask, Task.Delay(deadline)) == crawlTask
    return try await doCrawl(urls: urls, depth: 0, deadline: deadline)
    // Main scope of concurrency end
}

public func crawlWithActors(urls: [String], deadline: DispatchTime) async throws -> Int {
    // Main scope of concurrency begin
    // TODO add timeout using race, e.g. await Task.WhenAny(crawlTask, Task.Delay(deadline)) == crawlTask
    return try await doCrawlWithActors(urls: urls, depth: 0, deadline: deadline)
    // Main scope of concurrency end
}


///////////////// PRIVATE METHODS ////////////////
func doCrawl(urls: [String], depth: Int, deadline: DispatchTime) async throws -> Int {
    if depth >= MAX_DEPTH {
	return 0
    }
    let requests = urls.map { Request(url: $0, depth: depth, deadline: deadline) }

    var totalChildURLs = 0
    try await withThrowingTaskGroup(of: (Request, Int).self) { group in
        for req in requests {
	    group.addTask(priority: .background) {
	        return (req, try await handleRequest(req))
	    }
        }
        for try await (req, childURLs) in group {
	    if totalChildURLs % 10 == 0 {
		print("received request \(req)")
	    }
	    totalChildURLs += childURLs
        }
    }
    return totalChildURLs
}

func doCrawlWithActors(urls: [String], depth: Int, deadline: DispatchTime) async throws -> Int {
    if depth >= MAX_DEPTH {
	return 0
    }
    let requests = urls.map { Request(url: $0, depth: depth, deadline: deadline) }

    var totalChildURLs = 0
    let crawler = CrawlActor() 
    for req in requests {
     	let childURLs = try await crawler.handle(req)
	totalChildURLs += childURLs
    }
    return totalChildURLs
}

func handleRequest(_ request: Request) async throws -> Int {
    let contents = try await download(request.url)
    let newContents = try await jsrender(request.url, contents)
    if hasContentsChanged(request.url, newContents) && !isSpam(request.url, newContents) {
        try await index(request.url, newContents)
        let urls = try await parseURLs(request.url, newContents)
        let childURLs = try await doCrawl(urls: urls, depth: request.depth + 1, deadline: request.deadline)
        return childURLs + 1
    } else {
        return 0
    }
}

func download(_ url: String) async throws -> String {
    // TODO check robots.txt and throttle policies
    // TODO add timeout for slow websites and linearize requests to the same domain to prevent denial of service attack
    return randomString(100)
}

func jsrender(_ url: String, _ contents: String) async throws -> String {
    // for SPA apps that use javascript for rendering contents
    return contents
}

func index(_ url: String, _ contents: String) async throws {
    // apply standardize, stem, ngram, etc for indexing
}

func parseURLs(_ url: String, _ contents: String) async throws -> [String] {
    // tokenize contents and extract href/image/script urls
    var urls = [String]()
    for _ in 0..<MAX_URLS {
        urls.append(randomUrl())
    }
    return urls
}

func hasContentsChanged(_ url: String, _ contents: String) -> Bool {
    return true
}

func isSpam(_ url: String, _ contents: String) -> Bool {
    return false
}

func randomUrl() -> String {
    let number = Int.random(in: 0..<WebCrawler.DOMAINS.count)
    return "https://" + WebCrawler.DOMAINS[number] + "/" + randomString(20)
}

func randomString(_ length: Int) -> String {
  let letters = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
  return String((0..<length).map{ _ in letters.randomElement()! })
}
