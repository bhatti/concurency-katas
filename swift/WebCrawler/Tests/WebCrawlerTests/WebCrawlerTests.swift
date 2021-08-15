import XCTest
@testable import WebCrawler

final class WebCrawlerTests: XCTestCase {
    func testCrawl() throws {
	let expectation = self.expectation(description: "Waiting for crawl via Async/Await.")
	Task {
	    let result = try await crawl(urls: DOMAINS, deadline: .now() + 100)
            XCTAssertEqual(36600, result)
	    expectation.fulfill()
	}
	waitForExpectations(timeout: 120)
    }

    func testCrawlWithActors() throws {
	let expectation = self.expectation(description: "Waiting for crawl via Actors and Async/Await.")
	Task {
	    let result = try await crawlWithActors(urls: DOMAINS, deadline: .now() + 100)
            XCTAssertEqual(36600, result)
	    expectation.fulfill()
	}
	waitForExpectations(timeout: 120)
    }
}
