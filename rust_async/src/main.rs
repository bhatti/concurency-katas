use std::time::Duration;
use futures::prelude::*;
use std::time::{Instant};
use crate::crawler::crawler::*;

mod crawler;

fn main() {
    let _ = do_crawl(8000);
}

fn do_crawl(timeout: u64) -> Result<usize, CrawlError> {
    let start = Instant::now();
    let urls = vec!["a.com", "b.com", "c.com", "d.com", "e.com", "f.com", "g.com", "h.com", "i.com", "j.com", "k.com", "l.com", "n.com"].into_iter().map(|s| s.to_string()).collect();
    let res = crawl(urls, Duration::from_millis(timeout));
    let duration = start.elapsed();
    println!("Crawled {:?} urls in () is: {:?}", res, duration);
    res
}

#[cfg(test)]
mod tests {
    use super::do_crawl;
    #[test]
    fn crawl_urls() {
        match do_crawl(8000) {
            Ok(size) => assert_eq!(size, 19032),
            Err(err) => assert!(false, format!("Unexpected error {:?}", err)),
        }
    }
}
