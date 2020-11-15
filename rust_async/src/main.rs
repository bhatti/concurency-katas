use std::time::Duration;
use futures::prelude::*;
use std::time::{Instant};
use crate::crawler::crawler::*;

mod crawler;

fn main() {
    do_crawl();
}

fn do_crawl() -> usize {
    let start = Instant::now();
    let urls = vec!["a.com", "b.com", "c.com", "d.com", "e.com", "f.com", "g.com", "h.com", "i.com", "j.com", "k.com", "l.com", "n.com"].into_iter().map(|s| s.to_string()).collect();
    let total_urls = crawl(urls, Duration::from_millis(8000));
    let duration = start.elapsed();
    println!("Crawled {} urls in () is: {:?}", total_urls, duration);
    total_urls
}

#[cfg(test)]
mod tests {
    use super::do_crawl;
    #[test]
    fn crawl_urls() {
        assert_eq!(do_crawl(), 19032);
    }
}
