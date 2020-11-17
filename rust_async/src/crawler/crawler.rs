extern crate rand;
use std::{error::Error, fmt};
use std::time::{Duration, SystemTime, UNIX_EPOCH};
use rand::Rng;
use rand::distributions::Alphanumeric;
use rand::seq::SliceRandom;
use futures::future::{Future, join_all, BoxFuture};
use futures::stream::{FuturesUnordered};
use futures::executor;
use async_std::{task, future};
use async_std::future::timeout;

const MAX_DEPTH: u8 = 4;
const MAX_URLS: u8 = 11;

// Request encapsulates details of url to crawl
#[derive(Debug, Clone, PartialEq)]
pub struct Request {
    pub url: String,
    pub depth: u8,
    pub timeout: Duration,
    pub created_at: u128,
}

impl Request {
    pub fn new(url: String, depth: u8, timeout: Duration) -> Request {
        let epoch = SystemTime::now().duration_since(UNIX_EPOCH).expect("epoch failed").as_millis();
        Request{url: url.to_string(), depth: depth, timeout: timeout, created_at: epoch}
    }
}

#[derive(Debug)]
pub enum CrawlError {
    Unknown,
    MaxDepthReached,
    DownloadError,
    ParseError,
    IndexError,
    ContentsNotChanged,
}

impl Error for CrawlError {}

impl fmt::Display for CrawlError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            CrawlError::MaxDepthReached => write!(f, "MaxDepthReached"),
            CrawlError::DownloadError => write!(f, "DownloadError"),
            CrawlError::ParseError => write!(f, "ParseError"),
            CrawlError::IndexError => write!(f, "IndexError"),
            CrawlError::ContentsNotChanged => write!(f, "ContentsNotChanged"),
            CrawlError::Unknown => write!(f, "Unknown"),
        }
    }
}


// crawling a collection of urls
pub fn crawl(urls: Vec<String>, timeout_dur: Duration) -> usize {
    return do_crawl(urls, timeout_dur, 0);
}

fn do_crawl(urls: Vec<String>, timeout_dur: Duration, depth: u8) -> usize {
    if depth >= MAX_DEPTH {
        return 0
    }

    let mut futures = Vec::new();
    let mut size = 0;

    for u in urls {
        size += 1;
        futures.push(async move {
            let child_urls = match handle_crawl(Request::new(u, depth, timeout_dur)) {
                Ok(urls) => urls,
                Err(_err) => [].to_vec(),
            };
            if child_urls.len() > 0 {
                do_crawl(child_urls, timeout_dur, depth+1)
            } else {
                0
            }
        });
    }
    let _ = task::block_on(
        timeout(timeout_dur, async {
            let res: Vec<usize> = join_all(futures).await;
            size += res.iter().fold(0usize, |sum, n| sum + n);
            Ok::<(), future::TimeoutError>(())
        })
    );
    size
}

// method to crawl a single url
fn handle_crawl(req: Request) -> Result<Vec<String>, CrawlError> {
    let res: Result<Vec<String>, CrawlError> = task::block_on(
        async {
            let contents = match download(&req.url).await {
                Ok(data) => data,
                Err(_err) => return Err(CrawlError::DownloadError),
            };

            if has_contents_changed(&req.url, &contents) && !is_spam(&req.url, &contents) {
                let urls = match index(&req.url, &contents).await {
                    Ok(_) =>
                        match parse_urls(&req.url, &contents) {
                            Ok(urls) => urls,
                            Err(_err) => return Err(CrawlError::ParseError),
                        },
                    Err(_err) => return Err(CrawlError::IndexError),
                };
                return Ok(urls)
            } else {
                return Err(CrawlError::ContentsNotChanged)
            }
        }
    );
    match res {
        Ok(list) => return Ok(list),
        Err(err) => return Err(err),
    }
}

async fn download(url: &str) -> Result<String, CrawlError> {
    // TODO check robots.txt and throttle policies
    // TODO add timeout for slow websites and linearize requests to the same domain to prevent denial of service attack
    // invoke jsrender to generate dynamic content
    jsrender(url, &random_string(100)).await
}

async fn jsrender(_url: &str, contents: &str) -> Result<String, CrawlError> {
    // for SPA apps that use javascript for rendering contents
    Ok(contents.to_string())
}

async fn index(_url: &str, _contents: &str) -> Result<bool, CrawlError> {
    // apply standardize, stem, ngram, etc for indexing
    Ok(true)
}

fn parse_urls(_url: &str, _contents: &str) -> Result<Vec<String>, CrawlError> {
    // tokenize contents and extract href/image/script urls
    Ok((0..MAX_URLS).into_iter().map(|i| random_url(i)).collect())
}

fn has_contents_changed(_url: &str, _contents: &str) -> bool {
    true
}

fn is_spam(_url: &str, _contents: &str) -> bool {
    false
}

fn random_string(max: usize) -> String {
    rand::thread_rng().sample_iter(&Alphanumeric).take(max).collect::<String>()
}

fn random_url(i: u8) -> String {
    let domains = vec!["ab.com", "bc.com", "cd.com", "de.com", "ef.com", "fg.com", "gh.com", "hi.com", "ij.com", "jk.com", "kl.com", "lm.com", "mn.com",
		"no.com", "op.com", "pq.com", "qr.com", "rs.com", "st.com", "tu.com", "uv.com", "vw.com", "wx.com", "xy.com", "yz.com"];
    let domain = domains.choose(&mut rand::thread_rng()).unwrap();
    format!("https://{}/{}_{}", domain, random_string(20), i)
}
