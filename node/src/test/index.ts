import { Crawler } from '../lib/index';
import { expect } from 'chai';

const EXPECTED_URLS = 19032;
const ROOT_URLS = [
  'a.com',
  'b.com',
  'c.com',
  'd.com',
  'e.com',
  'f.com',
  'g.com',
  'h.com',
  'i.com',
  'j.com',
  'k.com',
  'l.com',
  'n.com',
];

describe('crawler', async () => {
  it('crawling urls with nesting', async () => {
    const started = new Date().getTime();
    const timeout = 5000;
    const crawler = new Crawler();
    const res = await crawler.crawl(ROOT_URLS, timeout);
    const elapsed = new Date().getTime() - started;
    console.log(`Crawl took ${elapsed} to process ${res}`);
    expect(res).equal(EXPECTED_URLS);
  });

});
