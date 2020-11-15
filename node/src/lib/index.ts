import { Request, Response } from '../types/index';

const MAX_DEPTH = 4;
const MAX_URLS = 11;
const DOMAINS = [
  'ab.com',
  'bc.com',
  'cd.com',
  'de.com',
  'ef.com',
  'fg.com',
  'gh.com',
  'hi.com',
  'ij.com',
  'jk.com',
  'kl.com',
  'lm.com',
  'mn.com',
  'no.com',
  'op.com',
  'pq.com',
  'qr.com',
  'rs.com',
  'st.com',
  'tu.com',
  'uv.com',
  'vw.com',
  'wx.com',
  'xy.com',
  'yz.com',
];

export class Crawler {
  async crawl(urls: string[], timeoutMillis: number): Promise<Response> {
    return doCrawl(urls, 0, timeoutMillis);
  }
}

///////////////// PRIVATE METHODS ////////////////
const doCrawl = async (
  urls: string[],
  depth: number,
  timeoutMillis: number
): Promise<Response> => {
  const res = new Response();
  if (depth >= MAX_DEPTH) {
    res.failed('max-depth');
    return res;
  }
  const requests = urls.map((u) => new Request(u, depth, timeoutMillis));
  const promises = requests.map((r) => handleCrawl(r));
  const results = await Promise.race([
    Promise.all(promises),
    timeout(timeoutMillis),
  ]);

  const childURLs : number = results.reduce((total: number, r: Response) => total + r.childURLs, 0);
  res.succeeded(childURLs);
  return res;
};


const handleCrawl = async (req: Request): Promise<Response> => {
  const res = new Response();
  const contents = await download(req.url);
  const newContents = await jsrender(req.url, contents);
  if (
    hasContentsChanged(req.url, newContents) &&
    !isSpam(req.url, newContents)
  ) {
    await index(req.url, newContents);
    const urls = await parseURLs(req.url, newContents);
    const childResp = await doCrawl(urls, req.depth + 1, req.timeoutMillis);
    res.succeeded(childResp.childURLs + 1);
  } else {
    res.failed("contents didn't change");
  }
  return res;
};

const download = async (url: string): Promise<string> => {
  // TODO check robots.txt and throttle policies
  // TODO add timeout for slow websites and linearize requests to the same domain to prevent denial of service attack
  return randomString(100);
};

const jsrender = async (url: string, contents: string): Promise<string> => {
  // for SPA apps that use javascript for rendering contents
  return contents;
};

const index = async (url: string, contents: string) => {
  // apply standardize, stem, ngram, etc for indexing
};

const parseURLs = (url: string, contents: string): string[] => {
  // tokenize contents and extract href/image/script urls
  const urls = [];
  for (var i = 0; i < MAX_URLS; i++) {
    urls.push(randomUrl());
  }
  return urls;
};

const hasContentsChanged = (url: string, contents: string): boolean => {
  return true;
};

const isSpam = (url: string, contents: string): boolean => {
  return false;
};

const randomUrl = (): string => {
  const i = Math.floor(Math.random() * DOMAINS.length);
  return 'https://' + DOMAINS[i] + '/' + randomString(20);
};

const randomString = (n: number): string => {
  let letters =
    'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789';
  let text = '';
  for (let i = 0; i < n; i++) {
    text += letters.charAt(Math.floor(Math.random() * letters.length));
  }
  return text;
};

const timeout = (ms: number): Promise<any> => {
  return new Promise((resolve, reject) => setTimeout(
	  () => {
		reject(new Error(`Timed out ${ms}`))
	  }, ms));
};
