# torscraper 2

a haskell version of [torscraper](https://github.com/justinwoo/torscraper). scrapes a specific tracker's listings and downloads magnet links to `./downloads`.

## Usage

Requires a `config.json` with this schema:

```json
{
  "url": "httpsUrlForSearchResults",
  "blacklist": [
    "Substring of title you don't want to download"
  ]
}
```
