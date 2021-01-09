# HaskTorrent

## Brief Summary

1. **Language:** Haskell

2. **Libraries:**
   
   1. **Rest WebAPI:** [Servant](https://hackage.haskell.org/package/servant-server)
   2. **HTTP Server:** [Warp](https://hackage.haskell.org/package/warp)
   3. **MongoDB Database Access:** [mongoDB](https://hackage.haskell.org/package/mongoDB)
   4. **BSON Parsing (for DB):** [bson](https://hackage.haskell.org/package/bson)
   5. **JSON Parsing (for API):** [aeson](https://hackage.haskell.org/package/aeson)

3. **Database:** MongoDB

4. **Search Backend:** MongoDB Full Text Search.

## Fuzzy Text Search Implementation

 The methodology is simple.

1. During metainfo upload, process the incoming text fields to generate 
   a list of [ngrams](https://en.wikipedia.org/wiki/N-gram). We use 3-grams and 4-grams.
2. This list of N-Grams is used as a text-index for the torrents in MongoDB.
3. Any search queries will be processed in the same way as step one.
   This processed query is directly sent to MongoDB text search.

The processing applied to the text is

1. A simple [stop word](https://en.wikipedia.org/wiki/Stop_words) filter using the words  from [here](https://www.ranks.nl/stopwords). After this step, the text is reduced to a list of key-words
2. In order to allow fuzziness in searching, the keywords are then passed
   through another filter that produces 3-grams and 4-grams of the same.

Misspelt queries, often contain some patterns in them that match those found in exact queries. Since we store text-indices to identify torrents, we can rank appropriate documents using similarity algorithms, and standard Information retrieval methods, provided by MongoDB. 

Pattern similarity helps us bring fuzziness inspite of the limited capabilities of MongoDB Full Text Search. 

## API Description

This package's primary function is to serve a REST API with the following routes

- (GET) "/search/:search": Takes search term as parameter, and returns a JSON Array of objects, containing brief descriptions of possible matches. The Object contains the following keys 
  
  - 'infoHash': ID to use for fetching detailed description or download
  
  - 'title': Self explanatory
  
  - 'seed': Number of seeders for that torrent
  
  - 'leech': Number of leechers for that torrent
  
  - 'created by': Name of torrent creator, anonymous if not specified.
  
  - 'creation date': Time at which torrent was first uploaded.

- (GET) "/torrent/desc/:infoHash": Takes infoHash found from previous route as parameters, and returns a JSON Object containing detailed description of the torrent. Same keys as that mentioned above, with the addition of
  
  - 'file-list': List of files and their directory structure found by the torrent, including sizes.
  
  - 'description': A plain text description of the torrent written by uploaded to describe the contents of the torrent. 

- (GET) "/torrent/download/:infoHash": Takes infoHash as parameter, and returns .torrent metainfo file, which is automatically opened by Torrent Clients on most browsers, as it has mime-type "application/x-bittorrent"

- (POST) "/upload" : Takes a JSON Form containing 
  
  - 'title': Title of the Torrent (used for search)
  
  - 'description': Plain text description of torrent explaining it's contents. (used for search)
  
  - 'metainfo': Contains .torrent file to be uploaded, in ByteArray representation
  
  Returns a String, containing the infoHash of the newly uploaded torrent.
