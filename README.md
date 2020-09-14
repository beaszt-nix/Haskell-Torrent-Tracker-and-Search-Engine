# Minor Project
![High-Speed Walk-Through](https://j.gifs.com/L7EGKj.gif)

## Pre-Build 
Install MongoDB, create a database with authentication.
Note the port on which mongo is running.

Add the following environment variables to the ```~/.bash_profile```
and run ```source ~/.bash_profile```

```bash
# Any hostname would work, even localhost:port. 
# Necessary to specify port number
export TorrDBHostName="127.0.0.1:27018"  
# Name of the database created with authentication 
export TorrentDB="torrentDB"
# Name of the Collection used by the Libraries, do not change 
export TorrColl="torrents"
# Name of the username with readWrite privileges to $TorrentDB
export TorrDBUserName="user"
# Password for the same
export TorrDBPassWord="password"
```

## Build and REPL for testing 
```bash
# In the package directory
cabal v2-build
# To create the necessary indices in the Database for text search
cabal v2-run torrdb-init
# To test tracker libraries in REPL 
cabal v2-repl hask-tracker
# To run the tracker 
cabal v2-run hask-tracker

# For testing out the torrent database,
# To upload localhost:8080/upload
# To download localhost:8080/download
mv static/ /var/www/html/
cabal v2-run torrent-db 
```

Once started, find the address of the host that is running the tracker over the desired 
network on which to share the files.

If the address is ```192.168.a.b```, then the trackers to add while creating the torrent are 
```
http://192.168.a.b:6969/announce
udp://192.168.a.b:6969/announce
```

In any torrent-client, ensure that the torrent is automatically added to queue as soon as it is created
in order to create the first seeder.

## Project Features
### Tracker 
The Tracker itself uses [Servant](https://hackage.haskell.org/package/servant-server) along with [Warp](https://hackage.haskell.org/package/warp)
as a part of the HTTP Tracker, and the standard [network](https://hackage.haskell.org/package/network) package for the UDP Server.

The tracker has been implemented using the [BEP Specifications](https://www.bittorrent.org/beps/bep_0000.html) as well as [unofficial community documentation] (https://wiki.theory.org/BitTorrentSpecification).

This tracker is upto standard and supports standard announce as well as scrape features.

### Database
This package utilizes MongoDB in order to store the .torrent metainfo files. 
In order to utilize MongoDB via haskell, [the mongoDB package](https://hackage.haskell.org/package/mongoDB) was utilized.

Since this library takes queries as well as give responses in BSON
  encoding, represented in haskell using [Data.Bson](https://hackage.haskell.org/package/bson), 

It was necessary to implement a translation layer that takes 
translates MetaInformation to BSON from BEncoding, 
as well as to BEncoding from BSON

In order to parse BEncoded metainfo , [bencode](https://hackage.haskell.org/package/bencode) was used.

### Search Backend
This is largely implemented using the inbuilt MongoDB full text search.
However, since MongoDB text searches do not allow fuzzy searches,
(misspellings in query, etc), It was necessary to enhance it. 

For this application ElasticSearch is neither practical nor secure enough
to allow usage.

The methodology is simple.

1. During metainfo upload, process the incoming text fields to generate 
   a list of [ngrams](https://en.wikipedia.org/wiki/N-gram). We use 3-grams
   and 4-grams. 
2. Along with translated metainfo, add another field that contains these ngrams 
   generated from the text fields.
3. Create a text index in the torrents collection for the ngrams field.
4. Any search queries will be processed in the same way as step one.
   This new search query is directly sent to MongoDB text search.

The processing applied to the text is
1. A simple [stop word](https://en.wikipedia.org/wiki/Stop_words) filter.
   The stop words are as specified [here](https://www.ranks.nl/stopwords).   After this step, the text is reduced to a list of key-words
2. In order to allow fuzziness in searching, the keywords are then passed
   through another filter that produces 3-grams and 4-grams of the same.

Even for misspelt queries, we assume that there may be some patterns in it that match the patterns of the actual word we are looking for. This is why even with the mongoDB full text searching, we are able to build in some fuzziness.

Documents are ranked based on number of times query ngrams are found in their list of ngrams 

### Search Frontend
The web-based frontend for the search module is implemented using once again [Servant](https://hackage.haskell.org/package/servant-server)
along with [blaze-html](https://hackage.haskell.org/package/blaze-html) for generating HTML from templates
