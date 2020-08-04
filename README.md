# Minor Project
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
cabal v2-run torrDB-init
# To test tracker libraries in REPL 
cabal v2-repl hask-tracker
# To run the tracker 
cabal v2-run hask-tracker
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
