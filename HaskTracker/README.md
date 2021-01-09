# HaskTracker

## Brief Summary

1. **Language:** [Haskell](https://www.haskell.org/)

2. **Libraries**: 
   
   1. **Rest WebAPI:** [Servant](https://hackage.haskell.org/package/servant-server)
   
   2. **HTTP Server:**  [Warp](https://hackage.haskell.org/package/warp)
   
   3. **UDP Server (Sockets):** [Network](https://hackage.haskell.org/package/network)
   
   4. **Concurrency Management:** Haskell Base Libraries

## Design Description

This tracker provides two services, that are mentioned by the [BEP Specifications](https://www.bittorrent.org/beps/bep_0000.html).  

- Responds to *Announce* Requests at 'http://tracker-ip:6969/announce' and 'udp://tracker-ip:6969/announce' 

- Responds to *Scrape* Requests at 'http://tracker-ip:6969/scrape' and 'udp://tracker-ip:6969/scrape'

Protocol Specific Information on the exact structure of these requests is found from [TheoryOrg](https://wiki.theory.org/BitTorrentSpecification).

The peer-selection protocol is effectively a round-robin algorithm, by randomizing the selection of peers, thus proving an equal probability of selection to each peer, ensuring fairness.

The state information for this tracker is maintained in memory, and can be rebuilt subsequently after power-failures or server downtimes, providing no additional server interruption to clients.
