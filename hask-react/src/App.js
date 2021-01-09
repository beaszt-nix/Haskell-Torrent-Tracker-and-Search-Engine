import React from "react";
import { Route, Switch } from "react-router-dom";
import Header from "./components/Header";
import Upload from "./pages/Upload";
import Results from "./pages/Search";
import TorrentDesc from "./pages/TorrentDesc";
import Error404 from "./pages/Error404";

function App() {
  return (
    <>
      <Route path="/" component={Header} />
      <Switch>
        <Route exact path="/upload" component={Upload} />
        <Route exact path="/search/:term" component={Results} />
        <Route exact path="/torrent/:hash" component={TorrentDesc} />
        <Route exact path="/error404" component={Error404} />
      </Switch>
    </>
  );
}

export default App;