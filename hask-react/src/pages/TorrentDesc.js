import React, { useEffect, useState } from "react";
import DirTree from "../components/FileList";
import {mkDirTree, calcSize, formatBytes} from "../static/js/mkDirTree";
import axios from "axios";

const TorrentDesc = (props) => {
  const { hash } = props.match.params;
  const [res, setRes] = useState();

  useEffect(async () => {
    const { data } = await axios.get(
      `http://localhost:8080/torrent/desc/${hash.substring(1, hash.length - 1)}`
    );
    const dirtree = calcSize(mkDirTree(data.dirtree));
    const ifH = data.details["infoHash"];
    const tdesc = {
      title: data.details["title"],
      size: formatBytes(dirtree.size, 3),
      time: data.details["creation date"],
      creator: data.details["created by"],
      seeds: data.details["seed"],
      leeches: data.details["leech"],
      description: data.description,
      files: dirtree.children,
      infoHash: ifH.substring(1, ifH.length - 1),
    };
    setRes(tdesc);
  }, []);

  if (!res) return null;
  return (
    <div className="torrent-desc">
      <p className="torrent-desc__title">{res.title}</p>
      <div className="torrent-desc__details torrent-desc--sub_box">
        <dl className="torrent-desc__details__field">
          <dt>
            <i className="far fa-save"></i> Size
          </dt>
          <dd>{res.size}</dd>
        </dl>

        <dl className="torrent-desc__details__field">
          <dt>
            <i className="far fa-upload"></i> Uploaded At
          </dt>
          <dd>{res.time}</dd>
        </dl>

        <dl className="torrent-desc__details__field">
          <dt>
            <i className="far fa-user"></i> Uploaded By
          </dt>
          <dd>{res.creator}</dd>
        </dl>

        <dl className="torrent-desc__details__field">
          <dt>
            <i className="far fa-seedling"></i> Seeds
          </dt>
          <dd>{res.seeds}</dd>
        </dl>
        <dl className="torrent-desc__details__field">
          <dt>
            <i className="far fa-arrow-circle-down"></i> Leeches
          </dt>
          <dd>{res.leeches}</dd>
        </dl>
      </div>
      <div className="torrent-desc__files torrent-desc--sub_box">
        <div className="torrent-desc--title">
          <i className="far fa-folder-open"></i>
          <span>File-List</span>
        </div>
        <DirTree src={res.files} />
      </div>
      <button className="torrent-desc__dl torrent-desc--sub_box">
        <a href={`http://localhost:8080/torrent/download/${res.infoHash}`}>
          Download Now
        </a>
      </button>

      <div className="torrent-desc__readme torrent-desc--sub_box">
        <div className="torrent-desc--title">
          <i className="far fa-prescription-bottle"></i> Description
        </div>
        <p>{res.description}</p>
      </div>
    </div>
  );
};

export default TorrentDesc;