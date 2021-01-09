import React from "react";
import { Link } from "react-router-dom";

const ResultCard = ({ details }) => {
  const { title, seed, leech, "creation date": uploadedAt, "created by": createdBy, infoHash} = details;

  return (
    <div className="results-card">
      <div className="results-card__header">
        <p>
          <a>
            <Link to={`/torrent/${infoHash}`}>{title}</Link>
          </a>
        </p>
      </div>
      <ul className="results-card__scrape">
        <li>
          <i className="far fa-seedling"></i> {seed}{" "}
        </li>
        <li>
          <i className="far fa-arrow-circle-down"></i> {leech}{" "}
        </li>
      </ul>
      <div className="results-card__details">
        <div className="results-card__uploaded">Uploaded: {uploadedAt}</div>
        <div className="results-card__uploader">Created By: {createdBy}</div>
      </div>
    </div>
  );
};

export default ResultCard;
