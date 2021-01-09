import React from "react";
import doge from "../static/img/error404.jpg"

const Error404 = (props) => {
  return (
    <div className="torrent-desc">
      <p className="torrent-desc__title">ERROR! I CAN'T FIND WHAT YOU WANT!</p>
      <span
        style={{
          textAlign: "center",
          fontSize: "1.2rem",
        }}
      >
        Do me pardon pls.
      </span>
      <img src={doge} style={{
          objectFit: 'contain',
          width: '70%',
          margin: 'auto'
      }}/>
    </div>
  );
};

export default Error404;
