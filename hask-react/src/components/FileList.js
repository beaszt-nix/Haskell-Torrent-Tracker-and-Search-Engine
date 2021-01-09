import React, { useState } from "react";
import {formatBytes} from "../static/js/mkDirTree";

const LineTitle = ({ name, size, clickAction }) => {
  const setCollapse = clickAction;
  const handleClick = () => {
    if (setCollapse) {
      setCollapse((collapse) => (collapse ? false : true));
    }
  };
  return (
    <div className="file-list__leaf" onClick={() => handleClick()}>
      <i className={`far ${clickAction ? "fa-folder" : "fa-file"}`}></i>
      <span>{name}</span>
      <span style={{float: 'right'}}>{formatBytes(size)}</span>
    </div>
  );
};

const Folder = ({ name, next, size }) => {
  const [collapse, setCollapse] = useState(true);
  return (
    <div class="file-list__folder">
      <LineTitle name={name} size={size} isFile={false} clickAction={setCollapse} />
      <div className="file-list__folder__subdirectory">
        {collapse ? null : <DirTree src={next} />}
      </div>
    </div>
  );
};

const DirTree = ({ src }) => {
  const root = src;
  return (
    <div className="file-list">
      {root.map(({ name, size, children }) => {
        if (children.length === 0) {
          return <LineTitle name={name} size={size} isFile={true} />;
        } else {
          return <Folder name={name} size={size} next={children} />;
        }
      })}
    </div>
  );
};

export default DirTree;