import React, { useRef } from "react";
import { Link, useHistory } from "react-router-dom";
import logo from "../static/img/p2p_logo.png";

const Header = (props) => {
  const query = useRef(null);
  const history = useHistory();
  const handleSubmit = (obj) => {
    obj.preventDefault();
    history.push(`/search/${encodeURI(query.current.value)}`)
    return;
  };
  return (
    <header className="header">
      <div className="header__logo">
        <img src={logo} />
      </div>

      <div className="header__bar">
        <form
          id="search-form"
          className="header__bar__form"
          onSubmit={(obj) => handleSubmit(obj)}
        >
          <input ref={query} type="text" placeholder="Enter Search Term..." />
          <button type="submit" form="search-form" value="Search">
            Search
          </button>
        </form>
      </div>

      <div className="header__right">
        <a>
          <Link to="/">Home </Link>
        </a>
        <a>
          <Link to="/upload">Upload</Link>
        </a>
      </div>
    </header>
  );
};

export default Header;