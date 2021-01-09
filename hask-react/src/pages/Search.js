import React, {useEffect, useState} from "react";
import axios from 'axios';
import ResultCard from "../components/ResultCard";

const sampleData = {
  title: "This is a very long torrent title",
  seed: 123,
  leech: 234,
  createdBy: "Stephen",
  uploadedAt: "2018:20:20 19:28:28",
};

const Results = (props) => {
  const { term } = props.match.params;
  const [results, setResults] = useState([])
  useEffect(async () => {
    const res = await axios.get(`http://localhost:8080/search/${term}`);
    setResults(res.data);
  }, [results]);

  return (
    <div className="results">
      { results? results.map((x) => {
              return <ResultCard details={x} />;
            }) : null }
    </div>
  );
};

export default Results;
