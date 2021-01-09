import React from "react";
import axios from "axios";
import { useForm } from "react-hook-form";
import { useHistory } from "react-router-dom";

const Upload = (props) => {
  const { register, handleSubmit, watch } = useForm();
  const chosenFile = watch("metainfo", null);
  const history = useHistory();
  const readFileData = (e) => {
    const file = e;

    return new Promise((resolve, reject) => {
      const reader = new FileReader();
      reader.onload = (event) => {
        resolve(event.target.result);
      };
      reader.onerror = (err) => {
        reject(err);
      };
      reader.readAsBinaryString(file);
    });
  };

  const onSubmit = async (data) => {
    var modData = data;
    modData.metainfo = await data.metainfo[0];
    const str = await readFileData(modData.metainfo);
    modData.metainfo = str;
    const res = await axios.post("http://localhost:8080/upload", modData, {
      headers: { "Content-Type": "application/json" },
    });
    console.log(res);
    history.push(`/torrent/"${res.data}"`);
  };

  return (
    <div class="uploads">
      <form
        class="uploads-form"
        id="uploads-form"
        onSubmit={handleSubmit(onSubmit)}
      >
        <input
          name="title"
          type="text"
          placeholder="Title"
          ref={register({ required: true })}
        />
        <textarea
          name="description"
          rows="12"
          cols="50"
          placeholder="Enter Description..."
          ref={register({ required: true })}
        />
        <label id="upload-label" class="uploads-form__fileselect">
          <input
            type="file"
            accept=".torrent"
            name="metainfo"
            ref={register({ required: true })}
          />
          {chosenFile !== null ? `${chosenFile[0].name}` : "Choose File..."}
        </label>
        <button type="submit" form="uploads-form" value="Upload">
          Upload Torrent
        </button>
      </form>
    </div>
  );
};

export default Upload;