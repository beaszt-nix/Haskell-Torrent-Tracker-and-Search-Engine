const mkDirTree = (paths) => {
  const newNode = (name, size) => {
    return {
      name : name ? name : null,
      size: size,
      children : [] 
    }
  };
  const insertFilePath = ( {fst, snd}, root) => {
    if (fst.length === 0) 
      return root;
    const [head, ...tail] = fst;
    let { name, children, size } = root;
    const idx = children.findIndex( x => x.name === head );
    if (idx === -1) {
      const nnode = newNode(head, snd); 
      children.push(nnode);
      children[children.length -1] = insertFilePath({fst: tail, snd: snd}, children[children.length -1]);
    }
    else {
      children[idx] = insertFilePath({fst: tail, snd: snd}, children[idx]);
    }
    return {
      name: name,
      children: children,
      size: size
    };
  };  
  
  let nroot = newNode(false, 0);
  for (var i = 0; i < paths.length; i++) {
    nroot = insertFilePath(paths[i], nroot);
  }

  return nroot;
};

const calcSize = (node) => {
  const {name, children, size} = node;
  if (children.length === 0)
    return node;

  let sz = 0;
  
  for (var i = 0; i < children.length; i++) {
    children[i] = calcSize(children[i]);
    sz += children[i].size;
  }

  return {
    name: name,
    children: children,
    size: sz
  }
};

function formatBytes(bytes, decimals = 2) {
  if (bytes === 0) return '0 Bytes';

  const k = 1024;
  const dm = decimals < 0 ? 0 : decimals;
  const sizes = ['Bytes', 'KB', 'MB', 'GB', 'TB', 'PB', 'EB', 'ZB', 'YB'];

  const i = Math.floor(Math.log(bytes) / Math.log(k));

  return parseFloat((bytes / Math.pow(k, i)).toFixed(dm)) + ' ' + sizes[i];
}

export {mkDirTree, calcSize, formatBytes};