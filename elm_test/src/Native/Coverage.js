const fs = require('fs');
const path = require('path');
var make = function make(elm) {
  elm.Native = elm.Native || {};
  elm.Native.Coverage = elm.Native.Coverage || {};

  if (elm.Native.Coverage.values) return elm.Native.Coverage.values;

  return elm.Native.Coverage.values = {
    readTix: filename => {
      try {
        return fs.readFileSync(path.join('.epc', filename + '.tix')).toString();
      } catch(e) {
        return "";
      }
    },
    tick: x => filename => tix => {
      fs.writeFileSync(path.join('.epc', filename + ".tix"), tix);

      return x;
    },
  };
};

Elm.Native.Coverage = {};
Elm.Native.Coverage.make = make;
