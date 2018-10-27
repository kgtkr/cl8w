const fs = require("fs");

const buf = fs.readFileSync("./test.wasm");
const mod = new WebAssembly.Module(buf);
const instance = new WebAssembly.Instance(mod, {
  resource: {
    memory: new WebAssembly.Memory({ initial: 1 })
  },
  memory: new WebAssembly.Instance(new WebAssembly.Module(fs.readFileSync("./memory.wasm")), {
    resource: {
      memory: new WebAssembly.Memory({ initial: 1 })
    },
    config: {
      start: 1
    }
  }).exports

});
console.log(instance.exports.gcd(24, 36));