const fs = require("fs");

const memory = new WebAssembly.Memory({ initial: 10 });

const buf = fs.readFileSync("./test.wasm");
const mod = new WebAssembly.Module(buf);
const instance = new WebAssembly.Instance(mod, {
  resource: {
    memory: memory
  },
  memory: new WebAssembly.Instance(new WebAssembly.Module(fs.readFileSync("./memory.wasm")), {
    resource: {
      memory: memory
    },
    config: {
      start: 1
    }
  },
  ).exports,
  io: {
    print: x => console.log(x)
  }
});
instance.exports.main()