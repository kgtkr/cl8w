const fs = require("fs");

const memory = new WebAssembly.Memory({ initial: 10 });

const buf = fs.readFileSync("./main.wasm");
const mod = new WebAssembly.Module(buf);
const instance = new WebAssembly.Instance(mod, {
  resource: {
    memory: memory
  },
  memory: new WebAssembly.Instance(new WebAssembly.Module(fs.readFileSync("./memory/memory.wasm")), {
    resource: {
      memory: memory
    }
  },
  ).exports,
  io: {
    print: x => console.log(x)
  }
});
instance.exports.main()