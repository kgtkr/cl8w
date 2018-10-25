const fs = require("fs");

const buf = fs.readFileSync("./test.wasm");
const mod = new WebAssembly.Module(buf);
const instance = new WebAssembly.Instance(mod, {});
console.log(instance.exports.gcd(24, 36));