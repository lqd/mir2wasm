let buffer = readbuffer('test.wasm');

let empty_function = function() {}
let module_handler = {
  get: function(target, module_name) {
    return new Proxy({}, {
      get: function(target, func_name) {
        return empty_function;
      }
    });
  }
};
let proxy_ffi = new Proxy({}, module_handler);

let foo = Wasm.instantiateModule(buffer, proxy_ffi);

print(foo.exports.fact(5));
