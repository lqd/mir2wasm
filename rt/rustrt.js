//! JavaScript runtime for mir2wasm

// This currently assumes it's running under d8, the V8 shell. We'll
// probably want to make it engine-independent.

let buffer = readbuffer(arguments[0]);

let empty_function = function() {}
let module_handler = {
    get: function(target, module_name) {
        if(module_name == "spectest") {
            return {
                print: print
            };
        }
        return new Proxy({}, {
            get: function(target, func_name) {
                print("Rust requested runtime function " + module_name + "::" + func_name);
                return empty_function;
            }
        });
    }
};
let proxy_ffi = new Proxy({}, module_handler);

let foo = Wasm.instantiateModule(buffer, proxy_ffi);

foo.exports.rust_entry();
