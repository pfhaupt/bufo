wasm.js: wasm.wasm
	emcc -sMEMORY64=1 wasm.wasm -o wasm.js -Oz

wasm.wasm: wasm.bufo
	bufo.exe wasm.bufo -o wasm.wasm --target wasm64-unknown-unknown --dont-link --no-extern-comptime --emit-llvm
	mv wasm.wasm.obj wasm.wasm
