real.wasm: wasm.wasm
	wasm-ld -mwasm64 wasm.wasm -o real.wasm --no-entry --export=__heap_base --allow-undefined

wasm.js: wasm.wasm
	emcc -sMEMORY64=1 wasm.wasm -o wasm.js -Oz

wasm.wasm: wasm.bufo
	bufo.exe wasm.bufo -o wasm.wasm --target wasm64-unknown-unknown --dont-link --no-extern-comptime --emit-llvm --verbose --opt --llvm-passes "default<O3>"
	mv wasm.wasm.obj wasm.wasm
