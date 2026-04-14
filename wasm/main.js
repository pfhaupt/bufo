// Browser runtime for the Demo Virtual Console
function make_environment(...envs) {
    return new Proxy(envs, {
        get(target, prop, receiver) {
            for (let env of envs) {
                if (env.hasOwnProperty(prop)) {
                    return env[prop];
                }
            }
            return (...args) => {
                console.error("NOT IMPLEMENTED: "+prop, args)
                let msg = `Attempted to call function ${prop} from WASM.`;
                alert(`${msg} Check console for more details.`);
                throw `${msg} However, the function is not implemented. Arguments: ${args}`
            }
        }
    });
}

let iota = 0;
const CANVAS_PIXELS = iota++;
const CANVAS_WIDTH  = iota++;
const CANVAS_HEIGHT = iota++;
const CANVAS_STRIDE = iota++;
const CANVAS_SIZE   = iota++;

function readCanvasFromMemory(memory_buffer, canvas_ptr)
{
    const canvas_memory = new BigInt64Array(memory_buffer, canvas_ptr, CANVAS_SIZE);
    return {
        pixels: Number(canvas_memory[CANVAS_PIXELS]),
        width: Number(canvas_memory[CANVAS_WIDTH]),
        height: Number(canvas_memory[CANVAS_HEIGHT]),
        stride: Number(canvas_memory[CANVAS_STRIDE]),
    };
}

function readStringFromMemory(memory_buffer, str_ptr) {
    const buffer = new Uint8Array(memory_buffer);
    let s = "";
    while (buffer[str_ptr] !== 0) {
        s += String.fromCharCode(buffer[str_ptr]);
        str_ptr++;
    }
    return s;
}

(async () => {
    const app = document.getElementById("canvas");
    if (app === null) {
        console.error("Could not find canvas");
        return;
    }

    const konsole = document.getElementById("console");
    if (konsole === null) {
        console.error("Could not find console");
    }
    konsole.innerText = '\n';

    const ctx = app.getContext("2d");
    let print_string = (str) => {
        // NOTE(2026-03-21): If this shit breaks it's because it's AI generated
        const countNewlines = s => (s.match(/\n/g) || []).length;
        const keepLast25 = s => (a=s.split('\n'), a.length>25 ? a.slice(-25).join('\n') : s);
        konsole.innerText = konsole.innerText + str;
        if (countNewlines(konsole.innerText) > 25) {
            konsole.innerText = `\n${keepLast25(konsole.innerText)}`;
        }
    };
    let rt = {
        "print_string": (_str) => {
            let str = readStringFromMemory(memory_buffer, _str);
            print_string(str);
        },
        "print_number": (num) => {
            print_string(`${num}`);
        },
        "puts": (_str) => {
            let str = readStringFromMemory(memory_buffer, _str);
            print_string(str);
        },
        "putchar": (_char) => {
            print_string(String.fromCharCode(_char));
        },
        "get_time": () => {
            return performance.now();
        },
    };

    const w = await WebAssembly.instantiateStreaming(fetch("real.wasm"), {
        "env": make_environment(rt),
    });
    const heap_base = w.instance.exports.__heap_base.value;
    const memory_buffer = w.instance.exports.memory.buffer;

    w.instance.exports.__init_runtime();

    let run = true;
    function render() {
        if (!run) return;
        try {
            w.instance.exports.render_frame(heap_base);
        } catch (e) {
            console.error(e);
            run = false;
            return;
        }
        const canvas = readCanvasFromMemory(memory_buffer, Number(heap_base));
        if (canvas.width != canvas.stride) {
            console.error("canvas width and stride don't match");
            return;
        }
        const image = new ImageData(
            new Uint8ClampedArray(memory_buffer, canvas.pixels, canvas.width*canvas.height*4),
            canvas.width);
        app.width = canvas.width;
        app.height = canvas.height;
        ctx.putImageData(image, 0, 0);
    }
    setInterval(render, 16.67);
})()
