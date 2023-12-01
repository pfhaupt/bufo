extern crate proc_macro;
use proc_macro::*;

#[proc_macro_attribute]
pub fn trace_call(_attr: TokenStream, item: TokenStream) -> TokenStream {
    // item is the function to be injected
    // it adds a println!() to the beginning of the function that prints the function name
    debug_assert!(item.to_string().contains("fn"));
    let item_str = item.to_string();
    let fn_pub = if item_str.contains("pub") { "pub " } else { "" };
    let fn_name = item_str
        .split("fn")
        .nth(1)
        .unwrap()
        .split("(")
        .nth(0)
        .unwrap()
        .trim()
        .to_string();
    println!("[LOG] Injecting log into function `{}`", fn_name);

    let fn_args = item_str
        .splitn(2, "(")
        .nth(1)
        .unwrap()
        .splitn(2, ")")
        .nth(0)
        .unwrap()
        .trim()
        .to_string();

    let fn_return = if item.to_string().contains("->") {
        "->".to_owned()
            + &item_str
                .splitn(2, "->")
                .nth(1)
                .unwrap()
                .splitn(2, "{")
                .nth(0)
                .unwrap()
                .trim()
                .to_string()
    } else {
        "".to_string()
    };

    let fn_body = item_str
        .splitn(2, "{")
        .nth(1)
        .unwrap()
        .rsplitn(2, "}")
        .nth(1)
        .unwrap()
        .trim()
        .to_string();

    // Only inject the tracing code if the feature is enabled
    let tracing_fn = format!(
        "
        {} fn {}({}) {} {{
            println!(\"[LOG] {{}}:{{}}: {}() called\", file!(), line!());
            {}
        }}",
        fn_pub, fn_name, fn_args, fn_return, fn_name, fn_body
    );
    let normal_fn = format!(
        "
        {} fn {}({}) {} {{
            {}
        }}",
        fn_pub, fn_name, fn_args, fn_return, fn_body
    );

    let feature_fn = format!(
        "#[cfg(feature = \"trace\")]
        {}
        #[cfg(not(feature = \"trace\"))]
        {}",
        tracing_fn, normal_fn
    );

    feature_fn.parse().unwrap()
}
