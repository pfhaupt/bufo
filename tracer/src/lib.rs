extern crate proc_macro;
use proc_macro::*;
use quote::quote;

// There are 2 tracer feature flags:
// - trace: Trace all functions with the #[trace_call(always)] attribute
// - trace_extra: Trace all functions with the #[trace_call(extra)] and all functions traced above
// - default: No tracing

macro_rules! parse_traced {
    ($func:ident) => {{
        let syn::ItemFn {
            attrs,
            vis,
            sig,
            block,
        } = $func;
        let name = sig.ident.to_string();

        let block = if attrs.iter().any(|attr| attr.path.is_ident("track_caller")) {
            quote! { #block }
        } else {
            quote! {
                (move || #block )()
            }
        };

        let traced_output = quote! {
            #[track_caller]
            #(#attrs)*
            #vis #sig {
                println!("[LOG] {}: Entering `{}`",
                    ::core::panic::Location::caller(),
                    #name,
                );
                #block
            }
        };
        let normal_output = quote! {
            #(#attrs)*
            #vis #sig {
                #block
            }
        };
        (traced_output, normal_output)
    }};
}

fn trace_call_always(item: TokenStream) -> TokenStream {
    let func = syn::parse_macro_input!(item as syn::ItemFn);
    let (traced, normal) = parse_traced!(func);

    let output = quote!(
        #[cfg(any(feature = "trace", feature = "trace_extra"))]
        #traced
        #[cfg(not(any(feature = "trace", feature = "trace_extra")))]
        #normal
    );
    output.into()
}

fn trace_call_extra(item: TokenStream) -> TokenStream {
    let func = syn::parse_macro_input!(item as syn::ItemFn);
    let (traced, normal) = parse_traced!(func);

    let output = quote!(
        #[cfg(any(feature = "trace_extra"))]
        #traced
        #[cfg(not(any(feature = "trace_extra")))]
        #normal
    );

    output.into()
}

#[proc_macro]
pub fn trace_panic(input: TokenStream) -> TokenStream {
    let output = if input.is_empty() {
        quote!(
            #[cfg(any(feature = "trace", feature = "trace_extra"))]
            panic!("This panic has been inserted by the tracer.")
        )
    } else {
        // The fact that we have to call `trace_panic!((v1, v2, v3))` is kinda stupid
        // FIXME: Find a way to not use a Tuple, and instead a Comma-separated idk-Expr
        let stuff = syn::parse_macro_input!(input as syn::ExprTuple);
        let output: Vec<_> = stuff.elems.iter().map(|e| {
            quote!(println!("[LOG] {}: {:?}", stringify!(#e), #e);)
        }).collect();
        let output = quote!(
            #[cfg(any(feature = "trace", feature = "trace_extra"))]
            {
                #(#output)*
                panic!("[LOG] This panic has been inserted by the tracer.")
            }
        );
        output
    };
    output.into()
}

#[proc_macro_attribute]
pub fn trace_call(attr: TokenStream, item: TokenStream) -> TokenStream {
    // item is the function to be injected
    // it adds a println!() to the beginning of the function that prints the function name
    debug_assert!(item.to_string().contains("fn"));

    let mode = attr.to_string();
    if mode.len() == 0 {
        let error = syn::Error::new_spanned(mode, "Expected one attribute");
        return error.to_compile_error().into();
    }

    // depending on the attribute, we either add tracing or not
    match mode.as_str() {
        "always" => trace_call_always(item),
        "extra" => trace_call_extra(item),
        _ => {
            let error = syn::Error::new_spanned(mode, "Expected one of `always` or `extra`");
            error.to_compile_error().into()
        }
    }
}
