extern crate proc_macro;
use proc_macro::*;
use quote::quote;

#[proc_macro_attribute]
pub fn trace_call(_attr: TokenStream, item: TokenStream) -> TokenStream {
    // item is the function to be injected
    // it adds a println!() to the beginning of the function that prints the function name
    debug_assert!(item.to_string().contains("fn"));
    let func = syn::parse_macro_input!(item as syn::ItemFn);

    let syn::ItemFn {
        attrs,
        vis,
        sig,
        block,
    } = func;
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

    let output = quote!(
        #[cfg(feature = "trace")]
        #traced_output
        #[cfg(not(feature = "trace"))]
        #normal_output
    );

    output.into()
}
