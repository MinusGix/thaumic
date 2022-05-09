use std::collections::HashSet;

use darling::{FromAttributes, FromField, FromMeta, FromMetaItem};
use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::{format_ident, quote, quote_spanned};
use syn::{
    parse::{Parse, ParseStream},
    parse_macro_input,
    punctuated::Punctuated,
    spanned::Spanned,
    AttrStyle, Attribute, Ident, ItemStruct, PathArguments, PathSegment, Token, Type, Visibility,
};

fn comp_err(span: Span, text: impl AsRef<str>) -> TokenStream {
    let text = text.as_ref();
    quote_spanned! {span => compile_error!(#text); }.into()
}

#[proc_macro_attribute]
pub fn thaumic(metadata: TokenStream, input: TokenStream) -> TokenStream {
    let ItemStruct {
        attrs,
        vis,
        ident,
        generics,
        fields,
        ..
    } = parse_macro_input!(input);

    let lifetime = generics.lifetimes().next();
    if let Some(lifetime) = lifetime {
        return comp_err(
            lifetime.span(),
            "Thaumic structures cannot contain lifetimes at the moment!",
        );
    }

    let generic = generics.type_params().next();
    if let Some(generic) = generic {
        return comp_err(
            generic.span(),
            "Thaumic structures cannot contain generics at the moment!",
        );
    }

    let const_generic = generics.const_params().next();
    if let Some(const_generic) = const_generic {
        return comp_err(
            const_generic.span(),
            "Thaumic structures cannot contain const generics at the moment!",
        );
    }

    match fields {
        syn::Fields::Named(named_fields) => match Instance::new(vis, ident, attrs, named_fields) {
            Ok(instance) => match instance.generate() {
                Ok(data) => data,
                Err(err) => err.write_errors().into(),
            },
            Err(err) => err.write_errors().into(),
        },
        syn::Fields::Unnamed(_) | syn::Fields::Unit => comp_err(
            ident.span(),
            "Thaumic structures currently only support normal structure syntax.",
        ),
    }
}

struct Field {
    vis: Visibility,
    ident: Ident,
    ty: Type,
    dst: Option<DstAttribute>,
}

#[derive(Debug, Copy, Clone)]
enum StorageType {
    /// Allocate it as a pointer, managing the memory ourself
    AllocManaged,
    /// Try to make an actual rust structure
    TypicalStructure,
    // TODO: Alloc structure type (like an actual structure but it focuses on providing a managed
    // heap allocated version, mostly for dsts, though this isn't too useful currently due to the
    // poor support for dsts)
    // TODO: Alloca managed/structure type? Probably not for a long while.
    // TODO: Auto storage?
}
impl StorageType {
    fn supports_fancy_dst(self) -> bool {
        match self {
            StorageType::AllocManaged => true,
            StorageType::TypicalStructure => false,
        }
    }
}

struct InstanceConf {
    vis: Visibility,
    name: Ident,
    storage: StorageType,
}

struct Instance {
    conf: InstanceConf,
    fields: Vec<Field>,
    // TODO: Attributes
    // TODO: Generics

    // Outputs warnings to this to be displayed
    warning_output: Vec<String>,
}
impl Instance {
    fn new(
        vis: Visibility,
        name: Ident,
        attrs: Vec<Attribute>,
        fields: syn::FieldsNamed,
    ) -> darling::Result<Instance> {
        let mut storage = None;
        for attr in attrs {
            if !matches!(attr.style, AttrStyle::Outer) {
                return Err(syn::Error::new(
                    attr.path.span(),
                    "Only #[] attributes are currently supported",
                )
                .into());
            }

            if attr.path.segments.len() == 1 {
                let segment = &attr.path.segments[0];
                let segment_span = segment.span();

                let segment = segment.ident.to_string();

                match segment.as_str() {
                    // Allocate, managed by thaumic
                    "alloc" => {
                        if storage.is_some() {
                            return Err(syn::Error::new(
                                segment_span,
                                "Storage type already declared",
                            )
                            .into());
                        }

                        storage = Some(StorageType::AllocManaged);
                    }
                    _ => {
                        return Err(syn::Error::new(
                            attr.path.span(),
                            "Unknown attribute kind. Custom attributes are not supported.",
                        )
                        .into());
                    }
                }
            } else {
                return Err(syn::Error::new(
                    attr.path.span(),
                    "Unknown attribute kind. Custom attributes are not supported.",
                )
                .into());
            }
        }

        let storage = storage.unwrap_or(StorageType::TypicalStructure);

        if matches!(storage, StorageType::TypicalStructure) {
            return Err(syn::Error::new(name.span(), "Default storage type is currently not supported, though it plans to be. You might want to add `#[alloc]` as in the examples.").into());
        }

        let mut instance_conf = InstanceConf { vis, name, storage };
        let fields = Instance::extract_validate(&mut instance_conf, fields)?;

        Ok(Instance {
            conf: instance_conf,
            fields,
            warning_output: Vec::new(),
        })
    }

    fn extract_validate(
        conf: &mut InstanceConf,
        fields: syn::FieldsNamed,
    ) -> darling::Result<Vec<Field>> {
        let mut seen_names: HashSet<String> = HashSet::new();
        let mut fields_result = Vec::with_capacity(fields.named.len());
        for field in fields.named {
            let field = Instance::extract_validate_field(conf, field, &mut seen_names)?;
            fields_result.push(field);
        }

        Ok(fields_result)
    }

    fn extract_validate_field(
        conf: &mut InstanceConf,
        field: syn::Field,
        seen_names: &mut HashSet<String>,
    ) -> darling::Result<Field> {
        let field_span = field.span();
        // Guard against duplicate fields
        let field_name = if let Some(field_name) = field.ident {
            if !seen_names.insert(field_name.to_string()) {
                return Err(
                    syn::Error::new(field_name.span(), "There was a duplicate field").into(),
                );
            }
            field_name
        } else {
            return Err(syn::Error::new(
                field.span(),
                "There is no support for unnamed fields currently.",
            )
            .into());
        };

        let field_ident_span = field_name.span();

        let mut dst_attr = None;
        for attr in field.attrs {
            if !matches!(attr.style, AttrStyle::Outer) {
                return Err(syn::Error::new(
                    attr.path.span(),
                    "Only #[] attributes are currently supported",
                )
                .into());
            }

            if attr.path.segments.len() == 1 {
                let segment = &attr.path.segments[0];
                if !matches!(segment.arguments, PathArguments::None) {
                    return Err(syn::Error::new(
                        segment.span(),
                        "Attribute may not have path arguments.",
                    )
                    .into());
                }

                let segment_span = segment.span();
                let segment = segment.ident.to_string();
                match segment.as_str() {
                    "dst" => {
                        if dst_attr.is_some() {
                            return Err(
                                syn::Error::new(segment_span, "Duplicate dst attribute").into()
                            );
                        }

                        if !conf.storage.supports_fancy_dst() {
                            return Err(syn::Error::new(segment_span, "Currently DSTs are not supported in this way. You may want to put #[alloc] on the structure.").into());
                        }

                        dst_attr = Some(attr);
                    }
                    // TODO: #[from], #[try_from]
                    _ => {
                        return Err(syn::Error::new(
                            attr.path.span(),
                            "Unknown attribute kind. Custom attributes are not supported.",
                        )
                        .into())
                    }
                }
            } else {
                return Err(syn::Error::new(
                    attr.path.span(),
                    "Unknown attribute kind. Custom attributes are not supported.",
                )
                .into());
            }
        }

        let dst = if let Some(dst) = dst_attr {
            if !is_ty_fancy_dst(&field.ty).unwrap_or(false) {
                return Err(syn::Error::new(field_ident_span, "Type is not a DST, or it is unrecognized as a DST. We have trouble peeking past paths, but this coould be improved so that we don't rely on it.").into());
            }

            if !conf.storage.supports_fancy_dst() {
                return Err(syn::Error::new(field_ident_span, "Type was a DST but the storage type was not in a mode that supports it. Consider using #[alloc]").into());
            }

            let meta = dst.parse_meta()?;
            let args = DstAttribute::from_meta(&meta)?;

            Some(args)
        } else if is_ty_fancy_dst(&field.ty).unwrap_or(false) {
            return Err(syn::Error::new(
                field_ident_span,
                "Type was a DST and so should be marked with the #[dst()] attribute",
            )
            .into());
        } else {
            None
        };

        Ok(Field {
            vis: field.vis,
            ident: field_name,
            ty: field.ty,
            dst,
        })
    }

    fn generate(self) -> darling::Result<TokenStream> {
        match self.conf.storage {
            StorageType::AllocManaged => self.generate_alloc_managed(),
            StorageType::TypicalStructure => todo!(),
        }
    }

    fn generate_alloc_managed(self) -> darling::Result<TokenStream> {
        // The name of the hidden module that we'll be outputting which contains hidden
        // implementation details
        let module_name = format_ident!("__{}_impl", self.conf.name);

        let main_structure_vis = &self.conf.vis;
        let main_structure_name = &self.conf.name;

        let structure_tokens = {
            // TODO: custom allocators
            // TODO: Pub here is probably too lenient and technically might expose types that
            // shouldn't be public, though they're in the hidden impl and so they shouldn't use it
            // anyway
            // TODO: Put in doc comments on structure onto generated structure
            // TODO: Put in doc comments on field getters
            let structure_definition = quote! {
                pub struct #main_structure_name {
                    /// Raw pointer to the allocated data
                    /// For things with dst we can't explicitly name the type
                    /// and alloc managed does the management anyway
                    ptr: core::ptr::NonNull<u8>,
                }
            };
            let constructors = self.generate_constructors_alloc_managed(main_structure_name)?;
            let structure_impl = quote! {
                impl #main_structure_name {
                    #constructors
                }
            };
            let structure_private_impl = quote! {
                // Implements internal functions
                #[doc(hidden)]
                trait Impl {

                }
            };

            quote! {
                #structure_definition
                #structure_impl

                #structure_private_impl
            }
        };
        // TODO: const asserts that held types which shouldn't be dsts aren't dsts

        Ok(quote! {
            #[doc(hidden)]
            #main_structure_vis mod #module_name {
                #structure_tokens
            }

            #main_structure_vis use #module_name::#main_structure_name;
        }
        .into())
    }

    fn generate_constructors_alloc_managed(
        &self,
        main_structure_name: &Ident,
    ) -> darling::Result<proc_macro2::TokenStream> {
        #[derive(Default)]
        struct Output {
            parameter_names: Vec<Ident>,
            parameter_types: Vec<proc_macro2::TokenStream>,
            initial_setup: Vec<proc_macro2::TokenStream>,
            store: Vec<proc_macro2::TokenStream>,
        }
        impl Output {
            fn push(
                &mut self,
                name: Ident,
                typ: proc_macro2::TokenStream,
                setup: proc_macro2::TokenStream,
                store: proc_macro2::TokenStream,
            ) {
                self.parameter_names.push(name);
                self.parameter_types.push(typ);
                self.initial_setup.push(setup);
                self.store.push(store);
            }
        }

        // The name of the ptr we are writing to
        let ptr_name = Ident::new("ptr", Span::call_site());
        // The name of the layout we are editing and redefining
        let layout_name = Ident::new("layout", Span::call_site());

        let mut has_dst = false;
        let mut most_direct = Output::default();
        for field in self.fields.iter() {
            let name = &field.ident;
            let ty = &field.ty;
            if let Some(dst) = &field.dst {
                has_dst = true;
                match ty {
                    Type::Array(_) => todo!(),
                    Type::BareFn(_) => todo!(),
                    Type::Group(_) => todo!(),
                    Type::ImplTrait(_) => todo!(),
                    Type::Infer(_) => todo!(),
                    Type::Macro(_) => todo!(),
                    Type::Never(_) => todo!(),
                    Type::Paren(_) => todo!(),
                    Type::Path(_) => todo!(),
                    Type::Ptr(_) => todo!(),
                    Type::Reference(_) => todo!(),
                    Type::Slice(slice) => {
                        let inner_type = slice.elem.as_ref();
                        let offset_name = format_ident!("{}_off", name);
                        let length_name = format_ident!("{}_length", name);
                        most_direct.push(
                            name.clone(),
                            quote! { impl core::iter::ExactSizeIterator<Item = #inner_type> },
                            quote! {
                                let #length_name = #name.len();
                                // Validate the layout data
                                {
                                    assert!(isize::try_from(#length_name).is_ok(), "Failed to fit length into isize");

                                    let size: usize = usize::from(#length_name).checked_mul(core::mem::size_of::<#inner_type>())
                                    .expect("Overflowed usize with the number of elements");
                                    
                                    assert!(isize::try_from(size).is_ok(), "Overflowed isize with the number of elements. Array allocation must be below `isize::MAX` bytes");
                                };
                                let (#layout_name, #offset_name) = #layout_name.extend(std::alloc::Layout::array::<#inner_type>(#length_name)?)?;
                            },
                            quote! {
                                {
                                    
                                };
                            },
                        );
                    }
                    Type::TraitObject(_) => todo!("Trait object is not yet supported"),
                    Type::Tuple(_) => todo!(),
                    Type::Verbatim(_) => todo!(),
                    _ => todo!(),
                }
            } else {
                // There is no dst
                let offset_name = format_ident!("{}_off", name);
                most_direct.push(
                    name.clone(),
                    quote! { #ty },
                    quote! {
                        let (#layout_name, #offset_name) = #layout_name.extend(std::alloc::Layout::new::<#ty>())?;
                        assert!(isize::try_from(#offset_name).is_ok(), "Offset would overflow isize");
                    },
                    quote! {
                        {
                            // Safety:
                            // - offset was given by layout and thus should be in the bounds of the 
                            // allocation
                            // - offset was asserted not to overflow an isize earlier
                            let val_ptr: *mut u8 = unsafe { #ptr_name.add(#offset_name) };
                            let val_ptr: *mut #ty = val_ptr.cast::<#ty>();
                            unsafe { std::ptr::write(val_ptr, #name) };
                        };
                    },
                );
            }
        }
        // TODO: Provide option to not use std, and use alloc directly
        let Output {
            parameter_names,
            parameter_types,
            initial_setup,
            store,
        } = most_direct;
        let new_with = quote! {
            pub fn new_with(
                #(#parameter_names : #parameter_types),*
            ) -> Result<#main_structure_name, core::alloc::LayoutError> {
                let #layout_name = std::alloc::Layout::from_size_align(0, 1)?;

                #(
                    #initial_setup
                )*

                // TODO: Should we do this? This is needed to make it more C-structure like
                let #layout_name = #layout_name.pad_to_align();

                // Safety: The layout we were given is valid
                let #ptr_name: *mut u8 = unsafe { std::alloc::alloc(layout) };
                // TODO: Don't panic
                assert!(!#ptr_name.is_null(), "Failed to allocate");

                #(
                    #store
                )*

                // We know it is already non-null
                let #ptr_name: core::ptr::NonNull<u8> = core::ptr::NonNull::new(#ptr_name).unwrap();

                Ok(#main_structure_name {
                    ptr: #ptr_name,
                })
            }
        };

        Ok(quote! {
            #new_with
        })
    }
}

// TODO: Darling produces bad errors for this. Can we improve that?
// TODO: Darling does a collection even though we don't need that. That probably isn't really that
// notable in perf, but it is annoying.
#[derive(Debug, Copy, Clone, Eq, PartialEq, FromMeta)]
enum IntegerType {
    // TODO: u128?
    #[darling(rename = "usize")]
    USize,
    U64,
    U32,
    U16,
    U8,
}
impl IntegerType {
    fn usize() -> IntegerType {
        IntegerType::USize
    }
}

// TODO: Support doc comments!
#[derive(Debug, FromMeta)]
struct DstAttribute {
    #[darling(default)]
    length: LengthData,
    #[darling(default = "default_default_value")]
    default_value: String,
}

fn default_default_value() -> String {
    "Default::default".to_string()
}

#[derive(Debug, FromMeta)]
struct LengthData {
    #[darling(default = "IntegerType::usize")]
    index: IntegerType,
    #[darling(default = "DstLengthPositioning::before_all_dst")]
    pos: DstLengthPositioning,
}
impl Default for LengthData {
    fn default() -> LengthData {
        LengthData {
            index: IntegerType::USize,
            pos: DstLengthPositioning::BeforeAllDst,
        }
    }
}

/// How to position the length field
#[derive(Debug, FromMeta)]
enum DstLengthPositioning {
    /// Put it before all dynamically sized types, this reduces the amount
    /// of runtime computation needed
    #[darling(rename = "before-all-dst")]
    BeforeAllDst,
    /// Put it before the dst the length is for, but it might be after other dsts
    #[darling(rename = "before-dst")]
    BeforeDst,
}
impl DstLengthPositioning {
    fn before_all_dst() -> Self {
        Self::BeforeAllDst
    }
}

fn is_ty_fancy_dst(ty: &Type) -> Option<bool> {
    Some(match ty {
        // Arrays can't be a dynamically sized type
        // though maybe we could do some weird things but that would require an attribute
        Type::Array(_) => false,
        // Functions aren't dsts
        Type::BareFn(_) => false,
        Type::Group(_) => todo!(),
        // Impl trait is typically not a dst
        Type::ImplTrait(_) => false,
        // We don't infer type
        Type::Infer(_) => false,
        // We have no clue
        Type::Macro(_) => return None,
        Type::Never(_) => false,
        Type::Paren(p) => return is_ty_fancy_dst(p.elem.as_ref()),
        // Can't determine
        Type::Path(_) => false,
        Type::Ptr(_) => false,
        Type::Reference(_) => false,
        Type::Slice(_) => true,
        Type::TraitObject(_) => true,
        // Technically depends on the last parameter but we don't support them currently
        Type::Tuple(_) => false,
        Type::Verbatim(_) => false,
        _ => false,
    })
}
