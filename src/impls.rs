//! Implementations
//!
//! Most types can be implemented at library level, however, user types (structs, enums, unions)
//! need to have compiler support to emit the type information.

use crate::*;

macro_rules! maybe_tuple_doc {
    (@ $item:item) => {
        #[doc = "This trait is implemented for tuples up to twelve items long."]
        $item
    };
    ($($rest_a:ident)+ @ $item:item) => {
        #[doc(hidden)]
        $item
    };
}

// Borrowed from https://doc.rust-lang.org/src/core/tuple.rs.html
macro_rules! tuple_impls {
    // Stopping criteria (0-nary tuple)
    () => {
        tuple_impls!(@impl);
    };
    // Running criteria (n-ary tuple, with n >= 1)
    ($T:ident $( $U:ident )*) => {
        tuple_impls!($( $U )*);
        tuple_impls!(@impl $T $( $U )*);
    };
    // "Private" internal implementation
    (@impl $( $T:ident )*) => {
        maybe_tuple_doc! {
            $($T)* @
            impl<$($T: TypeInfo),*> TypeInfo for ($($T,)*)
            {
                const INFO: Type = Sized(Sequence(Tuple(TupleTy { fields: &[
                    $(&$T::INFO,)*
                ] })), SizeInfo::new::<($($T,)*)>());
            }
        }
    }
}

tuple_impls!(E D C B A Z Y X W V U T);

impl TypeInfo for str {
    const INFO: Type = Unsized(Primitive(Textual(Str)));
}

impl<T: TypeInfo> TypeInfo for [T] {
    const INFO: Type = Unsized(Sequence(Slice(SliceTy { ty: &T::INFO })));
}

impl<T: TypeInfo, const N: usize> TypeInfo for [T; N] {
    const INFO: Type = Sized(
        Sequence(Array(ArrayTy {
            ty: &T::INFO,
            size: N,
        })),
        SizeInfo::new::<[T; N]>(),
    );
}

macro_rules! indirect_impl {
    ($ind:ident, $indty:ty) => {
        impl<T: TypeInfo> TypeInfo for $indty {
            const INFO: Type = Sized(
                Pointer(IndirectTy {
                    indirection: Indirection::$ind,
                    target: type_info::<T>,
                    // HACK: avoid recursively going into T::INFO
                    sized: core::mem::size_of::<&T>() == core::mem::size_of::<&()>(),
                }),
                SizeInfo::new::<$indty>(),
            );
        }
    };
}

indirect_impl!(Ref, &T);
indirect_impl!(MutRef, &mut T);
indirect_impl!(ConstPtr, *const T);
indirect_impl!(MutPtr, *mut T);

macro_rules! integer_impl {
    ($ty: ty) => {
        impl TypeInfo for $ty {
            const INFO: Type = Sized(Primitive(Numeric(Integer( IntegerTy {
                name: stringify!($ty),
                signed: (1 as $ty).checked_neg().is_some(),
                size: core::mem::size_of::<$ty>(),
            }))), SizeInfo::new::<$ty>());
        }
    };
    ($ty: ty, $($nextty: ty),+) => {
        integer_impl!($ty);
        integer_impl!($($nextty),+);
    }
}

integer_impl!(u8, i8, u16, i16, u32, i32, u64, i64, u128, i128, usize, isize);

macro_rules! float_impl {
    ($ty: ty) => {
        impl TypeInfo for $ty {
            const INFO: Type = Sized(Primitive(Numeric(Float(FloatTy {
                size: core::mem::size_of::<$ty>(),
            }))), SizeInfo::new::<$ty>());
        }
    };
    ($ty: ty, $($nextty: ty),+) => {
        float_impl!($ty);
        float_impl!($($nextty),+);
    }
}

float_impl!(f32, f64);

#[macro_export]
#[doc(hidden)]
macro_rules! __impl_parse_visibility {
    () => {
        false
    };
    ($tt:tt) => {
        true
    };
}

#[macro_export]
#[doc(hidden)]
/// Matches C, Rust, transparent, u*, i* and converts it to correct BaseRepr instance
macro_rules! __impl_parse_base_repr {
    ($repr:expr$(,)?) => { $repr };
    ($repr:expr, C $($tail:tt)*) => { $crate::__impl_parse_base_repr!(BaseRepr::C, $($tail)*) };
    ($repr:expr, Rust $($tail:tt)*) => { BaseRepr::Rust };
    ($repr:expr, transparent $($tail:tt)*) => { BaseRepr::transparent };
    ($repr:expr, u8 $($tail:tt)*) => { BaseRepr::EnumUnsigned(1) };
    ($repr:expr, u16 $($tail:tt)*) => { BaseRepr::EnumUnsigned(2) };
    ($repr:expr, u32 $($tail:tt)*) => { BaseRepr::EnumUnsigned(4) };
    ($repr:expr, u64 $($tail:tt)*) => { BaseRepr::EnumUnsigned(8) };
    ($repr:expr, u128 $($tail:tt)*) => { BaseRepr::EnumUnsigned(16) };
    ($repr:expr, usize $($tail:tt)*) => { BaseRepr::EnumUnsigned(core::mem::size_of::<usize>()) };
    ($repr:expr, i8 $($tail:tt)*) => { BaseRepr::EnumSigned(1) };
    ($repr:expr, i16 $($tail:tt)*) => { BaseRepr::EnumSigned(2) };
    ($repr:expr, i32 $($tail:tt)*) => { BaseRepr::EnumSigned(4) };
    ($repr:expr, i64 $($tail:tt)*) => { BaseRepr::EnumSigned(8) };
    ($repr:expr, i128 $($tail:tt)*) => { BaseRepr::EnumSigned(16) };
    ($repr:expr, isize $($tail:tt)*) => { BaseRepr::EnumSigned(core::mem::size_of::<usize>()) };
    ($repr:expr, $skip:tt $($tail:tt)*) => { $crate::__impl_parse_base_repr!($repr, $($tail)*) };
    ($repr:expr, $skip:expr) => { $crate::__impl_parse_base_repr!($repr) };
}

#[macro_export]
#[doc(hidden)]
/// Matches packed within the #[repr(...)] instantiation and returns true if it is found
macro_rules! __impl_parse_repr_packed {
    ($packed:expr$(,)?) => { $packed };
    ($packed:expr, packed $($tail:tt)*) => { true };
    ($packed:expr, $skip:expr, $($tail:tt)*) => { $crate::__impl_parse_repr_packed!($packed, $($tail)*) };
    ($packed:expr, $skip:expr) => { $crate::__impl_parse_repr_packed!($packed) };
}

#[macro_export]
#[doc(hidden)]
/// Matches align(N) within the $[repr(...)] instantiation and returns Some(N) if it was found
macro_rules! __impl_parse_repr_align {
    ($align:expr$(,)?) => { $align };
    ($align:expr, align($new_align:expr) $($tail:tt)*) => { Some($new_align) };
    ($align:expr, $skip:expr, $($tail:tt)*) => { $crate::__impl_parse_repr_align!($align, $($tail)*) };
    ($align:expr, $skip:expr) => { $crate::__impl_parse_repr_align!($align) };
}

#[macro_export]
#[doc(hidden)]
/// Parses struct repr from a number of #[meta] arguments
macro_rules! __impl_parse_repr {
    (, $packed:expr, $align:expr, $repr:expr) => {
        Repr {
            base: $repr,
            packed: $packed,
            align: $align,
        }
    };
    (#[repr($($tokens:tt)*)] $(#[$($othermeta:tt)*])*, $packed:expr, $align:expr, $repr:expr) => {
        $crate::__impl_parse_repr!($(#[$($othermeta)*])*, $crate::__impl_parse_repr_packed!($packed, $($tokens)*), $crate::__impl_parse_repr_align!($align, $($tokens)*), $crate::__impl_parse_base_repr!($repr, $($tokens)*))
    };
    (#[$meta:meta] $(#[$($othermeta:tt)*])*, $packed:expr, $align:expr, $repr:expr) => {
        $crate::__impl_parse_repr!($(#[$($othermeta)*])*, $packed, $align, $repr)
    };
}

#[macro_export]
#[doc(hidden)]
macro_rules! __impl_block_to_enum {
    (struct, $($tail:tt)*) => { Struct(StructTy $($tail)*) };
    (union, $($tail:tt)*) => { Union(UnionTy $($tail)*) }
}

#[macro_export]
#[doc(hidden)]
macro_rules! __impl_parse_struct_body {
    ({ $($vis:vis $field:ident: $fty:ty),*$(,)? }) => {
        Named(&[
            $(NamedField {
                name: stringify!($field),
                inner: Field {
                    public: $crate::__impl_parse_visibility!($vis),
                    ty: &<$fty>::INFO,
                }
            },)*
        ])
    };
    (( $($vis:vis $fty:ty),*$(,)? )) => {
        Unnamed(&[
            $(Field {
                public: $crate::__impl_parse_visibility!($vis),
                ty: &<$fty>::INFO,
            },)*
        ])
    };
    () => {
        Unnamed(&[])
    };
}

#[macro_export]
#[doc(hidden)]
macro_rules! __impl_parse_enum_variants {
    ( &[$($pre:tt)*]$(,)? ) => { &[$($pre)*] };
    ( &[$($pre:tt)*], $field:ident { $($body:tt)* } = $discrim:literal $($tail:tt)*) => {
        $crate::__impl_parse_enum_variants!(
            &[
                $($pre)*
                EnumVariant {
                    name: stringify!($field),
                    value: Some($discrim),
                    fields: $crate::__impl_parse_struct_body!({ $($body)* }),
                },
            ],
            $($tail)*
        )
    };
    ( &[$($pre:tt)*], $field:ident { $($body:tt)* } $($tail:tt)*) => {
        $crate::__impl_parse_enum_variants!(
            &[
                $($pre)*
                EnumVariant {
                    name: stringify!($field),
                    value: None,
                    fields: $crate::__impl_parse_struct_body!({ $($body)* }),
                },
            ],
            $($tail)*
        )
    };
    ( &[$($pre:tt)*], $field:ident ( $($body:tt)* ) = $discrim:literal $($tail:tt)*) => {
        $crate::__impl_parse_enum_variants!(
            &[
                $($pre)*
                EnumVariant {
                    name: stringify!($field),
                    value: Some($discrim),
                    fields: $crate::__impl_parse_struct_body!(( $($body)* )),
                },
            ],
            $($tail)*
        )
    };
    ( &[$($pre:tt)*], $field:ident ( $($body:tt)* ) $($tail:tt)*) => {
        $crate::__impl_parse_enum_variants!(
            &[
                $($pre)*
                EnumVariant {
                    name: stringify!($field),
                    value: None,
                    fields: $crate::__impl_parse_struct_body!(( $($body)* )),
                },
            ],
            $($tail)*
        )
    };
    ( &[$($pre:tt)*], $field:ident = $discrim:literal $($tail:tt)*) => {
        $crate::__impl_parse_enum_variants!(
            &[
                $($pre)*
                EnumVariant {
                    name: stringify!($field),
                    value: Some($discrim),
                    fields: $crate::__impl_parse_struct_body!(),
                },
            ],
            $($tail)*
        )
    };
    ( &[$($pre:tt)*], $field:ident $(= $discrim:literal)? $($tail:tt)*) => {
        $crate::__impl_parse_enum_variants!(
            &[
                $($pre)*
                EnumVariant {
                    name: stringify!($field),
                    value: None,
                    fields: $crate::__impl_parse_struct_body!(),
                },
            ],
            $($tail)*
        )
    };
    ( &[$($pre:tt)*],, $($tail:tt)*) => { $crate::__impl_parse_enum_variants!(&[$($pre)*], $($tail)*) };
}

#[macro_export]
#[doc(hidden)]
macro_rules! __impl_parse_enum_repr {
    ($retrepr:expr, $value:expr$(,)?) => { $retrepr };
    ($retrepr:expr, $value:expr, C $($tail:tt)*) => { $crate::__impl_parse_enum_repr!(C($value), $value, $($tail)*) };
    ($retrepr:expr, $value:expr, u8 $($tail:tt)*) => { U8($value) };
    ($retrepr:expr, $value:expr, u16 $($tail:tt)*) => { U16($value) };
    ($retrepr:expr, $value:expr, u32 $($tail:tt)*) => { U32($value) };
    ($retrepr:expr, $value:expr, u64 $($tail:tt)*) => { U64($value) };
    ($retrepr:expr, $value:expr, u128 $($tail:tt)*) => { U128($value) };
    ($retrepr:expr, $value:expr, usize $($tail:tt)*) => { Usize($value) };
    ($retrepr:expr, $value:expr, i8 $($tail:tt)*) => { I8($value) };
    ($retrepr:expr, $value:expr, i16 $($tail:tt)*) => { I16($value) };
    ($retrepr:expr, $value:expr, i32 $($tail:tt)*) => { I32($value) };
    ($retrepr:expr, $value:expr, i64 $($tail:tt)*) => { I64($value) };
    ($retrepr:expr, $value:expr, i128 $($tail:tt)*) => { I128($value) };
    ($retrepr:expr, $value:expr, isize $($tail:tt)*) => { Isize($value) };
    ($retrepr:expr, $value:expr, $skip:tt $($tail:tt)*) => { $crate::__impl_parse_enum_repr!($retrepr, $value, $($tail)*) };
}

#[macro_export]
#[doc(hidden)]
macro_rules! __impl_enum_variant_from_repr {
    (, $body:expr) => { Undefined($body) };
    (#[repr($($tokens:tt)*)] $(#[$($othermeta:tt)*])*, $body:expr) => {
        $crate::__impl_parse_enum_repr!(Undefined($body), $body, $($tokens)*)
    };
    (#[$meta:meta] $(#[$($othermeta:tt)*])*, $body:expr) => {
        $crate::__impl_enum_variant_from_repr!($(#[$($othermeta:tt)*])*, $body)
    };
}

#[macro_export]
/// Implement TypeInfo for a struct/enum/union definition.
macro_rules! impl_block {
    ($(#[$($meta:tt)*])* $tyvis:vis enum $tyname:ident$(<$($pty:ident),*>)? { $($body:tt)* }) => {
        $(#[$($meta)*])*
        $tyvis enum $tyname $(<$($pty),*>)? {
            $($body)*
        }

        impl $(<$($pty: TypeInfo),*>)? TypeInfo for $tyname$(<$($pty),*>)? {
            const INFO: Type = Sized(User(Enum(EnumTy {
                name: stringify!($tyname),
                repr: $crate::__impl_parse_repr!($(#[$($meta)*])*, false, None, BaseRepr::Rust),
                variants: $crate::__impl_enum_variant_from_repr!($(#[$($meta)*])*, $crate::__impl_parse_enum_variants!(&[], $($body)*)),
            })), SizeInfo::new::<$tyname$(<$($pty),*>)?>());
        }
    };
    ($(#[$($meta:tt)*])* $tyvis:vis struct $tyname:ident<$($pty:ident),*> $body:tt $($semi:tt)?) => {
        $(#[$($meta)*])*
        $tyvis struct $tyname <$($pty),*> $body $($semi)?

        // TODO: emit Unsized if struct is DST.
        impl <$($pty: TypeInfo),*> TypeInfo for $tyname<$($pty),*> {
            const INFO: Type = Sized(User($crate::__impl_block_to_enum!(struct, {
                name: stringify!($tyname),
                repr: $crate::__impl_parse_repr!($(#[$($meta)*])*, false, None, BaseRepr::Rust),
                fields: __impl_parse_struct_body!($body),
            })), SizeInfo::new::<$tyname <$($pty),*>>());
        }
    };
    ($(#[$($meta:tt)*])* $tyvis:vis struct $tyname:ident;) => {
        $crate::impl_block!($(#[$($meta)*])* $tyvis struct $tyname <> (); );
    };
    ($(#[$($meta:tt)*])* $tyvis:vis struct $tyname:ident $body:tt $($semi:tt)?) => {
        $crate::impl_block!($(#[$($meta)*])* $tyvis struct $tyname <> $body $($semi)?);
    };
    ($(#[$($meta:tt)*])* $tyvis:vis union $tyname:ident$(<$($pty:ident),*>)? { $($vis:vis $field:ident: $fty:ty),*$(,)? }) => {
        $(#[$($meta)*])*
        $tyvis union $tyname $(<$($pty),*>)? {
            $($vis $field: $fty,)*
        }

        // TODO: emit Unsized if struct is DST.
        impl $(<$($pty: TypeInfo),*>)? TypeInfo for $tyname$(<$($pty),*>)? {
            const INFO: Type = Sized(User(Struct(StructTy {
                name: stringify!($tyname),
                repr: $crate::__impl_parse_repr!($(#[$($meta)*])*, false, None, BaseRepr::Rust),
                fields: &[
                    $(NamedField {
                        name: stringify!($field),
                        inner: Field {
                            public: $crate::__impl_parse_visibility!($vis),
                            ty: &<$fty>::INFO,
                        }
                    },)*
                ],
            })), SizeInfo::new::<$tyname$(<$($pty),*>)?>());
        }
    }
}

#[cfg(test)]
mod test {
    use crate::*;

    macro_rules! assert_layout {
        ($ty:ty, $(|)? $($pattern:pat_param)|+ $(if $guard:expr)? $(,)?) => {
            match <$ty>::INFO {
                $( $pattern )|+ $( if $guard )? => {}
                _ => panic!("{} invalid.\nExpected: {}\nActual: {:?}", stringify!($ty), stringify!($( $pattern )|+ $( if $guard )?), <$ty>::INFO),
            }
        };
    }

    // Sample implementations for a regular struct
    impl_block! {
        pub struct SampleStruct {
            pub first: u8,
            pub second: (u64, u32),
            pub third: *const ()
        }
    }

    #[test]
    fn sample_struct() {
        assert_layout!(
            SampleStruct,
            Sized(
                User(Struct(StructTy {
                    repr: Repr {
                        base: BaseRepr::Rust,
                        packed: false,
                        align: None
                    },
                    fields: Named(&[_, _, _]),
                    name: "SampleStruct",
                })),
                _
            )
        );
    }

    impl_block! {
        #[repr(C)]
        /// Hiii
        pub struct SelfReferential {
            pub r: &'static SelfReferential
        }
    }

    #[test]
    fn self_referential() {
        const SELFREF_FN: extern "C" fn() -> &'static Type = type_info::<SelfReferential>;
        assert_layout!(
            SelfReferential,
            Sized(
                User(Struct(StructTy {
                    repr: Repr {
                        base: BaseRepr::C,
                        packed: false,
                        align: None
                    },
                    fields: Named(&[NamedField {
                        name: "r",
                        inner: Field {
                            public: true,
                            ty: Sized(
                                Pointer(IndirectTy {
                                    indirection: Indirection::Ref,
                                    target: SELFREF_FN,
                                    sized: true
                                }),
                                _
                            )
                        }
                    }]),
                    name: "SelfReferential",
                })),
                _
            )
        );
    }

    impl_block! {
        #[repr(C, packed)]
        pub struct Generic<A, B> {
            pub first: A,
            pub second: (B, A, A, B),
        }
    }

    fn generic_inner<A: TypeInfo, B: TypeInfo>() {
        assert_layout!(
            Generic<A, B>,
            Sized(User(Struct(StructTy {
                repr: Repr {
                    base: BaseRepr::C,
                    packed: true,
                    align: None
                },
                fields: Named(&[
                    NamedField {
                        name: "first",
                        inner: Field {
                            public: true,
                            ty: _,
                        }
                    },
                    NamedField {
                        name: "second",
                        inner: Field {
                            public: true,
                            ty: _,
                        }
                    },
                ]),
                name: "Generic",
            })), _)
        );
    }

    #[test]
    fn generic() {
        generic_inner::<u8, u64>();
        generic_inner::<(u8, u8, f64), u64>();
    }

    impl_block! {
        #[repr(i8)]
        pub enum SimpleEnum {
            A,
            B,
            C
        }
    }

    #[test]
    fn enum_simple() {
        assert_layout!(
            SimpleEnum,
            Sized(
                User(Enum(EnumTy {
                    name: "SimpleEnum",
                    variants: I8(&[
                        EnumVariant {
                            name: "A",
                            value: None,
                            fields: Unnamed(&[])
                        },
                        EnumVariant {
                            name: "B",
                            value: None,
                            fields: Unnamed(&[])
                        },
                        EnumVariant {
                            name: "C",
                            value: None,
                            fields: Unnamed(&[])
                        },
                    ]),
                    repr: Repr {
                        packed: false,
                        align: None,
                        base: BaseRepr::EnumSigned(1),
                    }
                })),
                _
            )
        );
    }

    impl_block! {
        #[repr(C)]
        pub enum SimpleEnum2 {
            A,
            B,
            C
        }
    }

    #[test]
    fn enum_simple2() {
        assert_layout!(
            SimpleEnum2,
            Sized(
                User(Enum(EnumTy {
                    name: "SimpleEnum2",
                    variants: C(&[
                        EnumVariant {
                            name: "A",
                            value: None,
                            fields: Unnamed(&[])
                        },
                        EnumVariant {
                            name: "B",
                            value: None,
                            fields: Unnamed(&[])
                        },
                        EnumVariant {
                            name: "C",
                            value: None,
                            fields: Unnamed(&[])
                        },
                    ]),
                    repr: Repr {
                        packed: false,
                        align: None,
                        base: BaseRepr::C,
                    }
                })),
                _
            )
        );
    }

    impl_block! {
        #[repr(i8)]
        pub enum SimpleEnum3 {
            A = 4,
            B = -1,
            C
        }
    }
}
