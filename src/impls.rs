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
    const INFO: Type = Sized(Sequence(Array(ArrayTy {
        ty: &T::INFO,
        size: N,
    })), SizeInfo::new::<[T; N]>());
}

macro_rules! indirect_impl {
    ($ind:ident, $indty:ty) => {
        impl<T: TypeInfo> TypeInfo for $indty {
            const INFO: Type = Sized(Pointer(IndirectTy {
                indirection: Indirection::$ind,
                target: type_info::<T>,
                // HACK: avoid recursively going into T::INFO
                sized: core::mem::size_of::<&T>() == core::mem::size_of::<&()>(),
            }), SizeInfo::new::<$indty>());
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

macro_rules! parse_visibility {
    () => {
        false
    };
    ($tt:tt) => {
        true
    };
}

/// Matches C, Rust, transparent, u*, i* and converts it to correct BaseRepr instance
macro_rules! parse_base_repr {
    ($repr:expr$(,)?) => { $repr };
    ($repr:expr, C $($tail:tt)*) => { BaseRepr::C };
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
    ($repr:expr, $skip:expr, $($tail:tt)*) => { parse_base_repr!($repr, $($tail)*) };
    ($repr:expr, $skip:expr) => { parse_base_repr!($repr) };
}

/// Matches packed within the #[repr(...)] instantiation and returns true if it is found
macro_rules! parse_repr_packed {
    ($packed:expr$(,)?) => { $packed };
    ($packed:expr, packed $($tail:tt)*) => { true };
    ($packed:expr, $skip:expr, $($tail:tt)*) => { parse_repr_packed!($packed, $($tail)*) };
    ($packed:expr, $skip:expr) => { parse_repr_packed!($packed) };
}

/// Matches align(N) within the $[repr(...)] instantiation and returns Some(N) if it was found
macro_rules! parse_repr_align {
    ($align:expr$(,)?) => { $align };
    ($align:expr, align($new_align:expr) $($tail:tt)*) => { Some($new_align) };
    ($align:expr, $skip:expr, $($tail:tt)*) => { parse_repr_align!($align, $($tail)*) };
    ($align:expr, $skip:expr) => { parse_repr_align!($align) };
}

/// Parses struct repr from a number of #[meta] arguments
macro_rules! parse_repr {
    (, $packed:expr, $align:expr, $repr:expr) => {
        Repr {
            base: $repr,
            packed: $packed,
            align: $align,
        }
    };
    (#[repr($($tokens:tt)*)] $(#[$($othermeta:tt)*])*, $packed:expr, $align:expr, $repr:expr) => {
        parse_repr!($(#[$($othermeta)*])*, parse_repr_packed!($packed, $($tokens)*), parse_repr_align!($align, $($tokens)*), parse_base_repr!($repr, $($tokens)*))
    };
    (#[$meta:meta] $(#[$($othermeta:tt)*])*, $packed:expr, $align:expr, $repr:expr) => {
        parse_repr!($(#[$($othermeta)*])*, $packed, $align, $repr)
    };
}

#[macro_export]
/// Implement TypeInfo for a struct definition.
macro_rules! impl_struct {
    ($(#[$($meta:tt)*])* $tyvis:vis struct $tyname:ident$(<$($pty:ident),*>)? { $($vis:vis $field:ident: $fty:ty),*$(,)? }) => {
        $tyvis struct $tyname $(<$($pty),*>)? {
            $($vis $field: $fty,)*
        }

        // TODO: emit Unsized if struct is DST.
        impl $(<$($pty: TypeInfo),*>)? TypeInfo for $tyname$(<$($pty),*>)? {
            const INFO: Type = Sized(User(Struct(StructTy {
                name: stringify!($tyname),
                repr: parse_repr!($(#[$($meta)*])*, false, None, BaseRepr::Rust),
                fields: &[
                    $(NamedField {
                        name: stringify!($field),
                        inner: Field {
                            public: parse_visibility!($vis),
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
    impl_struct! {
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
            Sized(User(Struct(StructTy {
                repr: Repr {
                    base: BaseRepr::Rust,
                    packed: false,
                    align: None
                },
                fields: &[_, _, _],
                name: "SampleStruct",
            })), _)
        );
    }

    impl_struct! {
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
            Sized(User(Struct(StructTy {
                repr: Repr {
                    base: BaseRepr::C,
                    packed: false,
                    align: None
                },
                fields: &[NamedField {
                    name: "r",
                    inner: Field {
                        public: true,
                        ty: Sized(Pointer(IndirectTy {
                            indirection: Indirection::Ref,
                            target: SELFREF_FN,
                            sized: true
                        }), _)
                    }
                }],
                name: "SelfReferential",
            })), _)
        );
    }

    impl_struct! {
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
                fields: &[
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
                ],
                name: "Generic",
            })), _)
        );
    }

    #[test]
    fn generic() {
        generic_inner::<u8, u64>();
        generic_inner::<(u8, u8, f64), u64>();
    }
}
