//! # Compile-Time Type Information
//!
//! This crate is an experimental standard library side implementation of potential ctti language feature.
//!
//! The idea is to provide a function `const fn type_info<T: ?Sized>() -> &'static Type` which
//! encodes the underlying type. Other crates can then build on top of this to provide advanced
//! features, such as:
//!
//! 1. Stable `TypeId`.
//! 2. Safe dynamic linking by comparing 2 `Type`s across libraries.
//! 3. Reflection.
//!
//! None of the above need to be implemented by the standard library. Everything can be built on
//! top of compile-time type information.
//!
//! **Why not just this crate?**
//!
//! To be useful, the type information should be available to all types. Implementing it on
//! primitives is simple, however, user types (structs, enums, unions) are impossible without
//! either:
//!
//! 1. Wrapping every type with a macro.
//! 2. Compiler support.
//!
//! 1 is the current state of many crates, such as `abi_stable`, `safer_ffi`. See
//!   [`impls`](crate::impls) module with example of how we do it. It works, however, it is
//!   incredibly invasive - production code requires heavy modification to the crate, and
//!   downstream crates cannot be used by them for the purposes of type layout checks.
//! 
//! 2 becomes the only feasible solution. Depending on how advanced it is, it can also nicely
//!   handle things like self-referential structures without requiring runtime processing.

pub mod type_id;
pub mod impls;

// Import enum fields to make this enum-heavy code look better

pub mod prelude {
    use super::*;
    pub use NumericTy::*;
    pub use PrimitiveTy::*;
    pub use SequenceTy::*;
    pub use TextualTy::*;
    pub use InnerType::*;
    pub use UserTy::*;
    pub use Type::*;
}

use prelude::*;

pub trait TypeInfo {
    /// Compile-time type information
    const INFO: Type;
}

const extern "C" fn type_info<T: TypeInfo>() -> &'static Type {
    &T::INFO
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct SizeInfo {
    pub size: usize,
    pub align: usize,
}

impl SizeInfo {
    const fn new<T>() -> Self {
        Self {
            size: core::mem::size_of::<T>(),
            align: core::mem::align_of::<T>(),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Type {
    Sized(InnerType, SizeInfo),
    Unsized(InnerType),
}

impl Type {
    
    pub const fn is_sized(&self) -> bool {
        match self {
            Unsized(_) => false,
            Sized(_, _) => true,
            /*Primitive(Textual(Str)) => false,
            Sequence(Slice(_)) => false,
            User(Struct(st)) => {
                let fields = st.fields.len();
                fields == 0 || st.fields[fields - 1].inner.ty.is_sized()
            }
            _ => true,*/
        }
    }
}

// Enum definitions

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum InnerType {
    Primitive(PrimitiveTy),
    Sequence(SequenceTy),
    User(UserTy),
    Function(FunctionTy),
    Pointer(IndirectTy),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PrimitiveTy {
    Boolean,
    Numeric(NumericTy),
    Textual(TextualTy),
    Never,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum NumericTy {
    Integer(IntegerTy),
    Float(FloatTy),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TextualTy {
    Char,
    Str,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SequenceTy {
    Tuple(TupleTy),
    Array(ArrayTy),
    Slice(SliceTy),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum UserTy {
    Struct(StructTy),
    Enum(EnumTy),
    Union(UnionTy),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FunctionTy {}

// Underlying type structures

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Repr {
    pub base: BaseRepr,
    pub packed: bool,
    pub align: Option<usize>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BaseRepr {
    C,
    Rust,
    Transparent,
    EnumSigned(usize),
    EnumUnsigned(usize),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct StructTy {
    pub repr: Repr,
    pub name: &'static str,
    pub fields: &'static [NamedField],
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct UnionTy {
    pub repr: Repr,
    pub name: &'static str,
    pub fields: &'static [NamedField],
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct TupleTy {
    pub fields: &'static [&'static Type],
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct EnumTy {
    pub repr: Repr,
    pub name: &'static str,
    pub variants: &'static [EnumVariant],
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct IndirectTy {
    pub indirection: Indirection,
    // We must use a function to avoid infinite recursion
    // in self referential structures.
    pub target: extern "C" fn() -> &'static Type,
    pub sized: bool,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct SliceTy {
    pub ty: &'static Type,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ArrayTy {
    pub ty: &'static Type,
    pub size: usize,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct IntegerTy {
    pub name: &'static str,
    pub signed: bool,
    pub size: usize,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct FloatTy {
    pub size: usize,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct EnumVariant {
    pub name: &'static str,
    pub fields: &'static [Field],
}

// We can not hold the underlying type, because
// self-referential types lead to infinite recursion.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Indirection {
    Ref,
    MutRef,
    ConstPtr,
    MutPtr,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Field {
    pub public: bool,
    pub ty: &'static Type,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct NamedField {
    pub name: &'static str,
    pub inner: Field,
}

