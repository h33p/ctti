//! Example implementation of hashed type ID
//!
//! This is a sample implementation of `core::any::type_id` built on top of `TypeInfo`. This is
//! freely implementable as additional crate. It doesn't need to live within ctti. It doesn't
//! use cryptographically secure hashing, but should be sufficient in regular cases.

use crate::*;

pub const fn type_id<T: TypeInfo>() -> u64 {
    type_id_of_val(&T::INFO)
}

pub const fn type_id_of_val(info: &Type) -> u64 {
    match info {
        Unsized(ty) => hash_mul(&[1, type_id_of_val_inner(ty)]),
        Sized(ty, inner) => hash_mul(&[
            2,
            type_id_of_val_inner(ty),
            inner.size as u64,
            inner.align as u64,
        ]),
    }
}

pub const fn type_id_of_val_inner(info: &InnerType) -> u64 {
    match info {
        Primitive(ty) => hash_mul(&[
            1,
            match ty {
                Boolean => 1,
                Numeric(ty) => hash_mul(&[
                    2,
                    match ty {
                        Integer(ty) => hash_mul(&[
                            1,
                            match ty {
                                ty => {
                                    hash_mul(&[hash_str(ty.name), ty.signed as u64, ty.size as u64])
                                }
                            },
                        ]),
                        Float(ty) => hash_mul(&[2, ty.size as u64]),
                    },
                ]),
                Textual(ty) => hash_mul(&[
                    3,
                    match ty {
                        Char => 1,
                        Str => 2,
                    },
                ]),
                Never => 4,
            },
        ]),
        Sequence(ty) => hash_mul(&[
            2,
            match ty {
                Tuple(ty) => hash_mul(&[1, {
                    let mut tmp = 0;
                    let mut i = 0;
                    while i < ty.fields.len() {
                        tmp = hash_mul(&[tmp, type_id_of_val(ty.fields[i])]);
                        i += 1;
                    }
                    tmp
                }]),
                Array(ty) => hash_mul(&[2, ty.size as u64, type_id_of_val(ty.ty)]),
                Slice(ty) => hash_mul(&[3, type_id_of_val(ty.ty)]),
            },
        ]),
        User(ty) => hash_mul(&[
            3,
            match ty {
                Struct(ty) => hash_mul(&[
                    1,
                    hash_repr(&ty.repr),
                    // FIXME: name is not sufficient for type uniqueness.
                    // Do we want the type ID represent layout compatibility or
                    // actual implementation identicalness, because tracking the
                    // latter is close to impossible.
                    hash_str(ty.name),
                    match ty.fields {
                        Named(fields) => {
                            hash_mul(&[1, {
                                let mut tmp = 0;
                                let mut i = 0;
                                while i < fields.len() {
                                    // Intentionally do not hash field names, visibility, because they
                                    // do not change the layout.
                                    tmp = hash_mul(&[tmp, type_id_of_val(fields[i].inner.ty)]);
                                    i += 1;
                                }
                                tmp
                            }])
                        }
                        Unnamed(fields) => {
                            hash_mul(&[2, {
                                let mut tmp = 0;
                                let mut i = 0;
                                while i < fields.len() {
                                    // Intentionally do not hash field names, visibility, because they
                                    // do not change the layout.
                                    tmp = hash_mul(&[tmp, type_id_of_val(fields[i].ty)]);
                                    i += 1;
                                }
                                tmp
                            }])
                        }
                    },
                ]),
                Enum(ty) => hash_mul(&[
                    2,
                    hash_repr(&ty.repr),
                    // FIXME: name is not sufficient for type uniqueness.
                    // Do we want the type ID represent layout compatibility or
                    // actual implementation identicalness, because tracking the
                    // latter is close to impossible.
                    hash_str(ty.name),
                    {
                        const fn hash_variants<T: TypeInfo>(variants: &[EnumVariant<T>]) -> u64 {
                            let mut tmp = 0;
                            let mut i = 0;

                            while i < variants.len() {
                                let variant = &variants[i];

                                let mut tmp2 = 0;
                                let mut o = 0;

                                match variant.fields {
                                    Named(fields) => {
                                        while o < fields.len() {
                                            tmp2 = hash_mul(&[
                                                tmp2,
                                                1,
                                                type_id_of_val(fields[o].inner.ty),
                                            ]);
                                            o += 1;
                                        }
                                    }
                                    Unnamed(fields) => {
                                        while o < fields.len() {
                                            tmp2 =
                                                hash_mul(&[tmp2, 2, type_id_of_val(fields[o].ty)]);
                                            o += 1;
                                        }
                                    }
                                }
                                hash_mul(&[tmp, tmp2, hash_str(variant.name)]);
                            }

                            hash_mul(&[type_id::<T>(), tmp])
                        }

                        match ty.variants {
                            // Avoid clashing with I128 definition
                            C(v) => hash_mul(&[1, hash_variants(&v)]),
                            U8(v) => hash_variants(&v),
                            I8(v) => hash_variants(&v),
                            U16(v) => hash_variants(&v),
                            I16(v) => hash_variants(&v),
                            U32(v) => hash_variants(&v),
                            I32(v) => hash_variants(&v),
                            U64(v) => hash_variants(&v),
                            I64(v) => hash_variants(&v),
                            U128(v) => hash_variants(&v),
                            I128(v) => hash_variants(&v),
                            Usize(v) => hash_variants(&v),
                            Isize(v) => hash_variants(&v),
                            Undefined(v) => hash_variants(&v),
                        }
                    },
                ]),
                Union(ty) => hash_mul(&[
                    3,
                    hash_repr(&ty.repr),
                    // FIXME: name is not sufficient for type uniqueness.
                    // Do we want the type ID represent layout compatibility or
                    // actual implementation identicalness, because tracking the
                    // latter is close to impossible.
                    hash_str(ty.name),
                    {
                        let mut tmp = 0;
                        let mut i = 0;
                        while i < ty.fields.len() {
                            // Intentionally do not hash field names, visibility, because they
                            // do not change the layout.
                            tmp = hash_mul(&[tmp, type_id_of_val(ty.fields[i].inner.ty)]);
                            i += 1;
                        }
                        tmp
                    },
                ]),
            },
        ]),
        Function(ty) => hash_mul(&[
            4,
            match ty {
                _ => 0,
            },
        ]),
        Pointer(ty) => hash_mul(&[
            5,
            match &ty.indirection {
                Indirection::Ref => 1,
                Indirection::MutRef => 2,
                Indirection::ConstPtr => 3,
                Indirection::MutPtr => 4,
            },
            ty.sized as u64,
        ]),
    }
}

const fn hash_repr(repr: &Repr) -> u64 {
    hash_mul(&[
        match repr.base {
            BaseRepr::C => 1,
            BaseRepr::Rust => 2,
            BaseRepr::Transparent => 3,
            BaseRepr::EnumSigned(s) => hash_mul(&[4, s as u64]),
            BaseRepr::EnumUnsigned(s) => hash_mul(&[5, s as u64]),
        },
        repr.packed as u64,
        if let Some(align) = repr.align {
            align as u64
        } else {
            0
        },
    ])
}

// Compile time hashing (not secure hashes).

const fn hash(mut x: u64) -> u64 {
    x = (x ^ (x >> 30)).wrapping_mul(0xbf58476d1ce4e5b9u64);
    x = (x ^ (x >> 27)).wrapping_mul(0x94d049bb133111ebu64);
    x ^ (x >> 31)
}

const fn hash_str(s: &str) -> u64 {
    let mut ret = 0u64;

    let mut i = 0;
    let b = s.as_bytes();

    while i < b.len() {
        ret = hash(ret.wrapping_add(b[i] as u64));
        i += 1;
    }

    hash(ret)
}

const fn hash_mul(xs: &[u64]) -> u64 {
    let mut ret = 0u64;

    let mut i = 0;

    while i < xs.len() {
        ret = hash(ret.wrapping_add(xs[i]));
        i += 1;
    }

    ret
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn check_different() {
        assert_ne!(type_id::<u64>(), type_id::<usize>());
        assert_ne!(type_id::<u64>(), type_id::<(u64,)>());
        assert_ne!(type_id::<()>(), type_id::<(u64,)>());
        assert_ne!(type_id::<()>(), type_id::<((),)>());
        assert_ne!(type_id::<((),)>(), type_id::<((), ())>());
    }
}
