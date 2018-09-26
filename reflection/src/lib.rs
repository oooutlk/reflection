//! This library produces type schema information,
//! including field names and type names in hierarchical [tree structure](https://docs.rs/trees), which reflects the type definitions.
//!
//! To avoid circular type definition, the pointer types( raw/smart pointers, references etc ) will be treated as terminal types,
//! unless using `expand()` to get the referenced type's `schemata()`.
//!
//! # Example
//!
//! See [`fn serde_issue_345()`](https://github.com/oooutlk/reflection/blob/master/reflection_test/src/lib.rs#L151) for generating pretty print format from `schemata()`.

extern crate trees;
use trees::{tr,Tree,Forest,Node};

use std::fmt;
use std::fmt::{Display,Formatter};

pub type Id = &'static str;
pub type Name = Option<String>;

#[derive( Copy, Clone, Debug, PartialEq, Eq, Ord )]
/// Type constructs.
pub enum Type {
    Unknown,
    Struct, Enum,
    Bool, I8, U8, I16, U16, I32, U32, I64, U64, I128, U128, F32, F64,
    Range,
    RefStr, String,
    Array, Tuple, Vec, 
    CPtr, Ptr, NonNull, Ref, RefMut, Box, Rc,
    Option, Result,
    BTreeSet, HashSet,
    BTreeMap, HashMap,
}

impl Type {
    /// Returns `true` for the generalized pointer types such as raw/smart pointers and references, otherwise returns `false`.
    pub fn is_pointer( &self ) -> bool { *self >= Type::CPtr && *self <= Type::Rc }
}

use std::cmp::Ordering;
use std::cmp::Ordering::*;

impl PartialOrd for Type {
    fn partial_cmp( &self, other: &Self ) -> Option<Ordering> {
        let ( a, b ) = ( *self as usize, *other as usize );
        if a < b {
            Some( Less )
        } else if a > b {
            Some( Greater )
        } else {
            Some( Equal )
        }
    }
}

impl Display for Type { fn fmt( &self, f: &mut Formatter ) -> fmt::Result { write!( f, "{}", TYPE_STR[ *self as usize ])}}

const TYPE_STR: [Id;35] = [
    "?",
    "struct", "enum",
    "bool", "i8", "u8", "i16", "u16", "i32", "u32", "i64", "u64", "i128", "u128", "f32", "f64",
    "Range",
    "&str", "String",
    "[]", "()", "Vec", 
    "*const", "*mut", "NonNull", "&", "&mut", "Box", "Rc",
    "Option", "Result",
    "BTreeSet", "HashSet",
    "BTreeMap", "HashMap",
];

/// To get the members of some type.
pub type Expander = Option<fn() -> Schemas>;

/// A type definition, or a field definition of some struct.
#[derive( Clone, Debug, PartialEq, Eq )]
pub struct Field {
    pub id       : Id,
    pub ty       : Type,
    pub tyname   : Name,
    pub expander : Expander,
}

impl Field {
    pub fn new( id: Id, ty: Type, tyname: Name, expander: Expander ) -> Self {
        Field { id, ty, tyname, expander }
    }
}

/// A variant definition of some enum.
#[derive( Clone, Debug, Eq, PartialEq )]
pub struct Variant {
    pub id     : Id,
    pub tyname : Name,
}

/// The type of schema tree node.
#[derive( Clone, Debug, PartialEq, Eq )]
pub enum Member {
    Field( Field ),
    Variant( Variant ),
}

impl Member {
    pub fn id( &self ) -> Id {
        match *self {
            Member::Field( ref field ) => field.id,
            Member::Variant( ref variant ) => variant.id,
        }
    }
}

/// Defines a `Field` as a tree node.
pub fn field( id: Id, ty: Type, tyname: Name, expander: Expander ) -> Schema { tr( Member::Field( Field::new( id, ty, tyname, expander )))}

/// Defines a `Variant` as a tree node.
pub fn variant( id: Id ) -> Schema { tr( Member::Variant( Variant{ id, tyname: None }))}

/// Defines a `Field` as a tree node, which should have no child node.
pub fn terminal( id: Id, ty: Type ) -> Schema {
    tr( Member::Field( Field::new( id, ty,
            Some( String::from( TYPE_STR[ ty as usize ])), None )))
}

impl Display for Member {
    fn fmt( &self, f: &mut Formatter ) -> fmt::Result {
        match *self {
            Member::Field( ref field ) =>
                write!( f, "{}:{}", field.id, &field.tyname.clone().unwrap_or_default() ),
            Member::Variant( ref variant ) => 
                write!( f, "{}|", variant.id ),
        }
    }
}

pub type Schema  = Tree  <Member>;
pub type Schemas = Forest<Member>;

/// Reflects type's fields' names and their type names.
pub trait Reflection {
    /// Returns type construct.
    fn ty() -> Type { Type::Unknown }

    /// Returns type name.
    fn name() -> Name { Some( String::from( TYPE_STR[ Self::ty() as usize ]))}

    /// Reflects field name, its type name and its members, by a tree of degree 1.
    fn schema( id: Id ) -> Schema;

    /// Reflects members' schema() by a forest of degree 1.
    fn members() -> Schemas { Schemas::new() }

    /// Reflects type's fields' names and their type names, by expanding `schema()` recursively.
    fn schemata() -> Schema {
        let mut schema = Self::schema( "_" );
        expand( &mut schema );
        schema
    }
}

/// Expands `schema()` recursively, stopping at fields of primitives or pointers.
#[allow( unused_must_use )]
pub fn expand( node: &mut Node<Member> ) {
    expand_field( node ) || expand_variant( node );
}

fn expand_field( node: &mut Node<Member> ) -> bool {
    let mut expander: Expander = None;
    if let Member::Field( ref field ) = node.data {
        if node.is_leaf() && !field.ty.is_pointer() {
            expander = field.expander;
        }
    }
    expander.map( |expander| {
        node.append( expander() );
        for mut child in node.iter_mut() {
            expand( &mut child );
        }
        true
    }).unwrap_or( false ) 
}

fn expand_variant( node: &mut Node<Member> ) -> bool {
    if let Member::Variant(_) = &mut node.data {
        for mut child in node.iter_mut() {
            expand_field( &mut child );
        }
        true
    } else {
        false
    }
}

macro_rules! ty {
    ($t:ty) => { <$t as Reflection>::ty() }
}

macro_rules! name {
    ($ty:ty) => { <$ty as Reflection>::name() }
}

macro_rules! name_ {
    ($ty:ty) => { <$ty as Reflection>::name().unwrap_or( String::from( "_" )) }
}

macro_rules! expander {
    ($ty:ty) => { Some( <$ty as Reflection>::members )}
}

impl Reflection for bool { fn ty() -> Type { Type::Bool } fn schema( id: Id ) -> Schema { terminal( id, Type::Bool )}}
impl Reflection for i8   { fn ty() -> Type { Type::I8   } fn schema( id: Id ) -> Schema { terminal( id, Type::I8   )}}
impl Reflection for u8   { fn ty() -> Type { Type::U8   } fn schema( id: Id ) -> Schema { terminal( id, Type::U8   )}}
impl Reflection for i16  { fn ty() -> Type { Type::I16  } fn schema( id: Id ) -> Schema { terminal( id, Type::I16  )}}
impl Reflection for u16  { fn ty() -> Type { Type::U16  } fn schema( id: Id ) -> Schema { terminal( id, Type::U16  )}}
impl Reflection for i32  { fn ty() -> Type { Type::I32  } fn schema( id: Id ) -> Schema { terminal( id, Type::I32  )}}
impl Reflection for u32  { fn ty() -> Type { Type::U32  } fn schema( id: Id ) -> Schema { terminal( id, Type::U32  )}}
impl Reflection for i64  { fn ty() -> Type { Type::I64  } fn schema( id: Id ) -> Schema { terminal( id, Type::I64  )}}
impl Reflection for u64  { fn ty() -> Type { Type::U64  } fn schema( id: Id ) -> Schema { terminal( id, Type::U64  )}}
impl Reflection for i128 { fn ty() -> Type { Type::I128 } fn schema( id: Id ) -> Schema { terminal( id, Type::I128 )}}
impl Reflection for u128 { fn ty() -> Type { Type::U128 } fn schema( id: Id ) -> Schema { terminal( id, Type::U128 )}}
impl Reflection for f32  { fn ty() -> Type { Type::F32  } fn schema( id: Id ) -> Schema { terminal( id, Type::F32  )}}
impl Reflection for f64  { fn ty() -> Type { Type::F64  } fn schema( id: Id ) -> Schema { terminal( id, Type::F64  )}}

impl<T:Reflection> Reflection for std::ops::Range<T> {
    fn ty() -> Type { Type::Range }
    fn name() -> Name { Some( format!( "Range<{}>", name_!(T) ))}
    fn schema( id: Id ) -> Schema { field( id, Type::Range, name!(Self), expander!(Self) )}
    fn members() -> Schemas {
        - field( "start", ty!(T), name!(T), expander!(T) )
        - field( "end",   ty!(T), name!(T), expander!(T) )
    }
}

impl<'a> Reflection for &'a str { fn ty() -> Type { Type::RefStr } fn schema( id: Id ) -> Schema { terminal( id, Type::String )}}
impl     Reflection for String  { fn ty() -> Type { Type::String } fn schema( id: Id ) -> Schema { terminal( id, Type::String )}}

impl<T> Reflection for *const T where T: ?Sized + Reflection {
    fn ty() -> Type { Type::CPtr }
    fn name() -> Name { Some( format!( "*const {}", name_!(T) ))}
    fn schema( id: Id ) -> Schema { field( id, Type::CPtr, name!(Self), expander!(Self) )}
    fn members() -> Schemas { - field( "_", ty!(T), name!(T), expander!(T) )}
}

impl<T> Reflection for *mut T where T: ?Sized + Reflection {
    fn ty() -> Type { Type::Ptr }
    fn name() -> Name { Some( format!( "*mut {}", name_!(T) ))}
    fn schema( id: Id ) -> Schema { field( id, Type::Ptr, name!(Self), expander!(Self) )}
    fn members() -> Schemas { - field( "_", ty!(T), name!(T), expander!(T) )}
}

impl<T> Reflection for std::ptr::NonNull<T> where T: ?Sized + Reflection {
    fn ty() -> Type { Type::NonNull }
    fn name() -> Name { Some( format!( "NonNull<{}>", name_!(T) ))}
    fn schema( id: Id ) -> Schema { field( id, Type::NonNull, name!(Self), expander!(Self) )}
    fn members() -> Schemas { - field( "_", ty!(T), name!(T), expander!(T) )}
}

impl<'a,T> Reflection for &'a T where T: 'a + ?Sized + Reflection {
    fn ty() -> Type { Type::Ref }
    fn name() -> Name { Some( format!( "&{}", name_!(T) ))}
    fn schema( id: Id ) -> Schema { field( id, Type::Ref, name!(Self), expander!(Self) )}
    fn members() -> Schemas { - field( "_", ty!(T), name!(T), expander!(T) )}
}

impl<'a,T> Reflection for &'a mut T where T: 'a + ?Sized + Reflection {
    fn ty() -> Type { Type::RefMut }
    fn name() -> Name { Some( format!( "&mut {}", name_!(T) ))}
    fn schema( id: Id ) -> Schema { field( id, Type::RefMut, name!(Self), expander!(Self) )}
    fn members() -> Schemas { - field( "_", ty!(T), name!(T), expander!(T) )}
}

impl<T> Reflection for Box<T> where T: ?Sized + Reflection {
    fn ty() -> Type { Type::Box }
    fn name() -> Name { Some( format!( "Box<{}>", name_!(T) ))}
    fn schema( id: Id ) -> Schema { field( id, Type::Box, name!(Self), expander!(Self) )}
    fn members() -> Schemas { - field( "_", ty!(T), name!(T), expander!(T) )}
}

impl<T> Reflection for std::rc::Rc<T> where T: ?Sized + Reflection {
    fn ty() -> Type { Type::Rc }
    fn name() -> Name { Some( format!( "Rc<{}>", name_!(T) ))}
    fn schema( id: Id ) -> Schema { field( id, Type::Rc, name!(Self), expander!(Self) )}
    fn members() -> Schemas { - field( "_", ty!(T), name!(T), expander!(T) )}
}

impl<T> Reflection for Vec<T> where T: Reflection {
    fn ty() -> Type { Type::Vec }
    fn name() -> Name { Some( format!( "Vec<{}>", name_!(T) ))}
    fn schema( id: Id ) -> Schema { field( id, Type::Vec, name!(Self), expander!(Self) )}
    fn members() -> Schemas { - field( "_", ty!(T), name!(T), expander!(T) )}
}

impl<T> Reflection for Option<T> where T: Reflection {
    fn ty() -> Type { Type::Enum }
    fn name() -> Name { Some( format!( "Option<{}>", name_!(T) ))}
    fn schema( id: Id ) -> Schema { field( id, Type::Option, name!(Self), expander!(Self) )}
    fn members() -> Schemas {
        -( variant( "None" ))
        -( variant( "Some" ) / field( "0", ty!(T), name!(T), expander!(T) ))
    }
}

impl<T,E> Reflection for Result<T,E> where T: Reflection, E: Reflection {
    fn ty() -> Type { Type::Result }
    fn name() -> Name { Some( format!( "Result<{},{}>", name_!(T), name_!(E) ))}
    fn schema( id: Id ) -> Schema { field( id, Type::Result, name!(Self), expander!(Self) )}
    fn members() -> Schemas {
        -( variant( "Ok"  ) / field( "_", ty!(T), name!(T), expander!(T) ))
        -( variant( "Err" ) / field( "_", ty!(E), name!(E), expander!(E) ))
    }
}

impl<T:Reflection> Reflection for std::collections::BTreeSet<T> {
    fn ty() -> Type { Type::BTreeSet }
    fn name() -> Name { Some( format!( "BTreeSet<{}>", name_!(T) ))}
    fn schema( id: Id ) -> Schema { field( id, Type::BTreeSet, name!(Self), expander!(Self) )}
    fn members() -> Schemas { - field( "_", ty!(T), name!(T), expander!(T) )}
}

impl<T:Reflection> Reflection for std::collections::HashSet<T> {
    fn ty() -> Type { Type::HashSet }
    fn name() -> Name { Some( format!( "HashSet<{}>", name_!(T) ))}
    fn schema( id: Id ) -> Schema { field( id, Type::HashSet, name!(Self), expander!(Self) )}
    fn members() -> Schemas { - field( "_", ty!(T), name!(T), expander!(T) )}
}

impl<K,V> Reflection for std::collections::BTreeMap<K,V> where K: Reflection, V: Reflection {
    fn ty() -> Type { Type::BTreeMap }
    fn name() -> Name { Some( format!( "BTreeMap<{},{}>", name_!(K), name_!(V) ))}
    fn schema( id: Id ) -> Schema { field( id, Type::BTreeMap, name!(Self), expander!(Self) )}
    fn members() -> Schemas {
        - field( "name", ty!(K), name!(K), expander!(K) )
        - field( "value", ty!(V), name!(V), expander!(V) )
    }
}

impl<K,V> Reflection for std::collections::HashMap<K,V> where K: Reflection, V: Reflection {
    fn ty() -> Type { Type::HashMap }
    fn name() -> Name { Some( format!( "HashMap<{},{}>", name_!(K), name_!(V) ))}
    fn schema( id: Id ) -> Schema { field( id, Type::HashMap, name!(Self), expander!(Self) )}
    fn members() -> Schemas {
        - field( "name", ty!(K), name!(K), expander!(K) )
        - field( "value", ty!(V), name!(V), expander!(V) )
    }
}

impl<T> Reflection for [T] where T: Reflection {
    fn ty() -> Type { Type::Array }
    fn name() -> Name { Some( format!( "[{}]", name_!(T) ))}
    fn schema( id: Id ) -> Schema { field( id, Type::Array, name!(Self), expander!(Self) )}
    fn members() -> Schemas { - field( "_", ty!(T), name!(T), expander!(T) )}
}

macro_rules! array_impls {
    ($($len:tt)+) => {
        $(
            impl<T> Reflection for [T;$len] where T: Reflection {
                fn ty() -> Type { Type::Array }
                fn name() -> Name { Some( format!( "[{}]", name_!(T) ))}
                fn schema( id: Id ) -> Schema { field( id, Type::Array, name!(Self), expander!(Self) )}
                fn members() -> Schemas { - field( "_", ty!(T), name!(T), expander!(T) )}
            }
        )+
    }
}

array_impls!(
    01 02 03 04 05 06 07 08
    09 10 11 12 13 14 15 16
    17 18 19 20 21 22 23 24
    25 26 27 28 29 30 31 32
);

impl Reflection for () {
    fn ty() -> Type { Type::Tuple }
    fn name() -> Name { Some( format!( "()" ))}
    fn schema( id: Id ) -> Schema { field( id, Type::Tuple, name!(Self), None )}
}

impl<T0> Reflection for (T0,)
    where T0: Reflection,
{
    fn ty() -> Type { Type::Tuple }
    fn name() -> Name { Some( format!( "({},)", name_!(T0) ))}
    fn schema( id: Id ) -> Schema { field( id, Type::Tuple, name!(Self), expander!(Self) )}
    fn members() -> Schemas { - field( "0", ty!(T0), name!(T0), expander!(T0) )}
}

impl<T0,T1> Reflection for (T0,T1)
    where T0: Reflection, T1: Reflection,
{
    fn ty() -> Type { Type::Tuple }
    fn name() -> Name { Some( format!( "({},{})", name_!(T0), name_!(T1) ))}
    fn schema( id: Id ) -> Schema { field( id, Type::Tuple, name!(Self), expander!(Self) )}
    fn members() -> Schemas {
        - field( "0", ty!(T0), name!(T0), expander!(T0) )
        - field( "1", ty!(T1), name!(T1), expander!(T1) )
    }
}

impl<T0,T1,T2> Reflection for (T0,T1,T2)
    where T0: Reflection, T1: Reflection, T2: Reflection
{
    fn ty() -> Type { Type::Tuple }
    fn name() -> Name { Some( format!( "({},{},{})", name_!(T0), name_!(T1), name_!(T2) ))}
    fn schema( id: Id ) -> Schema { field( id, Type::Tuple, name!(Self), expander!(Self) )}
    fn members() -> Schemas {
        - field( "0", ty!(T0), name!(T0), expander!(T0) )
        - field( "1", ty!(T1), name!(T1), expander!(T1) )
        - field( "2", ty!(T2), name!(T2), expander!(T2) )
    }
}

impl<T0,T1,T2,T3> Reflection for (T0,T1,T2,T3)
    where T0: Reflection, T1: Reflection, T2: Reflection, T3: Reflection
{
    fn ty() -> Type { Type::Tuple }
    fn name() -> Name { Some( format!( "({},{},{},{})", name_!(T0), name_!(T1), name_!(T2), name_!(T3) ))}
    fn schema( id: Id ) -> Schema { field( id, Type::Tuple, name!(Self), expander!(Self) )}
    fn members() -> Schemas {
        - field( "0", ty!(T0), name!(T0), expander!(T0) )
        - field( "1", ty!(T1), name!(T1), expander!(T1) )
        - field( "2", ty!(T2), name!(T2), expander!(T2) )
        - field( "3", ty!(T3), name!(T3), expander!(T3) )
    }
}

impl<T0,T1,T2,T3,T4> Reflection for (T0,T1,T2,T3,T4)
    where T0: Reflection, T1: Reflection, T2: Reflection, T3: Reflection, T4: Reflection
{
    fn ty() -> Type { Type::Tuple }
    fn name() -> Name { Some( format!( "({},{},{},{},{})", name_!(T0), name_!(T1), name_!(T2), name_!(T3), name_!(T4) ))}
    fn schema( id: Id ) -> Schema { field( id, Type::Tuple, name!(Self), expander!(Self) )}
    fn members() -> Schemas {
        - field( "0", ty!(T0), name!(T0), expander!(T0) )
        - field( "1", ty!(T1), name!(T1), expander!(T1) )
        - field( "2", ty!(T2), name!(T2), expander!(T2) )
        - field( "3", ty!(T3), name!(T3), expander!(T3) )
        - field( "4", ty!(T4), name!(T4), expander!(T4) )
    }
}

impl<T0,T1,T2,T3,T4,T5> Reflection for (T0,T1,T2,T3,T4,T5)
    where T0: Reflection, T1: Reflection, T2: Reflection, T3: Reflection, T4: Reflection, T5: Reflection
{
    fn ty() -> Type { Type::Tuple }
    fn name() -> Name { Some( format!( "({},{},{},{},{},{})", name_!(T0), name_!(T1), name_!(T2), name_!(T3), name_!(T4), name_!(T5) ))}
    fn schema( id: Id ) -> Schema { field( id, Type::Tuple, name!(Self), expander!(Self) )}
    fn members() -> Schemas {
        - field( "0", ty!(T0), name!(T0), expander!(T0) )
        - field( "1", ty!(T1), name!(T1), expander!(T1) )
        - field( "2", ty!(T2), name!(T2), expander!(T2) )
        - field( "3", ty!(T3), name!(T3), expander!(T3) )
        - field( "4", ty!(T4), name!(T4), expander!(T4) )
        - field( "5", ty!(T5), name!(T5), expander!(T5) )
    }
}
