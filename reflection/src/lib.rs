extern crate trees;
use trees::{tr,Tree,Forest};

use std::fmt;
use std::fmt::{Display,Formatter};

#[derive( Copy, Clone, Debug )]
pub enum Name {
    Field  ( &'static str ),
    Variant( &'static str ),
}

impl Display for Name {
    fn fmt( &self, f: &mut Formatter ) -> fmt::Result {
        match *self {
            Name::Field  ( name ) => write!( f, "{}", name ),
            Name::Variant( name ) => {
                write!( f, "{}", name )?;
                write!( f, ":" )
            },
        }
    }
}

pub type Names = Forest<Name>;

pub fn field  ( name: &'static str ) -> Tree<Name> { tr( Name::Field  ( name ))}
pub fn variant( name: &'static str ) -> Tree<Name> { tr( Name::Variant( name ))}

pub trait Reflection {
    fn names() -> Names;
}

macro_rules! dummy_impl {
    ( $($ty:ident),* ) => {
        $(
            impl Reflection for $ty {
                fn names() -> Names {
                    Names::new()
                }
            }
        )*
    }
}

dummy_impl!( bool,i8,u8,i16,u16,i32,u32,i64,u64,f32,f64,String );

impl Reflection for () { fn names() -> Names { Names::new() }}

impl<T0> Reflection for (T0,)
    where T0: Reflection,
{
    fn names() -> Names {
        - ( field("0") / <T0 as Reflection>::names() )
    }
}

impl<T0,T1> Reflection for (T0,T1)
    where T0: Reflection, T1: Reflection,
{
    fn names() -> Names {
        - ( field("0") / <T0 as Reflection>::names() )
        - ( field("1") / <T1 as Reflection>::names() )
    }
}

impl<T0,T1,T2> Reflection for (T0,T1,T2)
    where T0: Reflection, T1: Reflection, T2: Reflection
{
    fn names() -> Names {
        - ( field("0") / <T0 as Reflection>::names() )
        - ( field("1") / <T1 as Reflection>::names() )
        - ( field("2") / <T2 as Reflection>::names() )
    }
}

impl<T0,T1,T2,T3> Reflection for (T0,T1,T2,T3)
    where T0: Reflection, T1: Reflection, T2: Reflection, T3: Reflection
{
    fn names() -> Names {
        - ( field("0") / <T0 as Reflection>::names() )
        - ( field("1") / <T1 as Reflection>::names() )
        - ( field("2") / <T2 as Reflection>::names() )
        - ( field("3") / <T3 as Reflection>::names() )
    }
}

impl<T0,T1,T2,T3,T4> Reflection for (T0,T1,T2,T3,T4)
    where T0: Reflection, T1: Reflection, T2: Reflection, T3: Reflection, T4: Reflection
{
    fn names() -> Names {
        - ( field("0") / <T0 as Reflection>::names() )
        - ( field("1") / <T1 as Reflection>::names() )
        - ( field("2") / <T2 as Reflection>::names() )
        - ( field("3") / <T3 as Reflection>::names() )
        - ( field("4") / <T4 as Reflection>::names() )
    }
}

impl<T0,T1,T2,T3,T4,T5> Reflection for (T0,T1,T2,T3,T4,T5)
    where T0: Reflection, T1: Reflection, T2: Reflection, T3: Reflection, T4: Reflection, T5: Reflection
{
    fn names() -> Names {
        - ( field("0") / <T0 as Reflection>::names() )
        - ( field("1") / <T1 as Reflection>::names() )
        - ( field("2") / <T2 as Reflection>::names() )
        - ( field("3") / <T3 as Reflection>::names() )
        - ( field("4") / <T4 as Reflection>::names() )
        - ( field("5") / <T5 as Reflection>::names() )
    }
}

impl<T> Reflection for Box<T> where T: ?Sized + Reflection, {
    fn names() -> Names { <T as Reflection>::names() }
}

impl<T> Reflection for Vec<T> where T: Sized + Reflection, {
    fn names() -> Names { <T as Reflection>::names() }
}

impl<T> Reflection for [T] where T: Reflection, {
    fn names() -> Names { <T as Reflection>::names() }
}

impl<T> Reflection for std::rc::Rc<T> where T: ?Sized + Reflection, {
    fn names() -> Names { <T as Reflection>::names() }
}

impl<'a, T: ?Sized> Reflection for &'a T {
    fn names() -> Names { Names::new() }
}
