#![cfg_attr( feature = "custom_attribute", feature( custom_attribute ))]

#![allow( dead_code )]
#![allow( unused_attributes )]

#[macro_use]
extern crate reflection_derive;

extern crate reflection;
use reflection::Reflection;

extern crate trees;

#[derive( Reflection )]
struct S0;

#[derive( Reflection )]
struct SS0( S0 );

#[derive( Reflection )]
struct S1<'a> {
    bool_:bool, i8_:i8, u8_:u8, i16_:i16, u16_:u16, i32_:i32, u32_:u32, i64_:i64, u64_:u64, f32_:f32, f64_:f64, str_:&'a str, string:String
}

#[derive( Reflection )]
struct St1<'a> ( bool, i8, u8, i16, u16, i32, u32, i64, u64, f32, f64, &'a str, String );

#[derive( Reflection )]
struct SS1<'a>( S1<'a> );

#[derive( Reflection )]
struct SSt1<'a>( St1<'a> );

#[derive( Reflection )]
struct SString( String );

#[derive( Reflection )]
enum EEE {
    UUU( ((())) ),
}

#[derive( Reflection )]
struct Color(
    u32, // red
    u32, // green
    u32, // blue
    #[cfg(feature="custom_attribute")] 
    #[serde( skip_serializing )]
    u32, // alpha
);

#[derive( Reflection )]
struct Point {
    x: u32,
    y: u32,
    z: u32,
    #[cfg(feature="custom_attribute")] 
    #[serde( skip_serializing )]
    t: u32,
}

#[derive( Reflection )]
struct ColoredPoint {
    point: Point,
    #[cfg(feature="custom_attribute")] 
    #[serde( rename="colour" )]
    color: Color,
    #[cfg(not(feature="custom_attribute"))] 
    colour: Color,
}

#[derive( Reflection )]
struct StColoredPoint( Point, Color );

#[derive( Reflection )]
enum TrippleU32s {
    ColorT( Color ),
    ColorS{ color: Color },
    FlatColor( u32, u32, u32,
        #[cfg(feature="custom_attribute")] 
        #[serde( skip_serializing )]
        u32
    ),
    PointT( Point ),
    PointS{ point: Point },
    FlatPoint(
        #[cfg(feature="custom_attribute")] 
        #[serde( skip_serializing )]
        u32,
        #[cfg(feature="custom_attribute")] 
        #[serde( skip_serializing )]
        u32,
        #[cfg(feature="custom_attribute")] 
        #[serde( skip_serializing )]
        u32
    ),
}

fn main() {
    assert_eq!(              S0::names().to_string(), "()"                                                                    );
    assert_eq!(             SS0::names().to_string(), "( 0 )"                                                                 );
    assert_eq!(              S1::names().to_string(), "( bool_ i8_ u8_ i16_ u16_ i32_ u32_ i64_ u64_ f32_ f64_ str_ string )" );
    assert_eq!(             St1::names().to_string(), "( 0 1 2 3 4 5 6 7 8 9 10 11 12 )"                                      );
    assert_eq!(         SString::names().to_string(), "( 0 )"                                                                 );
    assert_eq!(             EEE::names().to_string(), "( UUU:( 0 ) )"                                                          );
    assert_eq!(           Color::names().to_string(), "( 0 1 2 )"                                                             );
    assert_eq!(           Point::names().to_string(), "( x y z )"                                                             );
    assert_eq!(    ColoredPoint::names().to_string(), "( point( x y z ) colour( 0 1 2 ) )"                                    );
    assert_eq!(  StColoredPoint::names().to_string(), "( 0( x y z ) 1( 0 1 2 ) )"                                             );
    assert_eq!( <(Color,Point)>::names().to_string(), "( 0( 0 1 2 ) 1( x y z ) )"                                             );
    assert_eq!(     TrippleU32s::names().to_string(), "( ColorT:( 0( 0 1 2 ) ) ColorS:( color( 0 1 2 ) ) FlatColor:( 0 1 2 ) \
                                                       PointT:( 0( x y z ) ) PointS:( point( x y z ) ) FlatPoint: )"           );
}
