namespace Rogz.Base


/// <summary></summary> 
[<Struct>]
type Maybe<'T> = Nothing | Just of 'T


/// <summary></summary>
type 'T maybe = Maybe<'T>


/// <summary></summary>
module Maybe =

// Primitive

    /// <summary></summary>
    val isJust: maybe: Maybe<'a> -> bool

    /// <summary></summary>
    val isNothing: maybe: Maybe<'a> -> bool

    /// <summary></summary>
    val inline caseof: just: (^a -> ^b) -> nothing: (unit -> ^b) -> maybe: Maybe< ^a> -> ^b

    /// <summary></summary>
    val fromMaybe: def: 'a -> maybe: Maybe< ^a> -> ^a

    /// <summary></summary>
    val inline fromMaybe': def: (unit -> ^a) -> maybe: Maybe< ^a> -> ^a

    /// <summary></summary>
    val inline mapMaybes: f: (^a -> Maybe< ^b>) -> source: #seq< ^a> -> ^b seq


// Conversions 

    /// <summary></summary>
    val ofObj: obj: 'a -> Maybe< ^a> when 'a : null

    /// <summary></summary>
    val ofNullable: nullable: System.Nullable<'a> -> Maybe< ^a>

    /// <summary></summary>
    val toSeq: maybe: Maybe<'a> -> ^a seq


// Functor

    /// <summary></summary>
    val inline map: f: (^a -> ^b) -> m: Maybe< ^a> -> Maybe< ^b>


// Applicative

    /// <summary></summary>
    val unit: x: 'a -> Maybe< ^a>

    /// <summary></summary>
    val inline map2: f: (^a -> ^b -> ^c) -> ma: Maybe< ^a> -> mb: Maybe< ^b> -> Maybe< ^c>

    /// <summary></summary>
    val inline map3: f: (^a -> ^b -> ^c -> ^d) -> ma: Maybe< ^a> -> mb: Maybe< ^b> -> mc: Maybe< ^c> -> Maybe< ^d>

    // traverse and/or sequence (for seqs)
    // if useful for the type: sequence_ (for seqs)

    /// <summary></summary>
    val sequence_: source: #seq<Maybe<'a>> -> Maybe<unit>


// Alternative

    /// <summary></summary>
    val empty<'a> : Maybe< ^a>
    
    /// <summary></summary>
    val orElse: second: Maybe<'a> -> first: Maybe< ^a> -> Maybe< ^a>

    /// <summary></summary>
    val orElse': second: (unit -> Maybe<'a>) -> first: Maybe< ^a> -> Maybe< ^a>

    /// <summary></summary>
    val concat: source: #seq<Maybe<'a>> -> Maybe< ^a>