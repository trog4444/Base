namespace Rogz.Base


[<Struct>]
type Maybe<'T> = Nothing | Just of 'T


type 'T maybe = Maybe<'T>


module Maybe =

// Primitive

    let isJust maybe =
        match maybe with
        | Just _  -> true
        | Nothing -> false

    let isNothing maybe =
        match maybe with
        | Just _  -> false
        | Nothing -> true

    let inline caseof (just: ^a -> ^b) nothing maybe =
        match maybe with
        | Nothing -> nothing ()
        | Just a  -> just a

    let fromMaybe def maybe =
        match maybe with
        | Nothing -> def
        | Just a  -> a

    let inline fromMaybe' def maybe : ^a =
        match maybe with
        | Nothing -> def ()
        | Just a  -> a

    let inline mapMaybes (f: ^a -> Maybe< ^b>) (source: #seq<_>) =
        seq { for x in source do
                match f x with
                | Nothing -> ()
                | Just a  -> yield a }


// Conversions

    // ?? [toSeq, ofSeq, toOption, ofOption, etc] OR maybe put these in a static class with Of and To methods overloaded

    let ofObj (obj: 'a) : Maybe< ^a> when 'a : null =
        if isNull obj then Nothing else Just obj

    let ofNullable (nullable: System.Nullable<'a>) =
        if nullable.HasValue then Just nullable.Value else Nothing

    let toSeq maybe =
        match maybe with
        | Nothing -> Seq.empty
        | Just a  -> Seq.singleton a


// Functor
    
    let inline map (f: ^a -> ^b) m =
        match m with
        | Nothing -> Nothing
        | Just a  -> Just (f a)


// Applicative

    let unit x = Just x

    // ap ?

    let inline map2 (f: ^a -> ^b -> ^c) ma mb =
        match ma, mb with
        | Nothing, _ | _, Nothing -> Nothing
        | Just a, Just b          -> Just (f a b)

    let inline map3 (f: ^a -> ^b -> ^c -> ^d) ma mb mc =
        match ma, mb, mc with
        | Nothing, _, _ | _, Nothing, _ | _, _, Nothing -> Nothing
        | Just a, Just b, Just c                        -> Just (f a b c)

    let inline traverse f (source: #seq<_>) : Maybe<_ seq> =
        use e = source.GetEnumerator()
        let xs = ResizeArray<_>()
        let rec go () =
            if e.MoveNext() then
                match f e.Current with
                | Nothing -> false
                | Just b  -> go (xs.Add b)
            else true
        if go () then Just (Seq.readonly xs)
        else xs.Clear()
             Nothing

    let inline traverse_ f (source: #seq<_>) : Maybe<unit> =
        use e = source.GetEnumerator()
        let rec go () =
            if e.MoveNext() then
                match f e.Current with
                | Nothing -> Nothing
                | Just _  -> go ()
            else Just ()
        go ()

    let sequence (source: #seq<Maybe<_>>) : Maybe<_ seq> =
        use e = source.GetEnumerator()
        let xs = ResizeArray<_>()
        let rec go () =
            if e.MoveNext() then
                match e.Current with
                | Nothing -> false
                | Just a  -> go (xs.Add a)
            else true
        if go () then Just (Seq.readonly xs)
        else xs.Clear()
             Nothing

//////////////////////////////////////////////////////////////////////////////////////////////////////////////

// This CPS transformation MAY be more performant in some cases. Perhaps add DList functionality as well..
// ? private helper function to share between all sequence functions?
// It also actually is possible to retain laziness to some degree, but performance suffers.
// The main thing with the list version is that (maybe) the list only exists at the end, possibly saving memory.
// Perhaps more and even performance with DLists.

    let inline sequence_cps_list (xs: _ maybe list) : Maybe<_ list> =
        let rec go xs k =
            match xs with
            | [] -> k []
            | Nothing::_   -> Nothing
            | (Just y)::xs -> go xs (fun ys -> printfn "%A" ys; k (y::ys))
        go xs Just

    let inline sequence_cps_seq (xs: _ maybe seq) : Maybe<_ list> =
        use e = xs.GetEnumerator()
        let rec go k =
            if e.MoveNext() then
                match e.Current with
                | Nothing -> Nothing
                | Just a  -> go (fun xs -> k <| seq { yield a; yield! xs })
            else k Seq.empty
        go Just |> map Seq.toList

    let xs = List.init 10 Just
    let ys = List.init 10 (fun x -> if x > 5 then Nothing else Just x)
    let xs' = sequence_cps_list xs
    printfn "----------------------------"
    let ys' = sequence_cps_list ys

    let r = ref 0
    let lxs = Seq.init 10 (fun i -> incr r; Just i)
    let lxs' = sequence_cps_seq lxs

//////////////////////////////////////////////////////////////////////////////////////////////////////////////

    let sequence_ (source: #seq<Maybe<_>>) : Maybe<unit> =
        use e = source.GetEnumerator()
        let rec go () =
            if e.MoveNext() then
                match e.Current with
                | Nothing -> Nothing
                | Just _  -> go ()
            else Just ()
        go ()

    let inline zipWith f (xs: #seq<_>) (ys: #seq<_>) =
        use ex = xs.GetEnumerator()
        use ey = ys.GetEnumerator()
        let rs = ResizeArray<_>()
        let rec go () =
            if ex.MoveNext() && ey.MoveNext() then
                match f ex.Current ey.Current with
                | Nothing -> false
                | Just c  -> go (rs.Add c)
            else true
        if go () then Just (Seq.readonly rs)
        else rs.Clear()
             Nothing

    let inline zipWith_ f (xs: #seq<_>) (ys: #seq<_>) =
        use ex = xs.GetEnumerator()
        use ey = ys.GetEnumerator()
        let rec go () =
            if ex.MoveNext() && ey.MoveNext() then
                match f ex.Current ey.Current with
                | Nothing -> Nothing
                | Just _  -> go ()
            else Just ()
        go ()


// Alternative
    
    let empty<'a> : Maybe< ^a> = Nothing
    
    let orElse second first =
        match first with
        | Nothing -> second
        | Just _  -> first

    let orElse' second first =
        match first with
        | Nothing -> second ()
        | Just _  -> first

    let concat (source: #seq<Maybe<'a>>) : Maybe< ^a> =
        System.Linq.Enumerable.FirstOrDefault(source, fun a -> match a with Just _ -> true | Nothing -> false)


// Monad
    
    let inline bind f m =
        match m with
        | Nothing -> Nothing
        | Just a  -> f a

    let flatten mm =
        match mm with
        | Nothing -> Nothing
        | Just m  -> m    


    [<RequireQualifiedAccess>]
    module Workflow =

        type MaybeBuilder() =
            member _.Return(x: 'a) = Just x
            member _.ReturnFrom(m) : Maybe<'a> = m
            member _.Zero() = Just ()
            member inline _.Bind(m, f) : Maybe< ^b> = bind f m
            member _.Using(res: 'd, f) : Maybe<'a> when 'd :> System.IDisposable = using res f


    let maybe = Workflow.MaybeBuilder()


    //////////foldM  :: Monad m => (b -> a -> m b) -> b -> [a] -> m b
    //////////foldM_ :: Monad m => (b -> a -> m b) -> b -> [a] -> m ()



// MonadPlus

    let guard condition = if condition then Just () else Nothing

    let inline join p f ma mb =
        match ma, mb with
        | Nothing, _ | _, Nothing -> Nothing
        | Just a, Just b          -> if p a b then Just (f a b) else Nothing
    
    let inline filter p m =
        match m with
        | Nothing -> Nothing
        | Just a  -> if p a then m else Nothing


// Foldable

    let inline fold f s m =
        match m with
        | Nothing -> s
        | Just a  -> f s a
    
    let inline foldBack f s m =
        match m with
        | Nothing -> s
        | Just a  -> f a s

    let inline mapFold f s m : struct (Maybe< ^r> * ^s) =
        match m with
        | Nothing -> struct (Nothing, s)
        | Just a  -> let struct (r, s) = f s a in struct (Just r, s)

    let inline mapFoldBack f s m : struct (Maybe< ^r> * ^s) =
        match m with
        | Nothing -> struct (Nothing, s)
        | Just a  -> let struct (r, s) = f a s in struct (Just r, s)

// Note* -- Semigroup and Monoid are removed because I am not going to use ANY duck-typing (ie statically resolved constraints).