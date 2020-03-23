namespace Rogz.Base.Control


module Maybe =

    open Rogz.Base

    let defaultValue def m =
        match m with
        | Nothing -> def
        | Just a  -> a