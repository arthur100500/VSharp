namespace VSharp.System

open VSharp
open VSharp.Core

module Json =
    let serialize (state : state) (args : term list) =
        assert (List.length args = 3)
        let _, source, _ = args[0], args[1], args[2]
        API.Terms.JsonSerialize source

    let deserialize (state : state) (args : term list) =
        assert (List.length args = 4)
        let stream = args[0]
        Nop ()
