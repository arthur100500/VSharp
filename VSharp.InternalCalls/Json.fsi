namespace VSharp.System

open VSharp
open VSharp.Core

module Json =
    [<Implements("System.String System.Text.Json.JsonSerializer.Serialize(TValue, System.Text.Json.JsonSerializerOptions)")>]
    val serialize: state -> term list -> term