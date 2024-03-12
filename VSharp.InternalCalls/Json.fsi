namespace VSharp.System

open VSharp
open VSharp.Core

module Json =
    [<Implements("System.String System.Text.Json.JsonSerializer.Serialize(TValue, System.Text.Json.JsonSerializerOptions)")>]
    val serialize: state -> term list -> term

    [<Implements("System.Threading.Tasks.ValueTask`1[System.Object] System.Text.Json.JsonSerializer.DeserializeAsync(System.IO.Stream, System.Type, System.Text.Json.JsonSerializerOptions, System.Threading.CancellationToken)")>]
    val deserialize: state -> term list -> term