namespace VSharp.System

open System
open VSharp
open VSharp.Core
open VSharp.Interpreter.IL
open VSharp.Interpreter.IL.CilState

module Json =
    // For use in other places
    val valueTaskOfResult: cilState -> bool -> term -> Type -> term

    [<Implements("System.Threading.Tasks.Task System.Text.Json.JsonSerializer.SerializeAsync(System.IO.Stream, TValue, System.Text.Json.JsonSerializerOptions, System.Threading.CancellationToken)")>]
    val serialize: state -> term list -> term

    [<Implements("System.Threading.Tasks.Task System.Text.Json.JsonSerializer.SerializeAsync(System.IO.Stream, System.Object, System.Type, System.Text.Json.JsonSerializerOptions, System.Threading.CancellationToken)")>]
    val serializeOther: state -> term list -> term

    [<Implements("System.Threading.Tasks.ValueTask`1[System.Object] System.Text.Json.JsonSerializer.DeserializeAsync(System.IO.Stream, System.Type, System.Text.Json.JsonSerializerOptions, System.Threading.CancellationToken)")>]
    val deserialize: IInterpreter -> cilState -> term list -> cilState list
