namespace VSharp.System

open global.System
open VSharp
open VSharp.Core
open VSharp.Interpreter.IL
open VSharp.Interpreter.IL.CilState

module internal Object =

    [<Implements("System.Object System.Object.MemberwiseClone(this)")>]
    val MemberwiseClone : IInterpreter -> cilState -> term list -> cilState list

    // For use in other utils
    val DeepCopy : cilState -> term -> bool -> cilState list
