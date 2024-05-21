namespace VSharp.System
open VSharp
open VSharp.Core
open VSharp.Interpreter.IL
open VSharp.Interpreter.IL.CilState

module ModelBinders =
    let complexObjectModelBinderBind (interpreter: IInterpreter) (cilState : cilState) (args : term list) : cilState list =
        failwith "TODO"