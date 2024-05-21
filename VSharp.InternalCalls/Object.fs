namespace VSharp.System

open System.Collections.Generic
open VSharp.Core.API
open global.System
open VSharp
open VSharp.Core
open VSharp.Interpreter.IL
open VSharp.Interpreter.IL.CilState

module internal Object =

    let getTermType (state) (t : term) =
        match t.term with
        | HeapRef _ -> MostConcreteTypeOfRef state t
        | _ -> TypeOf t

    let rec copy state (cilState : cilState) object recursive (visited : HashSet<term>) =
        let t = getTermType state object
        match t, object.term with
        | _, _ when TypeUtils.isArrayType t ->
            if not t.IsSZArray then
                internalfail $"MemberwiseClone: non-vector arrays are not supported {t}"
            let newObject = Memory.AllocateDefaultClass state t
            let zero = MakeNumber 0
            let len = Memory.ArrayLengthByDimension state object zero
            Memory.CopyArray state object zero t newObject zero t len
            cilState.Push newObject
            List.singleton cilState
        | _, Constant(v, s, t) ->
            let readValue = cilState.ReadField
            []
        | _, _ when t.IsValueType ->
            let v = cilState.Read object
            let ref = Memory.BoxValueType state v
            cilState.Push ref
            List.singleton cilState
        |_, _ when (isNull visited || visited.Contains(object) |> not) ->
            let t =
                if t.IsAbstract then
                    let address =
                        match object.term with
                        | HeapRef(address, _) -> address
                        | _ -> internalfail $"MemberwiseClone: unexpected object ref {object}"
                    match state.typeStorage[address] with
                    | Some candidates -> Seq.head (Seq.head candidates.ConcreteTypes).Types
                    | _ -> t
                else t
            let newObject = Memory.AllocateDefaultClass state t
            let fields = Reflection.fieldsOf false t
            let copyFieldShallow cilStates (field, _) =
                let copyForState (cilState : cilState) =
                    let v = cilState.ReadField object field
                    cilState.WriteClassField newObject field v
                List.collect copyForState cilStates
            let copyFieldDeep cilStates (field, _) =
                let copyForState (cilState : cilState) =
                    let v = cilState.ReadField object field
                    visited.Add object |> ignore
                    let copiedStates = copy state cilState v true visited
                    copiedStates |> List.collect (fun x -> x.WriteClassField newObject field v)
                List.collect copyForState cilStates
            let copyField = if recursive then copyFieldDeep else copyFieldShallow
            let cilStates = Array.fold copyField (List.singleton cilState) fields
            for cilState in cilStates do
                cilState.Push newObject
            cilStates
        | _, _ ->
            [cilState]

    let MemberwiseClone (_ : IInterpreter) (cilState : cilState) (args : term list) =
        assert(List.length args = 1)
        let object = args[0]
        let state = cilState.state
        copy state cilState object false null

    let DeepCopy (cilState : cilState) (object : term) =
        let visited = HashSet<term>()
        copy cilState.state cilState object true visited