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

    let rec copy state (cilState : cilState) source recursive forkOnNull (visited : HashSet<Type>) =
        let t = getTermType state source
        let copyRef (cilState : cilState) =
            let state = cilState.state
            if TypeUtils.isArrayType t then
                if not t.IsSZArray then
                    internalfail $"MemberwiseClone: non-vector arrays are not supported {t}"
                let newObject = Memory.AllocateDefaultClass state t
                let zero = MakeNumber 0
                let len = Memory.ArrayLengthByDimension state source zero
                Memory.CopyArray state source zero t newObject zero t len
                cilState.Push newObject
                List.singleton cilState
            elif t = typeof<string> then
                let length = cilState.ReadField source Reflection.stringLengthField
                let newObject = Memory.AllocateDefaultClass state t
                let newStates = cilState.WriteClassField newObject Reflection.stringLengthField length
                let zero = MakeNumber 0
                for s in newStates do
                    Memory.CopyStringArray s.state source zero newObject zero length; s.Push newObject
                newStates
            elif t.IsValueType then
                let v = cilState.Read source
                let ref = Memory.BoxValueType state v
                cilState.Push ref
                List.singleton cilState
            elif isNull visited |> not && visited.Contains(t) then
                internalfail "Unable to copy recursive objects"
            else
                let t =
                    if t.IsAbstract then
                        let address =
                            match source.term with
                            | HeapRef(address, _) -> address
                            | _ -> internalfail $"MemberwiseClone: unexpected object ref {source}"
                        match state.typeStorage[address] with
                        | Some candidates -> Seq.head (Seq.head candidates.ConcreteTypes).Types
                        | _ -> t
                    else t
                let newObject = Memory.AllocateDefaultClass state t
                let fields = Reflection.fieldsOf false t
                let copyFieldShallow cilStates (field, _) =
                    let copyForState (cilState : cilState) =
                        let v = cilState.ReadField source field
                        cilState.WriteClassField newObject field v
                    List.collect copyForState cilStates
                let copyFieldDeep cilStates (field, _) =
                    let copyForState (cilState : cilState) =
                        let v = cilState.ReadField source field
                        visited.Add t |> ignore
                        let copiedStates = copy state cilState v true forkOnNull visited
                        visited.Remove t |> ignore
                        copiedStates |> List.collect (fun (s : cilState) -> s.WriteClassField newObject field (s.Pop()))
                    List.collect copyForState cilStates
                let copyField = if recursive then copyFieldDeep else copyFieldShallow
                let cilStates = Array.fold copyField (List.singleton cilState) fields
                for cilState in cilStates do
                    cilState.Push newObject
                cilStates
        match source.term with
        | Ref _
        | HeapRef _ when forkOnNull ->
            cilState.StatedConditionalExecutionCIL
                (fun state k -> k (IsNullReference source, state))
                (fun (cilState : cilState) k -> NullRef t |> cilState.Push; k [cilState])
                (fun (cilState : cilState) k -> copyRef cilState |> k)
                id
        | HeapRef _ ->
            copyRef cilState
        | Union _ -> __notImplemented__()
        | _ ->
            cilState.Push source
            [cilState]


    let MemberwiseClone (_ : IInterpreter) (cilState : cilState) (args : term list) =
        assert(List.length args = 1)
        let object = args[0]
        let state = cilState.state
        copy state cilState object false false null

    let DeepCopy (cilState : cilState) (object : term) (forkOnNull : bool) =
        let visited = HashSet<Type>()
        copy cilState.state cilState object true forkOnNull visited
