namespace VSharp.System

open System
open System.Reflection
open VSharp
open VSharp.Core
open System.Threading.Tasks
open VSharp.Interpreter.IL
open VSharp.Interpreter.IL.CilState

module Json =
    let resultElseDefault taskResult (fieldInfo : FieldInfo) _ fieldType =
        match fieldInfo.Name with
        | "_result" -> taskResult
        | _ -> Memory.DefaultOf fieldType

    let valueTaskOfResult (cilState : cilState) boxObject taskResult resultType =
        let taskType = typeof<ValueTask>
        let taskFromResultMethod =
            taskType.GetMethods()
            |> Array.find (fun x -> x.Name = "FromResult")
            |> fun x -> x.MakeGenericMethod([|resultType|])
        let taskResult = if boxObject then Memory.BoxValueType cilState.state taskResult else taskResult
        let valueTaskStruct = MakeStruct false (resultElseDefault taskResult) taskFromResultMethod.ReturnType
        valueTaskStruct

    let getStreamBufferField state stream =
        let memoryStreamType = MostConcreteTypeOfRef state stream
        let memoryStreamBufferField =
            memoryStreamType.GetFields(BindingFlags.Instance + BindingFlags.NonPublic)
            |> Array.find (fun x -> x.Name = "_buffer")
        let bufferFieldId =
            { declaringType = memoryStreamType
              name = "_buffer"
              typ = memoryStreamBufferField.FieldType }
        bufferFieldId

    let serialize (state : state) (args : term list) =
        assert (List.length args = 5)
        let destination, source, options = args[1], args[2], args[3]
        match destination with
        | {term = HeapRef _; hc = _} ->
            let serializedByte = API.Terms.JsonSerialize source options
            let bufferFieldId = getStreamBufferField state destination
            // TODO: Later maybe merge 2 buffers
            let emptyBuffer = Memory.AllocateDefaultArray state [MakeNumber 1] bufferFieldId.typ
            let states = Memory.WriteArrayIndex state emptyBuffer [MakeNumber 0] serializedByte (Some typeof<byte>)
            assert(List.length states = 1)
            let states = Memory.WriteClassField state destination bufferFieldId emptyBuffer
            assert(List.length states = 1)
            let completedTask = Task.CompletedTask
            Memory.AllocateConcreteObject state completedTask (completedTask.GetType())
        | _ ->
            internalfail "Stream was not a HeapRef"

    let serializeOther (state : state) (args : term list) =
        // This serialize is similar and equivalent to one above, but with different arguments
        assert (List.length args = 5)
        serialize state [args[2]; args[0]; args[1]; args[3]; args[4]]

    let deserialize (interpreter: IInterpreter) (cilState : cilState) (args : term list) =
        assert (List.length args = 4)
        let stream, typ, options = args[0], args[1], args[3]

        let returnTask typesMatched isValueType (cilState : cilState) =
            let taskResult = cilState.Pop()
            cilState.StatedConditionalExecutionCIL
                (fun state k -> k (if typesMatched then True(), state else False(), state))
                (fun cilState k -> cilState.Push (valueTaskOfResult cilState isValueType taskResult (typeof<obj>)); k [cilState])
                (interpreter.Raise interpreter.ArgumentException) // TODO: Maybe properly do JsonException?
                id

        match stream, TryTermToFullyConcreteObj cilState.state typ with
        | {term = HeapRef _; hc = _}, Some concreteTyp ->
            let isValueType = (concreteTyp :?> Type).IsValueType
            let bufferFieldId = getStreamBufferField cilState.state stream
            let buffer = Memory.ReadField cilState.state stream bufferFieldId
            let firstElement = Memory.ReadArrayIndex cilState.state buffer [MakeNumber 0] None
            let taskResult = API.Terms.JsonDeserialize firstElement options
            let typesMatched = concreteTyp = TypeOf taskResult // TODO: More elaborate matching of type (it's json, not strict)
            let copiedStates = Object.DeepCopy cilState taskResult false
            List.collect (returnTask typesMatched isValueType) copiedStates

        | _, Some _ ->
            internalfail "Stream was not a HeapRef"
        | _, _ ->
            internalfail "Type of deserialized object has to be concrete"
