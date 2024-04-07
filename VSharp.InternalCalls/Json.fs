namespace VSharp.System

open System
open System.Reflection
open VSharp
open VSharp.Core
open System.Threading.Tasks

module Json =
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
        let destination, source = args[1], args[2]
        match destination with
        | {term = HeapRef _; hc = _} ->
            let serializedByte = API.Terms.JsonSerialize source
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
        // This serialize is similar and equivalent to one above, but with different arguemnts
        assert (List.length args = 5)
        serialize state [args[2]; args[0]; args[1]; args[2]; args[3]]


    let deserialize (state : state) (args : term list) =
        assert (List.length args = 4)
        let stream, typ = args[0], args[1]

        let resultElseDefault taskResult (fieldInfo : FieldInfo) fieldId fieldType =
            match fieldInfo.Name with
            | "_result" -> taskResult
            | _ -> Memory.DefaultOf fieldType

        match stream, TryTermToFullyConcreteObj state typ with
        | {term = HeapRef _; hc = _}, Some concreteTyp ->
            let isValueType = (concreteTyp :?> Type).IsValueType
            let bufferFieldId = getStreamBufferField state stream
            let buffer = Memory.ReadField state stream bufferFieldId
            let firstElement = Memory.ReadArrayIndex state buffer [MakeNumber 0] None
            let taskResult = API.Terms.JsonDeserialize firstElement
            let taskType = typeof<ValueTask>
            let taskFromResultMethod =
                taskType.GetMethods()
                |> Array.find (fun x -> x.Name = "FromResult")
                |> _.MakeGenericMethod(typeof<obj>)

            let taskResult = if isValueType then Memory.BoxValueType state taskResult else taskResult
            let valueTaskStruct = MakeStruct false (resultElseDefault taskResult) taskFromResultMethod.ReturnType
            valueTaskStruct
        | _, Some _ ->
            internalfail "Stream was not a HeapRef"
        | _, _ ->
            internalfail "Type of deserialized object has to be concrete"
