namespace VSharp.Interpreter.IL

open System
open System.Collections.Generic
open System.IO
open System.Reflection
open System.Text.Json
open VSharp
open VSharp.Core
open System.Linq
open VSharp.Core.API

type public testSuite =
    | Test
    | Error of string * bool
    with
    member x.IsErrorSuite with get() =
        match x with
        | Error _ -> true
        | Test -> false
    member x.IsFatalError with get() =
        match x with
        | Error(_, isFatal) -> isFatal
        | Test -> false
    member x.ErrorMessage with get() =
        match x with
        | Error(msg, _) -> msg
        | Test -> null

type testGeneratorInfo = {
    test : ATest
    suite : testSuite
    targetMethod : Method
    indices : Dictionary<concreteHeapAddress, int>
    implementations : IDictionary<MethodInfo, term array * term array array>
    state : state
    model : model
    mockCache: Dictionary<ITypeMock,Mocking.Type>
} with
    member info.Decompose() =
        let mockCache = info.mockCache
        let model = info.model
        let state = info.state
        let indices = info.indices
        let implementations = info.implementations
        let test = info.test
        let suite = info.suite
        let method = info.targetMethod
        (test, suite, method, indices, implementations, state, model, mockCache)

module TestGenerator =

    let mutable private maxBufferSize = 2048
    let public setMaxBufferSize size = maxBufferSize <- size

    let private addMockToMemoryGraph (indices : Dictionary<concreteHeapAddress, int>) encodeMock evalField (test : ATest) addr (mock : ITypeMock) =
        let index = test.MemoryGraph.ReserveRepresentation()
        indices.Add(addr, index)
        let mock : Mocking.Type = encodeMock mock
        let baseClass = mock.BaseClass
        let fields =
            match evalField with
            | Some evalField when baseClass <> null && not (TypeUtils.isDelegate baseClass) ->
                Reflection.fieldsOf false baseClass
                |> Array.map (fst >> evalField)
            | _ -> Array.empty
        test.MemoryGraph.AddMockedClass mock fields index :> obj

    let private encodeType (state : state) cha =
        match state.memory.ConcreteMemory.TryVirtToPhys cha with
        | Some obj ->
            assert(obj :? Type)
            let t = obj :?> Type
            typeRepr.Encode t
        | None -> internalfail "encodeType: unable to encode symbolic instance of 'System.Type'"

    let private obj2test state eval encodeArr (indices : Dictionary<concreteHeapAddress, int>) encodeMock (test : ATest) addr typ =
        let index = ref 0
        if indices.TryGetValue(addr, index) then
            let referenceRepr : referenceRepr = {index = index.Value}
            referenceRepr :> obj
        else
            let cha = ConcreteHeapAddress addr
            match typ with
            | ConcreteType typ when TypeUtils.isDelegate typ ->
                // Obj is a delegate which mock hasn't been created yet
                let mock = TypeMock(Seq.singleton typ)
                addMockToMemoryGraph indices encodeMock None test addr mock
            | ConcreteType typ ->
                match typ with
                | TypeUtils.ArrayType(elemType, dim) ->
                    let index = test.MemoryGraph.ReserveRepresentation()
                    indices.Add(addr, index)
                    let arrayType, (lengths : int array), (lowerBounds : int array) =
                        match dim with
                        | Vector ->
                            let arrayType = arrayType.CreateVector elemType
                            let len = ArrayLength(cha, MakeNumber 0, arrayType) |> eval |> unbox
                            arrayType, [| len |], null
                        | ConcreteDimension rank ->
                            let arrayType = { elemType = elemType; dimension = rank; isVector = false }
                            let lens = Array.init rank (fun i -> ArrayLength(cha, MakeNumber i, arrayType) |> eval |> unbox)
                            let lbs = Array.init rank (fun i -> ArrayLowerBound(cha, MakeNumber i, arrayType) |> eval |> unbox)
                            arrayType, lens, lbs
                        | SymbolicDimension -> __notImplemented__()
                    let length = Array.reduce (*) lengths
                    // TODO: normalize model (for example, try to minimize lengths of generated arrays)
                    if maxBufferSize > 0 && length > maxBufferSize then
                        raise <| InsufficientInformationException "Test generation for too large buffers disabled for now"
                    let repr = encodeArr test arrayType addr typ lengths lowerBounds index
                    repr :> obj
                | _ when typ.IsValueType ->
                    let index = test.MemoryGraph.ReserveRepresentation()
                    indices.Add(addr, index)
                    let content = BoxedLocation(ConcreteHeapAddress addr, typ) |> eval
                    let repr = test.MemoryGraph.AddBoxed content index
                    repr :> obj
                | _ when typ = typeof<string> ->
                    let index = test.MemoryGraph.ReserveRepresentation()
                    indices.Add(addr, index)
                    let length : int = ClassField(cha, Reflection.stringLengthField) |> eval |> unbox
                    let string =
                        if length <= 0 then String.Empty
                        else
                            let readChar i =
                                ArrayIndex(cha, [MakeNumber i], arrayType.CharVector) |> eval |> unbox
                            let contents : char array = Array.init length readChar
                            String(contents)
                    test.MemoryGraph.AddString index string
                | _ when Reflection.isInstanceOfType typ ->
                    // For instance of 'System.Type' or 'System.RuntimeType'
                    encodeType state addr
                | _ ->
                    let index = test.MemoryGraph.ReserveRepresentation()
                    indices.Add(addr, index)
                    let fields = Reflection.fieldsOf false typ |> Array.map (fun (field, _) ->
                        ClassField(cha, field) |> eval)
                    let repr = test.MemoryGraph.AddClass typ fields index
                    repr :> obj
            | MockType mock when mock.IsValueType -> test.MemoryGraph.RepresentMockedStruct (encodeMock mock) Array.empty
            | MockType mock ->
                let evalField field = ClassField(cha, field) |> eval
                addMockToMemoryGraph indices encodeMock (Some evalField) test addr mock

    let private encodeArrayCompactly (state : state) (model : model) (encode : term -> obj) (test : ATest) arrayType cha typ lengths lowerBounds index =
        assert(TypeUtils.isArrayType typ)
        let mutable lengths = lengths
        let memory = state.memory
        if memory.ConcreteMemory.Contains cha then
            // TODO: Use compact representation for big arrays
            let contents =
                memory.ConcreteMemory.VirtToPhys cha :?> Array
                |> Array.mapToOneDArray test.MemoryGraph.Encode
            test.MemoryGraph.AddArray typ contents lengths lowerBounds index
        else
            // Solver may return models with unused values with indices out of
            // legal range. This code throws such values away to avoid IndexOutOfRange's
            // while running tests.
            let checkArrayIndex =
                if lowerBounds = null then
                    (fun (i : int list) ->
                        assert(lengths.Length = 1 && i.Length = 1)
                        List.head i < lengths[0])
                else
                    List.toSeq >> Seq.zip3 lowerBounds lengths >> Seq.forall (fun (lb, l, i) -> i >= lb && i < lb + l)
            let isModelArray = VectorTime.less cha VectorTime.zero
            let arrays =
                if isModelArray then
                    match model with
                    | StateModel modelState -> modelState.memory.Arrays
                    | _ -> __unreachable__()
                else state.memory.Arrays
            let elemType = arrayType.elemType
            let checkElem value =
                match value.term, model with
                | HeapRef({term = ConcreteHeapAddress address}, _), StateModel modelState ->
                    match PersistentDict.tryFind modelState.memory.AllocatedTypes address with
                    | Some(ConcreteType typ) -> Memory.IsSafeContextWrite typ elemType
                    | _ -> true
                | _ -> internalfail $"checkElem: unexpected element {value}"
            let isSuitableElem =
                if isModelArray && not elemType.IsValueType then checkElem
                else fun _ -> true
            let defaultValue, indices, values =
                match PersistentDict.tryFind arrays arrayType with
                | Some region ->
                    let defaultValue =
                        match region.defaultValue with
                        | Some defaultValue -> encode defaultValue
                        | None -> null
                    let updates = region.updates
                    let indicesWithValues = SortedDictionary<int list, obj>()
                    let addOneKey _ (k : updateTreeKey<heapArrayKey, term>) () =
                        let value = k.value
                        match k.key with
                        // Filtering wrong store from SMT-model
                        | _ when isSuitableElem value |> not -> ()
                        | OneArrayIndexKey(address, keyIndices) ->
                            let heapAddress = model.Eval address
                            match heapAddress with
                            | {term = ConcreteHeapAddress(cha')} when cha' = cha ->
                                let i = keyIndices |> List.map (encode >> unbox)
                                let v = encode value
                                if checkArrayIndex i then
                                    indicesWithValues[i] <- v
                            | _ -> ()
                        | RangeArrayIndexKey(address, fromIndices, toIndices) ->
                            let heapAddress = model.Eval address
                            match heapAddress with
                            | {term = ConcreteHeapAddress(cha')} when cha' = cha ->
                                let fromIndices : int list = fromIndices |> List.map (encode >> unbox)
                                let toIndices : int list = toIndices |> List.map (encode >> unbox)
                                let allIndices = Array.allIndicesViaBound fromIndices toIndices
                                match value.term with
                                | Constant(_, ArrayRangeReading, _) ->
                                    for i in allIndices do
                                        let index = List.map MakeNumber i
                                        let key = OneArrayIndexKey(heapAddress, index)
                                        let v = SpecializeWithKey value key k.key |> encode
                                        if checkArrayIndex i then
                                            indicesWithValues[i] <- v
                                | _ ->
                                    for i in allIndices do
                                        let v = encode value
                                        if checkArrayIndex i then
                                            indicesWithValues[i] <- v
                            | _ -> ()

                    updates |> RegionTree.foldr addOneKey ()
                    let indices = indicesWithValues.Keys.ToArray()
                    let values = indicesWithValues.Values.ToArray()

                    let decodeJsonString _ (k : updateTreeKey<heapArrayKey, term>) f =
                        let value = k.value
                        match k.key with
                        | OneArrayIndexKey(address, _) ->
                            let heapAddress = model.Eval address
                            match heapAddress, value.term with
                            | {term = ConcreteHeapAddress(cha')}, Constant ({v=name}, _, _) when name.Contains "JsonByte(" && cha' = cha ->
                                let v = value |> JsonDeserialize |> model.Eval |> encode
                                test.RefreshMemoryGraph()
                                v |> test.MemoryGraph.DecodeValue
                            | _ -> f
                        | _ -> f

                    let decodeJsonByte () =
                        let contents = updates |> RegionTree.foldr decodeJsonString null
                        let serialized = JsonSerializer.Serialize(contents)
                        let bytes = System.Text.Encoding.UTF8.GetBytes(serialized)
                        let jsonLength = Array.length bytes
                        let indices = Array.init jsonLength List.singleton
                        let bytesAsObj = Array.create jsonLength null
                        Array.iteri (fun i b -> bytesAsObj[i] <- b :> obj) bytes
                        lengths <- [| jsonLength |]
                        defaultValue, indices, bytesAsObj

                    let containsJsonByte _ (k : updateTreeKey<heapArrayKey, term>) f =
                        match k.value.term with
                        | Constant ({v=name}, _, _) when name.Contains "JsonByte(" -> true
                        | _ -> f

                    let arrayContainsJsonByte = updates |> RegionTree.foldr containsJsonByte false
                    if values.Length = 1 && arrayContainsJsonByte then decodeJsonByte () // TODO: Replace true by correct value
                    else defaultValue, indices.ToArray(), values.ToArray()
                | None -> null, Array.empty, Array.empty
            let indices = Array.map Array.ofList indices
            test.MemoryGraph.AddCompactArrayRepresentation typ defaultValue indices values lengths lowerBounds index

    let rec private term2obj info term : obj =
        let model = info.model
        let test = info.test
        let term2obj = term2obj info
        match term with
        | {term = Concrete(_, TypeUtils.AddressType)} -> __unreachable__()
        | {term = Concrete(v, t)} when t = typeof<IntPtr> ->
            test.MemoryGraph.RepresentIntPtr (int64 (v :?> IntPtr))
        | {term = Concrete(v, t)} when t = typeof<UIntPtr> ->
            test.MemoryGraph.RepresentUIntPtr (int64 (v :?> UIntPtr))
        | {term = Concrete(v, t)} when t.IsEnum -> test.MemoryGraph.RepresentEnum v
        | {term = Concrete(v, _)} -> v
        | {term = Nop} -> null
        | {term = Constant _ } as c -> model.Eval c |> term2obj
        | {term = Struct(fields, t)} when Types.IsNullable t ->
            let valueField, hasValueField = Reflection.fieldsOfNullable t
            let hasValue : bool = term2obj fields[hasValueField] |> unbox
            if hasValue then term2obj fields[valueField]
            else null
        | {term = Struct(fields, t)} ->
            let field2obj (field, _) = model.Complete fields[field] |> term2obj
            let fieldReprs = Reflection.fieldsOf false t |> Array.map field2obj
            test.MemoryGraph.RepresentStruct t fieldReprs
        | NullRef _ -> null
        | NullPtr sightType -> test.MemoryGraph.RepresentNullPtr sightType
        | DetachedPtrTerm offset ->
            let offset = TypeUtils.convert (term2obj offset) typeof<int64> :?> int64
            test.MemoryGraph.RepresentDetachedPtr typeof<Void> offset
        | {term = Ptr(HeapLocation({term = ConcreteHeapAddress(addr)}, _), sightType, offset)} ->
            let offset = TypeUtils.convert (term2obj offset) typeof<int64> :?> int64
            let obj = address2obj info addr
            if obj :? referenceRepr then
                let index = (obj :?> referenceRepr).index
                test.MemoryGraph.RepresentPtr index sightType offset
            else test.MemoryGraph.RepresentDetachedPtr sightType offset
        | {term = HeapRef({term = ConcreteHeapAddress(addr)}, _)} ->
            address2obj info addr
        | CombinedTerm(terms, t) ->
            let slices = List.map model.Eval terms
            ReinterpretConcretes slices t
        | term -> internalfail $"term2obj: creating object from term: unexpected term {term}"

    and private address2obj info (address : concreteHeapAddress) : obj =
        let test, _, _, indices, _, state, model, _ = info.Decompose()
        let term2obj = term2obj info
        match address with
        | _ when VectorTime.less address VectorTime.zero ->
            match model with
            | StateModel modelState ->
                match PersistentDict.tryFind modelState.memory.AllocatedTypes address with
                | Some typ ->
                    let eval address =
                        address |> Ref |> Memory.Read modelState |> model.Complete |> term2obj
                    let arr2Obj = encodeArrayCompactly state model term2obj
                    let encodeMock = encodeTypeMock info
                    obj2test state eval arr2Obj indices encodeMock test address typ
                // If address is not in the 'allocatedTypes', it should not be allocated, so result is 'null'
                | None -> null
            | PrimitiveModel _ -> __unreachable__()
        | _ ->
            let term2Obj = model.Eval >> term2obj
            let eval address = Ref address |> Memory.Read state |> term2Obj
            let arr2Obj = encodeArrayCompactly state model term2Obj
            let typ = state.memory.AllocatedTypes[address]
            let encodeMock = encodeTypeMock info
            obj2test state eval arr2Obj indices encodeMock test address typ

    and private encodeTypeMock info mock : Mocking.Type =
        let _, _, _, _, implementations, _, model, mockCache = info.Decompose()
        let mockedType = ref Mocking.Type.Empty
        if mockCache.TryGetValue(mock, mockedType) then mockedType.Value
        else
            let eval = model.Eval >> term2obj info
            let freshMock = Mocking.Type(mock.Name)
            mockCache.Add(mock, freshMock)
            for t in mock.SuperTypes do
                freshMock.AddSuperType t
                for methodMock in implementations do
                    let method = methodMock.Key
                    let values = fst methodMock.Value
                    let outParams = snd methodMock.Value
                    let methodType = method.ReflectedType
                    let mockedBaseInterface() =
                        methodType.IsInterface && Array.contains methodType (TypeUtils.getAllInterfaces t)
                    if methodType = t || mockedBaseInterface() then
                        let retImplementations = Array.map eval values
                        let outImplementations = Array.map (Array.map eval) outParams
                        freshMock.AddMethod(method, retImplementations, outImplementations)
            freshMock

    let encodeExternMock info (methodMock : IMethodMock) =
        let eval = info.model.Eval >> term2obj info
        let clauses = methodMock.GetImplementationClauses() |> Array.map eval
        let extMock = extMockRepr.Encode info.test.GetPatchId methodMock.BaseMethod clauses
        info.test.AddExternMock extMock

    let createImplementations info =
        let implementations = Dictionary<MethodInfo, term[] * term[][]>()
        for entry in info.state.methodMocks do
            let mock = entry.Value
            match mock.MockingType with
            | Default ->
                let values = mock.GetImplementationClauses()
                let outValues = mock.GetOutClauses()
                implementations.Add(mock.BaseMethod, (values, outValues))
            | Extern ->
                encodeExternMock info mock
        implementations

    let setErrorOrResult info k =
        let test = info.test
        let suite = info.suite
        let state = info.state
        match state.exceptionsRegister.Peek, suite with
        | Unhandled(e, _, _), Error(msg, isFatal) ->
            test.Exception <- MostConcreteTypeOfRef state e
            test.IsError <- true
            if String.IsNullOrEmpty msg then
                let messageReference = Memory.ReadField state e Reflection.exceptionMessageField |> info.model.Eval
                let encoded = term2obj info messageReference
                test.ErrorMessage <- test.MemoryGraph.DecodeString encoded
            else test.ErrorMessage <- msg
            test.IsFatalError <- isFatal
        | Unhandled(e, _, _), _ ->
            test.Exception <- MostConcreteTypeOfRef state e
        | _, Error(msg, isFatal) ->
            test.IsError <- true
            test.ErrorMessage <- msg
            test.IsFatalError <- isFatal
        | _ ->
            let retVal = Memory.StateResult state |> info.model.Eval
            let expected = term2obj info retVal
            k test expected

    let setArguments info modelState k =
        let m = info.targetMethod
        let state = info.state
        let test = info.test
        let parametersInfo = m.Parameters
        if state.complete then
            for pi in parametersInfo do
                let arg = Memory.ReadArgument state pi
                let concreteArg = term2obj info arg
                k test pi concreteArg
        else
            for pi in parametersInfo do
                let value =
                    if pi.ParameterType.IsByRef then
                        let key = ParameterKey pi
                        let stackRef = Memory.ReadLocalVariable state key
                        Memory.Read modelState stackRef
                    else
                        Memory.ReadArgument modelState pi |> state.model.Complete
                let concreteValue : obj = term2obj info value
                k test pi concreteValue

    let private modelState2test info modelState =
        let test = info.test :?> UnitTest
        let m = info.targetMethod
        match SolveGenericMethodParameters info.state.typeStorage info.targetMethod with
        | None -> None
        | Some(classParams, methodParams) ->
            let implementations = createImplementations info
            let info = { info with implementations = implementations }
            let concreteClassParams = Array.zeroCreate classParams.Length
            let mockedClassParams = Array.zeroCreate classParams.Length
            let concreteMethodParams = Array.zeroCreate methodParams.Length
            let mockedMethodParams = Array.zeroCreate methodParams.Length
            let encodeMock = encodeTypeMock info
            let processSymbolicType (concreteArr : Type array) (mockArr : Mocking.Type option array) i = function
                | ConcreteType t -> concreteArr[i] <- t
                | MockType m -> mockArr[i] <- Some (encodeMock m)
            classParams |> Seq.iteri (processSymbolicType concreteClassParams mockedClassParams)
            methodParams |> Seq.iteri (processSymbolicType concreteMethodParams mockedMethodParams)
            test.SetTypeGenericParameters concreteClassParams mockedClassParams
            test.SetMethodGenericParameters concreteMethodParams mockedMethodParams

            setArguments info modelState (fun t -> (t :?> UnitTest).AddArg)

            if m.HasThis then
                let thisTerm =
                    if m.DeclaringType.IsValueType then
                        let stackRef = Memory.ReadThis info.state m
                        Memory.Read modelState stackRef
                    else
                        Memory.ReadThis modelState m |> info.model.Complete
                let concreteThis = term2obj info thisTerm
                test.ThisArg <- concreteThis

            let setExpected (test : ATest) expected = test.Expected <- expected
            setErrorOrResult info setExpected
            Some (test :> ATest)

    let private model2test info =
        assert(info.suite.IsFatalError || info.state.exceptionsRegister.Size = 1)
        let suitableState state =
            if info.targetMethod.HasParameterOnStack then Memory.CallStackSize state = 2
            else Memory.CallStackSize state = 1

        if not <| suitableState info.state then
            internalfail "Finished state has many frames on stack! (possibly unhandled exception)"

        match info.model with
        | StateModel modelState -> modelState2test info modelState
        | _ -> __unreachable__()

    let model2webTest info =
        Logger.error "Starting to create test"
        let modelState =
            match info.state.model with
            | StateModel modelState -> modelState
            | _ -> __unreachable__()

        let retrieveMemoryStreamBuffer obj =
            let bufferField = typeof<MemoryStream>.GetField("_buffer", BindingFlags.NonPublic + BindingFlags.Instance)
            let buffer = bufferField.GetValue(obj) :?> byte array
            System.Text.Encoding.UTF8.GetString(buffer).Trim(null)

        let jsonSerialize = JsonSerializer.Serialize

        let fillCorrespondingField (test : ATest) (parameterInfo: ParameterInfo) (concreteValue : obj) =
            let test = test :?> AspIntegrationTest
            match parameterInfo.Position with
            | 0 -> ()
            | 1 -> ()
            | 2 -> test.RequestPath <- test.MemoryGraph.DecodeString concreteValue
            | 3 -> test.RequestMethod <- test.MemoryGraph.DecodeString concreteValue
            | 4 ->
                test.RefreshMemoryGraph()
                test.RequestBody <- test.MemoryGraph.DecodeValue concreteValue |> jsonSerialize
            | _ -> failwith "TODO"

        // TODO: Set assembly path
        // TODO: Set deps.json path

        let implementations = createImplementations info
        let info = {info with implementations = implementations}

        let parameterInfos = info.targetMethod.Parameters
        let concreteParameterCount = 4
        let stateParameters = Array.truncate concreteParameterCount parameterInfos |> Array.skip 2 // First two are technical
        let modelParameters = Array.skip concreteParameterCount parameterInfos

        for pi in stateParameters do
            Memory.ReadArgument info.state pi |> term2obj info |> fillCorrespondingField info.test pi

        for pi in modelParameters do
            Memory.ReadArgument modelState pi |> term2obj info |> fillCorrespondingField info.test pi

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

        let isJsonByte buffer =
            let zero = MakeNumber 0
            let firstElement = Memory.ReadArrayIndex info.state buffer [zero] None
            match firstElement.term with
            | Constant ({v=name}, _, _) when name.Contains "JsonByte(" -> true
            | _ -> false

        let setExpected (test : ATest) _ =
            let test = test :?> AspIntegrationTest
            let response = Memory.StateResult info.state |> info.model.Eval
            let responseType = TypeOf response
            let bodyField = Reflection.fieldsOf false responseType |> Array.find (fun (_, fi) -> fi.Name = "m_Item2") |> fst

            let responseBody = Memory.ReadField info.state response bodyField
            let bufferFieldId = getStreamBufferField info.state responseBody
            let buffer = Memory.ReadField info.state responseBody bufferFieldId
            let zero = MakeNumber 0
            let one = MakeNumber 1
            let bufferLength = Memory.CountOfArrayElements info.state buffer
            match bufferLength with
            | o when o = one && isJsonByte buffer ->
                let firstElement = Memory.ReadArrayIndex info.state buffer [zero] None
                let taskResult = JsonDeserialize firstElement
                let taskResultEvaluated = taskResult |> term2obj info
                test.ResponseBody <- taskResultEvaluated
            | z when z = zero ->
                let emptyString = Memory.AllocateEmptyString info.state zero
                let emptyString = term2obj info emptyString
                test.ResponseBody <- emptyString
            | _ ->
                () // TODO: Figure out a way to propagate message from stream here
            let responseStatusCodeField = Reflection.fieldsOf false responseType |> Array.find (fun (_, fi) -> fi.Name = "m_Item1") |> fst
            let responseStatusCode = Memory.ReadField info.state response responseStatusCodeField
            let responseStatusCodeEvaluated = responseStatusCode |> term2obj info
            test.ResponseStatusCode <- responseStatusCodeEvaluated :?> int32

        setErrorOrResult info setExpected

        Some info.test

    let public state2test testSuite (m : Method) (state : state) : ATest option =
        let indices = Dictionary<concreteHeapAddress, int>()
        let mockCache = Dictionary<ITypeMock, Mocking.Type>()
        let test = UnitTest((m :> IMethod).MethodBase)
        let info = {
            test=test
            suite=testSuite
            targetMethod=m
            indices=indices
            implementations=null
            state=state
            model=state.model
            mockCache=mockCache
        }
        model2test info

    let public state2webTest testSuite (m : Method) (state : state) : ATest option =
        let indices = Dictionary<concreteHeapAddress, int>()
        let mockCache = Dictionary<ITypeMock, Mocking.Type>()
        let test = AspIntegrationTest()
        let info = {
            test=test
            suite=testSuite
            targetMethod=m
            indices=indices
            implementations=null
            state=state
            model=state.model
            mockCache=mockCache
        }
        model2webTest info


    let public state2testWithMockingCache testSuite (m : Method) (state : state) mockCache =
        let indices = Dictionary<concreteHeapAddress, int>()
        let test = UnitTest((m :> IMethod).MethodBase)
        let info = {
            test=test
            suite=testSuite
            targetMethod=m
            indices=indices
            implementations=null
            state=state
            model=state.model
            mockCache=mockCache
        }
        model2test info
