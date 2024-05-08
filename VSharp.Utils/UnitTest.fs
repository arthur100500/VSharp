namespace VSharp

open System
open System.Diagnostics
open System.IO
open System.Reflection
open System.Xml.Serialization
open FSharpx.Collections
open VSharp

[<CLIMutable>]
[<Serializable>]
[<XmlInclude(typeof<structureRepr>)>]
[<XmlInclude(typeof<arrayRepr>)>]
[<XmlInclude(typeof<referenceRepr>)>]
[<XmlInclude(typeof<pointerRepr>)>]
[<XmlInclude(typeof<enumRepr>)>]
[<XmlInclude(typeof<stringRepr>)>]
type testInfo = {
    common : commonTestInfo
    assemblyName : string
    moduleFullyQualifiedName : string
    token : int32
    thisArg : obj
    args : obj array
    expectedResult : obj
    classTypeParameters : typeRepr array
    methodTypeParameters : typeRepr array
    mockClassTypeParameters : Nullable<int> array
    mockMethodTypeParameters : Nullable<int> array
}
with
    static member OfMethod(m : MethodBase) = {
        common = {
            isError = false
            isFatalError = false
            memory = {objects = Array.empty; types = Array.empty}
            typeMocks = Array.empty
            extraAssemblyLoadDirs = Array.empty
            externMocks = new ResizeArray<extMockRepr>()
            throwsException = {assemblyName = null; moduleFullyQualifiedName = null; name = null; genericArgs = null}
            errorMessage = null
        }
        assemblyName = m.Module.Assembly.FullName
        moduleFullyQualifiedName = m.Module.FullyQualifiedName
        token = m.MetadataToken
        thisArg = null
        args = null
        expectedResult = null
        classTypeParameters = Array.empty
        methodTypeParameters = Array.empty
        mockClassTypeParameters = Array.empty
        mockMethodTypeParameters = Array.empty
    }

type UnitTest private (m : MethodBase, info : testInfo, mockStorage : MockStorage, createCompactRepr : bool) =
    inherit ATest(mockStorage, typeof<testInfo>)
    let common = info.common
    let mutable memoryGraph = MemoryGraph(common.memory, mockStorage, createCompactRepr)
    let thisArg = memoryGraph.DecodeValue info.thisArg
    let args = if info.args = null then null else info.args |> Array.map memoryGraph.DecodeValue
    let expectedResult = memoryGraph.DecodeValue info.expectedResult
    let compactRepresentations = memoryGraph.CompactRepresentations()

    new(m : MethodBase) =
        UnitTest(m, testInfo.OfMethod m, MockStorage(), false)

    override x.FileExtension with get() = "vst"
    override x.Common with get() = common
    override x.Info with get() = info
    member x.Method with get() = m
    member x.ThisArg
        with get() = thisArg
        and set this =
            let t = typeof<testInfo>
            let p = t.GetProperty("thisArg")
            p.SetValue(info, memoryGraph.Encode this)

    member x.Args with get() = args

    override x.Expected
        with get() = expectedResult
        and set r =
            let t = typeof<testInfo>
            let p = t.GetProperty("expectedResult")
            p.SetValue(info, r)

    member x.TypeMocks with get() : ResizeArray<Mocking.Type> = mockStorage.TypeMocks

    member x.CompactRepresentations with get() = compactRepresentations

    member private x.SerializeMock (m : Mocking.Type option) =
        match m with
        | Some m -> Nullable(mockStorage.RegisterMockedType m)
        | None -> Nullable()

    // @concreteParameters and @mockedParameters should have equal lengths and be complementary:
    // if @concreteParameters[i] is null, then @mockedParameters[i] is non-null and vice versa
    member x.SetTypeGenericParameters (concreteParameters : Type array) (mockedParameters : Mocking.Type option array) =
        let t = typeof<testInfo>
        let cp = t.GetProperty("classTypeParameters")
        cp.SetValue(info, concreteParameters |> Array.map typeRepr.Encode)
        let mp = t.GetProperty("mockClassTypeParameters")
        mp.SetValue(info, mockedParameters |> Array.map x.SerializeMock)

    // @concreteParameters and @mockedParameters should have equal lengths and be complementary:
    // if @concreteParameters[i] is null, then @mockedParameters[i] is non-null and vice versa
    member x.SetMethodGenericParameters (concreteParameters : Type array) (mockedParameters : Mocking.Type option array) =
        let t = typeof<testInfo>
        let cp = t.GetProperty("methodTypeParameters")
        cp.SetValue(info, concreteParameters |> Array.map typeRepr.Encode)
        let mp = t.GetProperty("mockMethodTypeParameters")
        mp.SetValue(info, mockedParameters |> Array.map x.SerializeMock)

    override x.MemoryGraph with get() = memoryGraph

    member x.ExtraAssemblyLoadDirs with get() = common.extraAssemblyLoadDirs

    override x.RefreshMemoryGraph() =
        let mem = x.Common.memory
        x.MemoryGraph.Serialize(mem)
        memoryGraph <- MemoryGraph(mem, mockStorage, false)

    member x.AddArg (arg : ParameterInfo) (value : obj) =
        if info.args = null then
            let t = typeof<testInfo>
            let p = t.GetProperty("args")
            p.SetValue(info, Array.zeroCreate <| m.GetParameters().Length)
        let value = memoryGraph.Encode value
        info.args[arg.Position] <- value

    static member DeserializeTestInfo(stream : FileStream) =
        ATest.DeserializeTestInfo(stream)

    static member DeserializeFromTestInfo(ti : testInfo, createCompactRepr : bool) =
        try
            let mdle = Reflection.resolveModule ti.assemblyName ti.moduleFullyQualifiedName
            if mdle = null then
                raise <| InvalidOperationException($"Could not resolve module {ti.moduleFullyQualifiedName}!")
            let mockStorage = MockStorage()
            mockStorage.Deserialize ti.common.typeMocks
            let decodeTypeParameter (concrete : typeRepr) (mockIndex : Nullable<int>) =
                if mockIndex.HasValue then
                    mockStorage[mockIndex.Value] |> snd
                else
                    let res = concrete.Decode()
                    assert(res <> null)
                    res
            let mp = Array.map2 decodeTypeParameter ti.methodTypeParameters ti.mockMethodTypeParameters
            let method = mdle.ResolveMethod(ti.token) |> AssemblyManager.NormalizeMethod
            let reflectedType = method.ReflectedType
            let tp = Array.map2 decodeTypeParameter ti.classTypeParameters ti.mockClassTypeParameters
            let concreteReflectedType = Reflection.concretizeTypeParameters method.ReflectedType tp
            let method = Reflection.concretizeMethodParameters concreteReflectedType method mp

            let common = ti.common
            // Ensure that parameters are substituted in memoryRepr
            if not method.IsStatic && reflectedType.IsGenericType && common.memory.types.Length > 0 then
                let getGenericTypeDefinition (typ : typeRepr) =
                    let decoded = typ.Decode()
                    if decoded <> null && decoded.IsGenericType then
                        decoded.GetGenericTypeDefinition()
                    else decoded
                let typeDefinitions = common.memory.types |> Array.map getGenericTypeDefinition
                let reflectedTypeIndex = Array.IndexOf(typeDefinitions, reflectedType)
                assert(reflectedTypeIndex >= 0 || common.typeMocks.Length > 0)
                if reflectedTypeIndex >= 0 then
                    // 'this' is not mock
                    common.memory.types[reflectedTypeIndex] <- typeRepr.Encode concreteReflectedType

            UnitTest(method, ti, mockStorage, createCompactRepr)
        with child ->
            let exn = InvalidDataException("Input test is incorrect", child)
            raise exn

    static member Deserialize(stream : FileStream) =
        let testInfo = UnitTest.DeserializeTestInfo(stream)
        UnitTest.DeserializeFromTestInfo(testInfo, false)

    static member Deserialize(source : string) =
        use stream = new FileStream(source, FileMode.Open, FileAccess.Read)
        UnitTest.Deserialize stream
