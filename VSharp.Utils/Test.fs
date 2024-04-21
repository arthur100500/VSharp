namespace VSharp

open System
open System.Collections.Generic
open System.IO
open System.Xml.Serialization

[<CLIMutable>]
[<Serializable>]
[<XmlInclude(typeof<structureRepr>)>]
[<XmlInclude(typeof<arrayRepr>)>]
[<XmlInclude(typeof<referenceRepr>)>]
[<XmlInclude(typeof<pointerRepr>)>]
[<XmlInclude(typeof<enumRepr>)>]
[<XmlInclude(typeof<stringRepr>)>]
type commonTestInfo = {
    memory : memoryRepr
    typeMocks : typeMockRepr array
    extraAssemblyLoadDirs : string array
    externMocks : ResizeArray<extMockRepr>
    isError : bool
    isFatalError : bool
    throwsException : typeRepr
    errorMessage : string
}

[<AbstractClass>]
type Test (mockStorage : MockStorage, testInfoType : Type) =
    let mutable extraAssemblyLoadDirs : string list = [Directory.GetCurrentDirectory()]
    let setViaReflection target propertyName newValue =
        let t = target.GetType()
        let p = t.GetProperty(propertyName)
        p.SetValue(target, newValue)

    abstract HasExternMocks: bool with get
    abstract HasOutMocks: bool with get

    abstract IsError: bool with get, set
    abstract IsFatalError: bool with get, set
    abstract ErrorMessage: string with get, set
    abstract Exception: Type with get, set

    abstract AddExternMock: extMockRepr -> unit
    abstract ApplyExternMocks: string -> unit
    abstract ReverseExternMocks: unit -> unit

    abstract MemoryGraph: MemoryGraph with get

    abstract Serialize: string -> unit
    abstract Common: commonTestInfo with get

    abstract BoxedLocations: HashSet<physicalAddress> with get
    abstract Info: obj with get

    default x.IsError
        with get() = x.Common.isError
        and set (e : bool) = setViaReflection x.Common "isError" e
    default x.IsFatalError
        with get() = x.Common.isFatalError
        and set (e : bool) = setViaReflection x.Common "isFatalError" e
    default x.ErrorMessage
        with get() = x.Common.errorMessage
        and set (m : string) = setViaReflection x.Common "errorMessage" m
    default x.Exception
        with get() =
            if x.Common.throwsException = {assemblyName = null; moduleFullyQualifiedName = null; name = null; genericArgs = null} then null
            else x.Common.throwsException.Decode()
        and set (e : Type) = setViaReflection x.Common "throwsException" <| typeRepr.Encode e
    default x.HasExternMocks with get() = x.Common.externMocks.Count > 0
    default x.HasOutMocks with get() = x.Common.typeMocks |> Array.exists (fun (m : typeMockRepr) -> not <| Array.isEmpty m.outImplementations)
    default x.BoxedLocations with get() = x.MemoryGraph.BoxedLocations()
    default x.AddExternMock extMock =
        x.Common.externMocks.Add extMock
    default x.ApplyExternMocks(testName : string) =
        for externMock in x.Common.externMocks do
            let extMock = externMock.Decode()
            ExtMocking.buildAndPatch testName x.MemoryGraph.DecodeValue extMock
    default x.ReverseExternMocks() =
        if x.HasExternMocks then ExtMocking.unPatch()

    default x.Serialize(destination : string) =
        let common = x.Common
        x.MemoryGraph.Serialize common.memory
        setViaReflection common "extraAssemblyLoadDirs" (Array.ofList extraAssemblyLoadDirs)
        let typeMocks =
            mockStorage.TypeMocks.ToArray()
            |> Array.map (fun m -> typeMockRepr.Encode m x.MemoryGraph.Encode)
        setViaReflection common "typeMocks" typeMocks
        setViaReflection common "externMocks" common.externMocks

        let serializer = XmlSerializer testInfoType
        use stream = File.Create(destination)
        serializer.Serialize(stream, x.Info)
    member x.AddExtraAssemblySearchPath path =
        if not <| List.contains path extraAssemblyLoadDirs then
            extraAssemblyLoadDirs <- path :: extraAssemblyLoadDirs
    member x.ExtraAssemblyLoadDirs with get() = extraAssemblyLoadDirs

    static member DeserializeTestInfo(stream : FileStream) =
        let serializer = XmlSerializer(typeof<'info>)
        try
            serializer.Deserialize(stream) :?> 'info
        with child ->
            let exn = InvalidDataException("Input test is incorrect", child)
            raise exn
