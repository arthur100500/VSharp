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
type webTestInfo = {
    common : commonTestInfo
    assemblyPath : string
    sourceProjectPath : string
    requestPath : string
    requestMethod : string
    requestBody: string
    responseBody: string
    responseStatusCode: int32
}
with
    static member Empty() = {
        common = {
            isError = false
            isFatalError = false
            throwsException = {assemblyName = null; moduleFullyQualifiedName = null; name = null; genericArgs = null}
            memory = {objects = Array.empty; types = Array.empty}
            typeMocks = Array.empty
            extraAssemblyLoadDirs = Array.empty
            externMocks = ResizeArray<extMockRepr>()
            errorMessage = null
        }
        assemblyPath = null
        sourceProjectPath = null
        requestPath = null
        requestMethod = null
        requestBody = null
        responseBody = null
        responseStatusCode = 0
    }

type AspIntegrationTest private (info: webTestInfo, mockStorage: MockStorage, createCompactRepr : bool) =
    inherit ATest(mockStorage, typeof<webTestInfo>)
    let common = info.common
    let memoryGraph = MemoryGraph(common.memory, mockStorage, createCompactRepr)
    let setViaReflection target propertyName newValue =
        let t = target.GetType()
        let p = t.GetProperty(propertyName)
        p.SetValue(target, newValue)
    override this.Common = common
    override this.MemoryGraph = memoryGraph
    override x.FileExtension with get() = "vswt"
    override this.Info = info
    new() = AspIntegrationTest(webTestInfo.Empty(), MockStorage(), false)
    member x.RequestBody
        with get() = info.requestBody
        and set (value : string) = setViaReflection info "requestBody" value
    member x.RequestPath
        with get() = info.requestPath
        and set (value : string) = setViaReflection info "requestPath" value
    member x.RequestMethod
        with get() = info.requestMethod
        and set (value : string) = setViaReflection info "requestMethod" value
    member x.ResponseBody
        with get() = info.responseBody
        and set (value : string) = setViaReflection info "responseBody" value
    member x.ResponseStatusCode
        with get() = info.responseStatusCode
        and set (value : int32) = setViaReflection info "responseStatusCode" value
    member x.assemblyPath
        with get() = info.assemblyPath
        and set (value : string) = setViaReflection info "assemblyPath" value

    static member DeserializeFromTestInfo(ti : webTestInfo, createCompactRepr : bool) =
        let mockStorage = MockStorage()
        mockStorage.Deserialize ti.common.typeMocks
        AspIntegrationTest(ti, mockStorage, createCompactRepr)

    static member Deserialize(stream : FileStream) =
        let testInfo = AspIntegrationTest.DeserializeTestInfo(stream)
        AspIntegrationTest.DeserializeFromTestInfo(testInfo, false)

    static member Deserialize(source : string) =
        use stream = new FileStream(source, FileMode.Open, FileAccess.Read)
        AspIntegrationTest.Deserialize stream

    override x.SetExpected(value) =
        let httpResponse = x.MemoryGraph.DecodeValue(value)
        failwith "todo"
