namespace VSharp

open System
open System.IO
open System.Text
open System.Text.Json
open System.Xml.Serialization
open FSharpx.Collections
open Microsoft.AspNetCore.Http.Features
open VSharp

[<CLIMutable>]
[<Serializable>]
type kvpRepr = {
    key: string
    value: string
}

[<CLIMutable>]
[<Serializable>]
[<XmlInclude(typeof<structureRepr>)>]
[<XmlInclude(typeof<arrayRepr>)>]
[<XmlInclude(typeof<referenceRepr>)>]
[<XmlInclude(typeof<pointerRepr>)>]
[<XmlInclude(typeof<enumRepr>)>]
[<XmlInclude(typeof<stringRepr>)>]
[<XmlInclude(typeof<kvpRepr>)>]
type webTestInfo = {
    common : commonTestInfo
    assemblyPath : string
    sourceProjectPath : string
    requestPath : string
    requestMethod : string
    requestHeaders: kvpRepr ResizeArray
    requestQuery: kvpRepr ResizeArray
    requestForm: kvpRepr ResizeArray
    requestFormBoundary: string
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
        requestHeaders = ResizeArray()
        requestQuery = ResizeArray()
        requestForm = ResizeArray()
        requestFormBoundary = null
        responseStatusCode = 0
    }

type AspIntegrationTest private (info: webTestInfo, mockStorage: MockStorage, createCompactRepr : bool) =
    inherit ATest(mockStorage, typeof<webTestInfo>)
    let common = info.common
    let mutable memoryGraph = MemoryGraph(common.memory, mockStorage, createCompactRepr)
    let setViaReflection target propertyName newValue =
        let t = target.GetType()
        let p = t.GetProperty(propertyName)
        p.SetValue(target, newValue)
    let readStream (stream : Stream) =
        stream.Position <- 0;
        use reader = new StreamReader(stream, Encoding.UTF8)
        reader.ReadToEnd();

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
    member x.RequestFormBoundary
        with get() = info.requestFormBoundary
        and set (value : string) = setViaReflection info "requestFormBoundary" value
    member x.RequestForm
        with get() = info.requestForm
    member x.RequestQuery
        with get() = info.requestQuery
    member x.RequestHeaders
        with get() = info.requestHeaders
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
    override x.RefreshMemoryGraph() =
        let mem = x.Common.memory
        x.MemoryGraph.Serialize(mem)
        memoryGraph <- MemoryGraph(mem, mockStorage, false)

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

    override x.Expected
        with set(value) = ()
        and get() = (x.ResponseBody, x.ResponseStatusCode)
