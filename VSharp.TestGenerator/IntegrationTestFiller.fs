namespace VSharp.Interpreter.IL

open System
open System.Collections
open System.Reflection
open System.Text.Json
open Mono.Cecil
open VSharp

module IntegrationTestFiller =

    type aspParameterSource =
        | FromBody
        | FromRoute
        | FromQuery
        | FromForm
        | FromHeader
        | Default

    let ocamlCaseJsonOptions =
        let options = JsonSerializerOptions()
        options.PropertyNamingPolicy <- JsonNamingPolicy.CamelCase
        options

    let jsonSerialize argument =
        JsonSerializer.Serialize(argument, options = ocamlCaseJsonOptions)

    let containsAttribute name (attributes : CustomAttributeData seq) =
        attributes
        |> Seq.tryFind (fun attrib -> attrib.AttributeType.FullName = $"Microsoft.AspNetCore.Mvc.{name}Attribute")
        |> Option.isSome


    // TODO: https://learn.microsoft.com/en-us/aspnet/core/mvc/models/model-binding?view=aspnetcore-8.0
    // TODO:   Property binders of controllers ([BindProperty], [BindProperties])
    let sourceOfAttributes attributes =
        if containsAttribute "FromBody" attributes then FromBody
        elif containsAttribute "FromForm" attributes then FromForm
        elif containsAttribute "FromQuery" attributes then FromQuery
        elif containsAttribute "FromHeader" attributes then FromHeader
        elif containsAttribute "FromRoute" attributes then FromRoute
        else Default

    let sourceOfParameterInfo (parameterInfo : ParameterInfo) =
        sourceOfAttributes parameterInfo.CustomAttributes

    // Gets name from custom attributes
    let getOverridingName (attributes: CustomAttributeData seq) =
        let modelNameProvider = "IModelNameProvider"
        let fetchName (source: CustomAttributeData) =
            let nameArgument =
                source.NamedArguments |> Seq.tryFind (fun x -> x.MemberName = "Name")
            nameArgument |> Option.map _.TypedValue.ToString()
        let containsModelNameProvider interfaces =
            Seq.tryFind (fun (i: Type) -> i.Name = modelNameProvider) interfaces
            |> Option.isSome
        let renames = attributes |> Seq.filter (fun a -> a.AttributeType.GetInterfaces() |> containsModelNameProvider)
        Seq.tryLast renames |> Option.bind fetchName

    // Simple types are just written in corresponding fields
    let setRequestPartSimple (test: AspIntegrationTest) (attributes: CustomAttributeData seq) object name =
        let converted = object |> jsonSerialize // TODO: For complex objects different serialization
        match sourceOfAttributes attributes with
        | Default ->
            // TODO: Think about auto-assigned sources of parameters in controllers
            test.RequestQuery <- $"{test.RequestQuery}\n{name}: {converted}"
        | FromBody -> test.RequestBody <- converted
        | FromForm -> test.RequestBody <- $"{test.RequestBody}\n{name}: {converted}"
        | FromQuery -> test.RequestQuery <- $"{test.RequestQuery}\n{name}: {converted}"
        | FromHeader -> test.RequestHeaders <- $"{test.RequestHeaders}\n{name}: {converted}"
        | FromRoute -> test.RequestPath <- $"{test.RequestPath}/{converted}"

    let rec setRequestPartsComplexProperties (test: AspIntegrationTest) object name =
        let properties = object.GetType().GetProperties()
        for property in properties do
            // TODO: bind or not bind? Need to filter smh
            let overridingName = getOverridingName property.CustomAttributes
            let propertyName = match overridingName with Some x -> x | None -> property.Name
            let propertyObject = property.GetMethod.Invoke(object, [||])
            setRequestPartInner test property.CustomAttributes propertyObject $"{name}.{propertyName}"

    // Property-wise setter
    and setRequestPartsComplex (test: AspIntegrationTest) attributes object name =
        // (From msdn) When [FromBody] is applied to a complex type parameter, any binding source attributes applied to its properties are ignored
        if (containsAttribute "FromBody" attributes)
        then test.RequestBody <- jsonSerialize object
        else setRequestPartsComplexProperties test object name

    and setRequestPartInner (test: AspIntegrationTest) (attributes: CustomAttributeData seq) (object : obj) name =
        match object with
        | null -> setRequestPartSimple test attributes object name
        | _ when object.GetType().IsValueType -> setRequestPartSimple test attributes object name
        | :? string -> setRequestPartSimple test attributes object name
        | :? IEnumerable -> internalfail "TODO: Lists and other sequences"
        | _ -> setRequestPartsComplex test attributes object name

    let setRequestPartForParameter (test: AspIntegrationTest) (parameterInfo: ParameterInfo) (object : obj) =
        let parameterName = getOverridingName parameterInfo.CustomAttributes
        let parameterName = match parameterName with Some x -> x | None -> parameterInfo.Name
        setRequestPartInner test parameterInfo.CustomAttributes object parameterName


