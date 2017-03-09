namespace VSharp

open System
open System.Collections.Generic
open System.Reflection

[<StructuralEquality;NoComparison>]
type public TermType =
    | Void
    | Bottom
    | Object
    | Bool
    | Numeric of System.Type
    | String
    | StructType of System.Type
    | ClassType of System.Type
    | Func of TermType list * TermType

    override this.ToString() =
        match this with
        | Void -> "void"
        | Bottom -> "exception"
        | Object -> "object"
        | Bool -> "bool"
        | Numeric t -> t.Name.ToLower()
        | String -> "string"
        | Func(domain, range) -> String.Join(" -> ", List.append domain [range])
        | StructType t -> t.ToString()
        | ClassType t -> t.ToString()

module public Types =
    let private integerTypes =
        new HashSet<System.Type>(
                          [typedefof<byte>; typedefof<sbyte>;
                           typedefof<int16>; typedefof<uint16>;
                           typedefof<int32>; typedefof<uint32>;
                           typedefof<int64>; typedefof<uint64>])

    let private realTypes =
        new HashSet<System.Type>([typedefof<single>; typedefof<double>; typedefof<decimal>])

    let private numericTypes = new HashSet<System.Type>(Seq.append integerTypes realTypes)

    let private primitiveTypes = new HashSet<Type>(Seq.append numericTypes [typedefof<bool>; typedefof<string>])

    let public ToDotNetType t =
        match t with
        | Object -> typedefof<obj>
        | Bool -> typedefof<bool>
        | Numeric res -> res
        | String -> typedefof<string>
        | StructType t -> t
        | ClassType t -> t
        | _ -> typedefof<obj>

    let rec public FromDotNetType t =
        match t with
        | b when b.Equals(typedefof<bool>) -> Bool
        | n when numericTypes.Contains(n) -> Numeric n
        | s when s.Equals(typedefof<string>) -> String
        | f when f.IsSubclassOf(typedefof<System.Delegate>) ->
            let methodInfo = f.GetMethod("Invoke") in
            let returnType = methodInfo.ReturnType |> FromDotNetType in
            let parameters = methodInfo.GetParameters() |> Array.map (fun (p : System.Reflection.ParameterInfo) -> FromDotNetType p.ParameterType) in
            Func(List.ofArray parameters, returnType)
        | s when s.IsValueType -> StructType t
        | c when c.IsClass -> ClassType t
        | _ -> __notImplemented__()

    let public FromMetadataType (t : JetBrains.Metadata.Reader.API.IMetadataType) =
        if t = null then Object
        else
            match t.AssemblyQualifiedName with
            | "__Null" -> Object
            | _ as qtn ->
                let dotNetType = Type.GetType(qtn) in
                if dotNetType = null then __notImplemented__()
                else FromDotNetType dotNetType

    let public MetadataToDotNetType (t : JetBrains.Metadata.Reader.API.IMetadataType) = t |> FromMetadataType |> ToDotNetType

    let public FromFunctionSignature (signature : JetBrains.Decompiler.Ast.IFunctionSignature) (returnMetadataType : JetBrains.Metadata.Reader.API.IMetadataType) =
        let returnType = FromMetadataType returnMetadataType in
        let paramToType (param : JetBrains.Decompiler.Ast.IMethodParameter) =
            param.Type |> FromMetadataType
        let args = Seq.map paramToType signature.Parameters |> List.ofSeq in
        Func(args, returnType)

    let public IsBool (t : TermType) =
        match t with
        | Bool -> true
        | _ -> false

    let public IsInteger = ToDotNetType >> integerTypes.Contains

    let public IsReal = ToDotNetType >> realTypes.Contains

    let public IsNumeric = function
        | Numeric _ -> true
        | _ -> false

    let public IsString = function
        | String -> true
        | _ -> false

    let public IsFunction = function
        | Func _ -> true
        | _ -> false

    let public IsClass = function
        | ClassType _ -> true
        | _ -> false

    let public IsStruct = function
        | StructType _ -> true
        | _ -> false

    let public IsObject = function
        | Object _ -> true
        | _ -> false

    let public IsVoid = function
        | Void -> true
        | _ -> false

    let public IsBottom = function
        | Bottom -> true
        | _ -> false

    let public IsReference t = IsClass t || IsObject t || IsFunction t

    let public IsPrimitive = ToDotNetType >> primitiveTypes.Contains

    let public DomainOf = function
        | Func(domain, _) -> domain
        | _ -> []

    let public RangeOf = function
        | Func(_, range) -> range
        | t -> t

    let public IsRelation = RangeOf >> IsBool

    let public GetMetadataTypeOfNode (node : JetBrains.Decompiler.Ast.INode) =
        DecompilerServices.getPropertyOfNode node "Type" null :?> JetBrains.Metadata.Reader.API.IMetadataType

    let public GetSystemTypeOfNode (node : JetBrains.Decompiler.Ast.INode) =
        let mt = GetMetadataTypeOfNode node in
        if mt = null then typedefof<obj>
        else ToDotNetType (FromMetadataType mt)

    let public GetFieldsOf (t : System.Type) =
        let fields = t.GetFields(BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.NonPublic) in
        fields |> Array.map (fun (field : FieldInfo) -> (field.Name, FromDotNetType field.FieldType)) |> Map.ofArray