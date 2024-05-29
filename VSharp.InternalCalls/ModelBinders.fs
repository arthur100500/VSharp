namespace VSharp.System
open System.IO
open System.Reflection
open VSharp
open VSharp.Core
open VSharp.Interpreter.IL
open VSharp.Interpreter.IL.CilState

module ModelBinders =
    let miniCallInvoke (method : MethodInfo) args (cilState : cilState) =
        let invokeMethod = method |> Reflection.createInvokeMethod |> Application.getMethod
        Memory.InitFunctionFrame cilState.state invokeMethod None (Some args)
        Instruction(0<offsets>, invokeMethod) |> cilState.PushToIp
        List.singleton cilState

    let parameterBinderBindModelAsync (_: IInterpreter) (cilState : cilState) (args : term list) (method : Method) : cilState list =
        let concreteArgs = List.map (TryTermToObj cilState.state) args
        let modelBindingResultType = method.ReturnType.GenericTypeArguments[0]
        let bindingModelResultConstructor = modelBindingResultType.GetConstructor(enum<BindingFlags>(-1), [| typeof<obj>; typeof<bool> |])
        let controllerParameterDescriptor = concreteArgs[4].Value.GetType()
        let parameterInfoField = controllerParameterDescriptor |> Reflection.fieldsOf false |> Array.find (fun (x, y : FieldInfo) -> y.Name.Contains("ParameterInfo"))
        let parameterInfo = Memory.ReadField cilState.state args[4] (fst parameterInfoField)
        let parameterInfoField = (TypeOf parameterInfo) |> Reflection.fieldsOf false |> Array.find (fun (x, y : FieldInfo) -> y.Name.Contains("Position"))
        let parameterPosition = Memory.ReadField cilState.state parameterInfo (fst parameterInfoField)
        let argumentPosition = parameterPosition |> TryTermToObj cilState.state |> Option.get :?> int
        let argument = cilState.webExplorationArguments |> Seq.find (fun x -> x.Key.Position = argumentPosition) |> (fun x -> x.Value)
        let resultObject = bindingModelResultConstructor.Invoke([|null; true|])
        let resultTerm = Memory.ObjectToTerm cilState.state resultObject modelBindingResultType
        let modelBindingResultModelField = modelBindingResultType |> Reflection.fieldsOf false |>  Array.find (fun (x, y : FieldInfo) -> y.Name = "<Model>k__BackingField") |> fst
        let returnResultInValueTask (cilState : cilState) =
            let argument = cilState.Pop()
            let resultTerm = Memory.WriteStructField resultTerm modelBindingResultModelField argument
            let task = Json.valueTaskOfResult cilState false resultTerm modelBindingResultType
            cilState.Push task
            cilState
        if (TypeOf argument).IsValueType then
            let task = Json.valueTaskOfResult cilState false resultTerm modelBindingResultType
            cilState.Push task
            [cilState]
        else
            let statesWithCopiedArgument = Object.DeepCopy cilState argument
            statesWithCopiedArgument |> List.map returnResultInValueTask
