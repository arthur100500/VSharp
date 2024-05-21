namespace VSharp.System
open VSharp
open VSharp.Core
open VSharp.Interpreter.IL
open VSharp.Interpreter.IL.CilState

module ModelBinders =
    [<Implements("System.Threading.Tasks.Task Microsoft.AspNetCore.Mvc.ModelBinding.Binders.ComplexObjectModelBinder.BindModelAsync(Microsoft.AspNetCore.Mvc.ModelBinding.ModelBindingContext)")>]
    val complexObjectModelBinderBind : IInterpreter -> cilState -> term list -> cilState list
