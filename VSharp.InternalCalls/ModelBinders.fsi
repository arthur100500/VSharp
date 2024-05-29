namespace VSharp.System
open VSharp
open VSharp.Core
open VSharp.Interpreter.IL
open VSharp.Interpreter.IL.CilState

module ModelBinders =
    [<Implements("System.Threading.Tasks.ValueTask`1[Microsoft.AspNetCore.Mvc.ModelBinding.ModelBindingResult] Microsoft.AspNetCore.Mvc.ModelBinding.ParameterBinder.BindModelAsync(this, Microsoft.AspNetCore.Mvc.ActionContext, Microsoft.AspNetCore.Mvc.ModelBinding.IModelBinder, Microsoft.AspNetCore.Mvc.ModelBinding.IValueProvider, Microsoft.AspNetCore.Mvc.Abstractions.ParameterDescriptor, Microsoft.AspNetCore.Mvc.ModelBinding.ModelMetadata, System.Object, System.Object)")>]
    val complexObjectModelBinderBind : IInterpreter -> cilState -> term list -> Method -> cilState list
