using System;
using System.IO;
using System.Reflection;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Hosting.Server;
using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.Http.Features;


namespace VSharp.CSharpUtils;

public class AspNet
{
    [Implements("System.Threading.Tasks.Task Microsoft.AspNetCore.Server.Kestrel.Core.KestrelServerImpl.StartAsync(this, Microsoft.AspNetCore.Hosting.Server.IHttpApplication`1[TContext], System.Threading.CancellationToken)")]
    public static Task StartKestrelServer(object t, object application, object cancellationToken)
    {
        return Task.CompletedTask;
    }

    [Implements("System.Action`2[System.Object,System.Object] Microsoft.Extensions.Internal.PropertyHelper.MakeFastPropertySetter(System.Reflection.PropertyInfo)")]
    public static Action<object, object> MakeFastPropertySetter(PropertyInfo pi)
    {
        var setMethod = pi.SetMethod;

        return (target, result) => { setMethod.Invoke(target, new [] {result}); };
    }

    [Implements("System.Func`2[System.Object,System.Object] Microsoft.Extensions.Internal.PropertyHelper.MakeFastPropertyGetter(System.Reflection.PropertyInfo, System.Reflection.MethodInfo, System.Reflection.MethodInfo)")]
    public static Func<object, object> MakeFastPropertyGetter(PropertyInfo pi, object refWrapper, object wrapper)
    {
        var getMethod = pi.GetMethod;

        return target => getMethod.Invoke(target, System.Array.Empty<object>());
    }

    [Implements("System.Void System.Diagnostics.DiagnosticListener.Write(this, System.String, System.Object)")]
    public static void DiagnosticListenerWrite(object t, object str, object obj)
    {

    }

    [Implements("System.Threading.Tasks.Task`1[Microsoft.AspNetCore.Http.IFormCollection] Microsoft.AspNetCore.Http.DefaultHttpRequest.ReadFormAsync(this, System.Threading.CancellationToken)")]
    public static Task<object> ReadFormAsync(CancellationToken token)
    {
        return Task.FromResult<object>(null);
    }

    /// <summary>
    /// Removes some validation steps of a form being read.
    /// Form and other data will be read from the cilState arguments
    /// </summary>
    /// <returns></returns>
    [Implements("System.Threading.Tasks.Task Microsoft.AspNetCore.Mvc.ModelBinding.FormValueProviderFactory.CreateValueProviderAsync(this, Microsoft.AspNetCore.Mvc.ModelBinding.ValueProviderFactoryContext)")]
    public static Task FormValueProviderFactoryCreateValueProviderAsync()
    {
        return Task.CompletedTask;
    }
    /// <summary>
    /// Removes some validation steps of a jquery data being read.
    /// Form and other data will be read from the cilState arguments
    /// </summary>
    /// <returns></returns>
    [Implements("System.Threading.Tasks.Task Microsoft.AspNetCore.Mvc.ModelBinding.JQueryFormValueProviderFactory.CreateValueProviderAsync(this, Microsoft.AspNetCore.Mvc.ModelBinding.ValueProviderFactoryContext)")]
    public static Task JQueryFormValueProviderFactoryCreateValueProviderAsync()
    {
        return Task.CompletedTask;
    }

    /// <summary>
    /// Removes some validation steps of a jquery data being read.
    /// Form and other data will be read from the cilState arguments
    /// </summary>
    /// <returns></returns>
    [Implements("System.Threading.Tasks.Task Microsoft.AspNetCore.Mvc.ModelBinding.FormFileValueProviderFactory.CreateValueProviderAsync(this, Microsoft.AspNetCore.Mvc.ModelBinding.ValueProviderFactoryContext)")]
    public static Task FormFileValueProviderFactoryCreateValueProviderAsync()
    {
        return Task.CompletedTask;
    }
}
