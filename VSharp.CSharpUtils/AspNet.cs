using System;
using System.IO;
using System.Reflection;
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
}
