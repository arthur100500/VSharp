#nullable enable
using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using System.Text.Json;
using System.Threading;
using VSharp.Test;
using System.Text.Json;
using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.Http.Features;

namespace IntegrationTests
{
    [TestSvmFixture]
    public static class AspNet
    {
        public class Wallet
        {
            public int MoneyAmount { get; set; }

            public void SetMoney(int newAmount)
            {
                MoneyAmount = newAmount;
            }
        }

        public class KeyBundle
        {
            public List<string> Keys;
        }

        [TestSvm(100)]
        public static int JsonSerialize(Wallet symbolicObject)
        {
            var memoryStream = new MemoryStream();

            JsonSerializer.SerializeAsync(memoryStream, symbolicObject).Wait();
            memoryStream.Seek(0, SeekOrigin.Begin);

            var deserializedObject = JsonSerializer.DeserializeAsync(
                memoryStream,
                typeof(Wallet),
                JsonSerializerOptions.Default,
                CancellationToken.None
            );

            var amount = ((Wallet)deserializedObject.Result!).MoneyAmount;

            if (amount < 0)
                throw new ArgumentException(nameof(amount));

            return amount;
        }

        [TestSvm(expectedCoverage: 100)]
        public static Wallet FeatureCollectionSetGet(Wallet symbolic)
        {
            symbolic.MoneyAmount = 21;

            var featureCollection = new FeatureCollection();
            featureCollection.Set<Wallet>(symbolic);
            var passedThrough = featureCollection.Get<Wallet>();

            return passedThrough;
        }

        [TestSvm(expectedCoverage: 100)]
        public static Wallet DictionaryPassThrough(Wallet symbolic)
        {
            symbolic.MoneyAmount = 32;

            var dictionary = new Dictionary<Type, object>();
            dictionary[typeof(Wallet)] = symbolic;
            var passedThrough = (Wallet)dictionary[typeof(Wallet)];

            return passedThrough;
        }

        [TestSvm(expectedCoverage: 100)]
        public static IHttpRequestFeature? FeatureReferences(string path)
        {
            var requestFeature = new HttpRequestFeature();

            requestFeature.Body = new MemoryStream();
            requestFeature.Path = path;
            requestFeature.Method = "GET";

            var features = new FeatureCollection();
            features.Set<IHttpRequestFeature>(requestFeature);

            // Code similar to one found in DefaultHttpRequest
            Func<IFeatureCollection, IHttpRequestFeature?> nullRequestFeature = _ => null;
            var featureReferences = new FeatureReferences<FeatureInterfaces>();
            featureReferences.Initalize(features);

            var fetchedRequestFeature = featureReferences.Fetch(ref featureReferences.Cache.Request, nullRequestFeature);

            return fetchedRequestFeature;
        }

        [TestSvm(expectedCoverage: 100)]
        public static PathString ConstructContextGetPath(string path)
        {
            ArgumentNullException.ThrowIfNull(path);

            var requestFeature = new HttpRequestFeature();
            requestFeature.Path = path;

            var features = new FeatureCollection();
            features.Set<IHttpRequestFeature>(requestFeature);

            var context = new DefaultHttpContext(features);

            var result = context.Request.Path;

            return result;
        }

        [TestSvm(expectedCoverage: 100)]
        public static PathString ConstructContextGetPathConcrete()
        {
            return ConstructContextGetPath("/get");
        }

        [TestSvm(expectedCoverage: 100)]
        public static bool ConstructContextIsRequestNull(string path)
        {
            ArgumentNullException.ThrowIfNull(path);

            var requestFeature = new HttpRequestFeature();
            requestFeature.Path = path;

            var features = new FeatureCollection();
            features.Set<IHttpRequestFeature>(requestFeature);

            var context = new DefaultHttpContext(features);

            var result = context.Request;

            return result is null;
        }

#pragma warning disable CS0649
        struct FeatureInterfaces
        {
            public IHttpRequestFeature? Request;
            public IQueryFeature? Query;
            public IFormFeature? Form;
            public IRequestCookiesFeature? Cookies;
            public IRouteValuesFeature? RouteValues;
            public IRequestBodyPipeFeature? BodyPipe;
        }
#pragma warning restore CS0649

        [TestSvm(expectedCoverage: 100)]
        public static int SetPropertyViaDelegate(Wallet symbolic)
        {
            var propertyInfo = typeof(Wallet).GetProperty("MoneyAmount")!;
            var setMethod = propertyInfo.SetMethod;
            var propertySetter = (object target, object result) => { setMethod.Invoke(target, new [] {result}); };
            propertySetter(symbolic, 32);
            return symbolic.MoneyAmount;
        }

        [TestSvm(expectedCoverage: 100)]
        public static int SetPropertyViaSetter(Wallet symbolic)
        {
            var propertyInfo = typeof(Wallet).GetProperty("MoneyAmount")!;
            var setMethod = propertyInfo.SetMethod;
            setMethod!.Invoke(symbolic, new object?[] { 340597 });
            return symbolic.MoneyAmount;
        }

        [TestSvm(expectedCoverage: 100)]
        public static int CallInvoke()
        {
            var valueToMutate = 0;
            var a = () => { valueToMutate = 3241; };
            a.Invoke();
            return valueToMutate;
        }

        [TestSvm(expectedCoverage: 100)]
        public static int CallInvokeWithArgument()
        {
            var valueToMutate = 0;
            var a = (int _) => { valueToMutate = 3241; };
            a.Invoke(3);
            return valueToMutate;
        }

        [TestSvm(expectedCoverage: 100)]
        public static int CallInvokeWithThis(Wallet symbolic)
        {
            var setMoneyMethod = typeof(Wallet).GetMethod("SetMoney");
            setMoneyMethod!.Invoke(symbolic, new object[] { 77137 });
            return symbolic.MoneyAmount;
        }

        public class TestClass
        {
            private int k = 22;
            public int Method(object x, int y, string z)
            {
                Console.WriteLine(x.GetType().ToString());
                Console.WriteLine(z);
                return y + k;
            }

            public int AddOne(int x)
            {
                if (x > 2) throw new ArgumentException();

                return x + (k / k);
            }
        }

        [TestSvm(expectedCoverage: 100)]
        public static int CallInvokeWithBoxed()
        {
            var arguments = new object[] {new(), 5, "hi"};
            var classWithMethodMethod = typeof(TestClass).GetMethod("Method")!;
            var classWithMethod = new TestClass();
            var result = classWithMethodMethod.Invoke(classWithMethod, arguments);

            return (int)result!;
        }


        [TestSvm(expectedCoverage: 100)]
        public static int CallInvokeWithBoxedSymbolic(object? arg1, object? arg2, object? arg3)
        {
            if (arg1 is null || arg2 is null || arg3 is null)
                return -1;

            var arguments = new object[3];

            arguments[0] = arg1;
            arguments[1] = arg2;
            arguments[2] = arg3;

            var classWithMethodMethod = typeof(TestClass).GetMethod("Method")!;
            var classWithMethod = new TestClass();
            var result = classWithMethodMethod.Invoke(classWithMethod, arguments);

            return (int)result!;
        }

        [TestSvm(expectedCoverage: 100)]
        public static int CallInvokeWithSymbolicInteger(int arg)
        {
            var classWithMethodMethod = typeof(TestClass).GetMethod("AddOne")!;
            var classWithMethod = new TestClass();

            var result = classWithMethodMethod.Invoke(classWithMethod, new object[] { arg });
            return (int)result!;
        }

        [TestSvm(expectedCoverage: 100)]
        public static int CallInvokeAfterJson(int arg)
        {
            var memoryStream = new MemoryStream();
            JsonSerializer.SerializeAsync(memoryStream, arg).Wait();
            // var argResult = JsonSerializer.DeserializeAsync(memoryStream, )


            var classWithMethodMethod = typeof(TestClass).GetMethod("AddOne")!;
            var classWithMethod = new TestClass();

            var result = classWithMethodMethod.Invoke(classWithMethod, new object[] { arg });
            return (int)result!;
        }

        [TestSvm(expectedCoverage: 65)]
        public static int JsonDeserializeThrowsExceptionOnTypeMismatch(int arg)
        {
            try
            {
                var memoryStream = new MemoryStream();
                JsonSerializer.SerializeAsync(memoryStream, arg).Wait();
                memoryStream.Position = 0;
                var result = JsonSerializer.DeserializeAsync(memoryStream, typeof(Wallet)).Result;
                return (int)result!;
            }
            catch (Exception _) // Needs to be this way as exception thrown and actual is currently different
            {
                return 25;
            }
        }

        [TestSvm(expectedCoverage: 100)]
        public static int JsonDeserializeReferenceType(Wallet arg)
        {
            var memoryStream = new MemoryStream();
            JsonSerializer.SerializeAsync(memoryStream, arg).Wait();
            memoryStream.Position = 0;
            var result = (Wallet)JsonSerializer.DeserializeAsync(memoryStream, typeof(Wallet)).Result!;
            return result.MoneyAmount + 1;
        }

        [TestSvm(expectedCoverage: 100)]
        public static int JsonDeserializeValueType(int arg)
        {
            var memoryStream = new MemoryStream();
            JsonSerializer.SerializeAsync(memoryStream, arg).Wait();
            memoryStream.Position = 0;
            var result = (int)JsonSerializer.DeserializeAsync(memoryStream, typeof(int)).Result!;
            return result + 2;
        }

        [TestSvm(expectedCoverage: 100)]
        public static string MemoryStreamReadWrite(string arg)
        {
            var memoryStream = new MemoryStream();
            var writer = new StreamWriter(memoryStream);
            writer.Write(arg + "Hello world!");
            memoryStream.Position = 0;
            var reader = new StreamReader(memoryStream);

            return reader.ReadToEnd();
        }

        [TestSvm(expectedCoverage: 100)]
        public static Stream JsonSerializeGetsCorrectStream(Wallet arg)
        {
            if (arg.MoneyAmount < 0)
                return Stream.Null;

            var memoryStream = new MemoryStream();
            JsonSerializer.SerializeAsync(memoryStream, arg).Wait();
            return memoryStream;
        }

        [TestSvm(expectedCoverage: 100)]
        public static Stream JsonSerializeWithOptionsGetsCorrectStream(Wallet arg)
        {
            if (arg.MoneyAmount < 0)
                return Stream.Null;

            var options = new JsonSerializerOptions()
            {
                PropertyNameCaseInsensitive = true,
                PropertyNamingPolicy = JsonNamingPolicy.CamelCase
            };
            var memoryStream = new MemoryStream();
            JsonSerializer.SerializeAsync(memoryStream, arg, options).Wait();
            return memoryStream;
        }

        [TestSvm(expectedCoverage: 100)]
        public static Wallet MutateValuePassThrough(Wallet w)
        {
            w.MoneyAmount = w.MoneyAmount + 20;
            return w;
        }

        [TestSvm(expectedCoverage: 100)]
        public static Stream MutableSerializedPassThrough(Wallet w)
        {
            var result = MutateValuePassThrough(w);
            var memoryStream = new MemoryStream();
            JsonSerializer.SerializeAsync(memoryStream, result).Wait();
            return memoryStream;
        }
    }
}
