#nullable enable
using System;
using System.Collections.Generic;
using System.IO;
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
    }
}
