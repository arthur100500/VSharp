#nullable enable
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Net.Http;
using System.Reflection;
using System.Runtime.Serialization;
using System.Text;
using System.Text.Json;
using Microsoft.AspNetCore.Hosting;
using Microsoft.AspNetCore.Http.Features;
using Microsoft.AspNetCore.Mvc.Testing;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.Logging;
using static VSharp.TestExtensions.ObjectsComparer;

namespace VSharp.TestRunner
{
    public static class TestRunner
    {
        private static unsafe bool CheckResult(object? expected, object? got)
        {
            return (expected, got) switch
            {
                (Pointer x, Pointer y) => CompareObjects(Pointer.Unbox(x), Pointer.Unbox(y)),
                (null, Pointer y) => CompareObjects(null, Pointer.Unbox(y)),
                (Pointer x, null) => CompareObjects(Pointer.Unbox(x), null),
                _ => CompareObjects(expected, got)
            };
        }

        private static bool ShouldInvoke(ATest test, SuiteType suiteType, bool fileMode = false)
        {
            var shouldInvoke = suiteType switch
            {
                SuiteType.TestsOnly => !test.IsError || fileMode,
                SuiteType.ErrorsOnly => test.IsError || fileMode,
                SuiteType.TestsAndErrors => !test.IsFatalError || fileMode,
                _ => false
            };

            return shouldInvoke;
        }


        private static bool ReproduceInitializedTest(
            ATest test,
            SuiteType suiteType,
            bool checkResult,
            FileInfo fileInfo,
            Func<ATest, object?> run,
            Func<ATest, object?, bool> checkResultFunc,
            bool fileMode = false)
        {
            var ex = test.Exception;
            try
            {
                object? result;
                var shouldInvoke = ShouldInvoke(test, suiteType, fileMode);
                if (shouldInvoke)
                {
                    test.ApplyExternMocks(fileInfo.Name);
                    result = run(test);
                    test.ReverseExternMocks(); // reverses if ex was not thrown
                }
                else
                {
                    Console.ForegroundColor = ConsoleColor.White;
                    Console.WriteLine("Test {0} ignored.", fileInfo.Name);
                    Console.ResetColor();
                    return true;
                }
                if (ex != null)
                {
                    Console.ForegroundColor = ConsoleColor.Red;
                    Console.Error.WriteLine("Test {0} failed! The expected exception {1} was not thrown",
                        fileInfo.Name, ex);
                    Console.ResetColor();
                    return false;
                }
                if (checkResult && !test.IsError && !checkResultFunc(test, result))
                {
                    // TODO: use NUnit?
                    Console.ForegroundColor = ConsoleColor.Red;
                    Console.Error.WriteLine("Test {0} failed! Expected {1}, but got {2}", fileInfo.Name,
                        test.Expected ?? "null",
                        result ?? "null");
                    Console.ResetColor();
                    return false;
                }
            }
            catch (TargetInvocationException e)
            {
                test.ReverseExternMocks(); // reverses if ex was thrown
                var exceptionExpected = e.InnerException != null && e.InnerException.GetType() == ex;
                if (exceptionExpected || test.IsError && suiteType == SuiteType.TestsAndErrors && !fileMode) {
                    Console.ForegroundColor = ConsoleColor.Green;
                    var exceptionType = e.InnerException?.GetType().FullName;
                    Console.WriteLine($"Test {fileInfo.Name} throws the expected exception {exceptionType}!");
                    Console.ResetColor();
                }
                else if (e.InnerException != null && ex != null)
                {
                    Console.ForegroundColor = ConsoleColor.Red;
                    Console.Error.WriteLine($"Test {fileInfo.Name} throws {e.InnerException} when the expected exception was {ex}!");
                    Console.ResetColor();
                    throw e.InnerException;
                }
                else throw;
            }
            finally
            {
                test.ReverseExternMocks();
            }
            Console.ForegroundColor = ConsoleColor.Green;
            Console.Out.WriteLine($"{fileInfo.Name} passed!");
            Console.ResetColor();

            return true;
        }

        private static object RunAspNetTest(ATest rawTest)
        {
            var test = (AspIntegrationTest)rawTest;
            // Should be stored in .vswt
            var assemblyPath = @"C:\Users\arthur\RiderProjects\AspNetApps\CatPhotoApi\CatPhotoApi\CatPhotoApi\bin\Release\net7.0\win-x64\publish\CatPhotoApi.dll";
            var sourceProjectPath = @"C:\Users\arthur\RiderProjects\AspNetApps\CatPhotoApi\CatPhotoApi";
            var depsPath = @"C:\Users\arthur\RiderProjects\AspNetApps\CatPhotoApi\CatPhotoApi\CatPhotoApi\bin\Release\net7.0\win-x64\publish\CatPhotoApi.deps.json";
            var requestPath = test.RequestPath;
            var requestMethod = test.RequestMethod;
            var requestBody = test.RequestBody;

            // Copy .deps file in order to create client
            var catPhotoAssembly = Assembly.LoadFrom(assemblyPath);
            var catPhotoDeps = File.ReadAllText(depsPath);
            File.WriteAllText(Path.GetFileName(depsPath), catPhotoDeps);

            var typeFromAssembly = catPhotoAssembly.GetTypes().First(x => x.Name == "Program");
            var factoryType = typeof(WebApplicationFactory<>).MakeGenericType(typeFromAssembly);
            var factory = Activator.CreateInstance(factoryType);

            // Configure factory (Maybe unnecessary)
            var withWebHostBuilderMethod = factoryType.GetMethod("WithWebHostBuilder");

            void WebHostBuilder(IWebHostBuilder builder)
            {
                builder
                    .UseSetting("contentRoot", sourceProjectPath)
                    .UseEnvironment("Production")
                    .UseContentRoot(sourceProjectPath)
                    .ConfigureLogging(o => o.AddFilter(logLevel => logLevel >= LogLevel.Warning))
                    .UseConfiguration(new ConfigurationBuilder().SetBasePath(sourceProjectPath).Build());
            }

            factory = withWebHostBuilderMethod!.Invoke(factory, new object[] { (Action<IWebHostBuilder>)WebHostBuilder });

            Environment.CurrentDirectory = sourceProjectPath;

            // Write assembly under tests in MvcTestingAppManifest.json
            const string manifestPath = "./MvcTestingAppManifest.json";
            var mvcTestingManifestExists = File.Exists(manifestPath);
            if (!mvcTestingManifestExists) File.WriteAllText(manifestPath, "{ }");
            var mvcTestingManifestEntries = JsonSerializer.Deserialize<IDictionary<string, string>>(File.ReadAllBytes(manifestPath))!;
            mvcTestingManifestEntries.TryAdd(catPhotoAssembly.FullName!, sourceProjectPath);
            var indentedOption = new JsonSerializerOptions { WriteIndented = true };
            var mvcTestingManifestEntriesSerialized = JsonSerializer.Serialize(mvcTestingManifestEntries, indentedOption);
            File.WriteAllText(manifestPath, mvcTestingManifestEntriesSerialized);

            // Create client and send request
            var createClientMethod = factoryType.GetMethods().First(x => x.Name.Contains("CreateClient"));
            var client = (HttpClient)createClientMethod.Invoke(factory, System.Array.Empty<object>())!;

            var requestBodyText = JsonSerializer.Serialize(requestBody);
            var content = new StringContent(requestBodyText, Encoding.UTF8, "application/json");
            var requestMethodTyped = requestMethod switch
            {
                "POST" => HttpMethod.Post,
                "GET" => HttpMethod.Get,
                "DELETE" => HttpMethod.Delete,
                // TODO: Other
                _ => null
            };

            var message = new HttpRequestMessage(requestMethodTyped!, requestPath);
            message.Content = content;
            var response = client.SendAsync(message).Result;
            return response;
        }

        private static bool CheckAspNetResult(ATest testRaw, object resultRaw)
        {
            var test = (AspIntegrationTest)testRaw;
            var result = (HttpResponseMessage)resultRaw;
            // Currently checking only status code and body.
            // TODO: Check headers and other components of response
            var expectedStatusCode = test.ResponseStatusCode;
            var resultStatusCode = (int)result.StatusCode;
            if (!CheckResult(expectedStatusCode, resultStatusCode)) return false;

            var expectedBody = (string)test.ResponseBody;
            var resultBody = result.Content.ReadAsStringAsync().Result;

            return expectedBody == resultBody;
        }

        private static bool ReproduceWebTest(FileInfo fileInfo, SuiteType suiteType, bool checkResult, bool fileMode = false)
        {
            try
            {
                using var stream = new FileStream(fileInfo.FullName, FileMode.Open, FileAccess.Read);
                var ti = ATest.DeserializeTestInfo<webTestInfo>(stream);
                AssemblyManager.SetDependenciesDirs(ti.common.extraAssemblyLoadDirs);
                var test = AspIntegrationTest.DeserializeFromTestInfo(ti, false);

                var requestMethod = test.RequestMethod;
                var requestPath = test.RequestPath;
                var requestBody = test.RequestBody;
                var responseBody = test.ResponseBody;
                var responseStatusCode = test.ResponseStatusCode;

                Console.Out.WriteLine($"Starting reproducing {fileInfo.Name} ({requestMethod} {requestPath})");
                if (!checkResult) Console.Out.WriteLine("Result check is disabled");
                if (suiteType == SuiteType.TestsOnly) Console.Out.WriteLine("Error reproducing is disabled");

                return ReproduceInitializedTest(test, suiteType, checkResult, fileInfo, RunAspNetTest, CheckAspNetResult, fileMode);
            }
            catch (Exception e)
            {
                Console.ForegroundColor = ConsoleColor.Red;
                Console.Error.WriteLine($"Error ({fileInfo.FullName}): {e}");
                Console.ResetColor();
                return false;
            }
        }

        private static bool ReproduceTest(FileInfo fileInfo, SuiteType suiteType, bool checkResult, bool fileMode = false)
        {
            try
            {
                using var stream = new FileStream(fileInfo.FullName, FileMode.Open, FileAccess.Read);
                var ti = UnitTest.DeserializeTestInfo<testInfo>(stream);

                AssemblyManager.SetDependenciesDirs(ti.common.extraAssemblyLoadDirs);
                var test = UnitTest.DeserializeFromTestInfo(ti, false);

                var method = test.Method;

                var methodName = Reflection.getFullMethodName(method);
                Console.Out.WriteLine($"Starting reproducing {fileInfo.Name} for method {methodName}");
                if (!checkResult)
                    Console.Out.WriteLine("Result check is disabled");
                if (suiteType == SuiteType.TestsOnly)
                    Console.Out.WriteLine("Error reproducing is disabled");
                var parameters = test.Args ?? method.GetParameters()
                    .Select(t => FormatterServices.GetUninitializedObject(t.ParameterType)).ToArray();

                return ReproduceInitializedTest(test, suiteType, checkResult, fileInfo, Run, CheckUnitTest, fileMode);

                object? Run(ATest target) => method.Invoke(((UnitTest)target).ThisArg, parameters);

                bool CheckUnitTest(ATest target, object result) => CheckResult(((UnitTest)target).Expected, result);

            }
            catch (Exception e)
            {
                Console.ForegroundColor = ConsoleColor.Red;
                Console.Error.WriteLine($"Error ({fileInfo.FullName}): {e}");
                Console.ResetColor();
                return false;
            }
        }

        public static bool ReproduceTest(FileInfo file, bool checkResult)
        {
            return ReproduceTest(file, SuiteType.TestsAndErrors, checkResult, true);
        }

        public static bool ReproduceTests(DirectoryInfo testsDir, SuiteType suiteType = SuiteType.TestsAndErrors, bool recursive = false)
        {
            var unitTests = SearchByExtension("vst").ToList();;
            var aspIntegrationTests = SearchByExtension("vswt").ToList();;


            if (unitTests.Count == 0 && aspIntegrationTests.Count == 0)
            {
                Console.ForegroundColor = ConsoleColor.Red;
                Console.Error.WriteLine("No *.vst/*.vswt tests found in {0}", testsDir.FullName);
                Console.ResetColor();
                return false;
            }

            var result = true;

            foreach (var testFileInfo in unitTests)
                result &= ReproduceTest(testFileInfo, suiteType, true);

            foreach (var testFileInfo in aspIntegrationTests)
                result &= ReproduceWebTest(testFileInfo, suiteType, true);

            return result;

            IEnumerable<FileInfo> SearchByExtension(string extension) => testsDir.EnumerateFiles($"*.{extension}", recursive ? SearchOption.AllDirectories : SearchOption.TopDirectoryOnly);
        }
    }
}
