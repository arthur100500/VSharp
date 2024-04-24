namespace VSharp

open System
open System.Collections.Generic
open System.IO
open VSharp

type UnitTests(outputDir : string) =
    let testPrefix = "VSharp.tests."
    let mutable testNumber = 0u
    let mutable errorNumber = 0u
    let rootDir = Directory.CreateDirectory(if String.IsNullOrWhiteSpace outputDir then Directory.GetCurrentDirectory() else outputDir)
    let mutable currentDir = rootDir

    let () =
        let testDirs = HashSet<string>()
        rootDir.EnumerateDirectories(testPrefix + "*") |> Seq.iter (fun dir -> dir.Name |> testDirs.Add |> ignore)
        let uniqueName = Seq.initInfinite id |> Seq.pick (fun i ->
            let name = testPrefix + i.ToString()
            if testDirs.Contains name then None else Some name)
        currentDir <- rootDir.CreateSubdirectory(uniqueName)
        let linkName = $"%s{rootDir.FullName}%c{Path.DirectorySeparatorChar}%s{testPrefix}last"
        FileSystem.createSymlink currentDir.FullName linkName

    let generateTest (test : ATest) (name : string) =
        let testExtension = test.FileExtension
        test.Serialize $"%s{currentDir.FullName}%c{Path.DirectorySeparatorChar}%s{name}%s{testExtension}"

    interface IDisposable with
        override x.Dispose() =
            ()

    member x.GenerateTest (test : ATest) =
        testNumber <- testNumber + 1u
        generateTest test ("test" + testNumber.ToString())

    member x.GenerateError (test : ATest) =
        errorNumber <- errorNumber + 1u
        generateTest test ("error" + errorNumber.ToString())

    member x.WriteReport (reporter : Action<TextWriter>) =
        let reportFileName = $"%s{currentDir.FullName}%c{Path.DirectorySeparatorChar}report.txt"
        use stream = new FileStream(reportFileName, FileMode.OpenOrCreate, FileAccess.Write)
        use writer = new StreamWriter(stream)
        reporter.Invoke writer

    member x.TestDirectory with get() = currentDir

    member x.UnitTestsCount with get() = testNumber
    member x.ErrorsCount with get() = errorNumber
