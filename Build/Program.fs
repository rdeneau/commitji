// Adapted from Fantomas: https://github.com/fsprojects/fantomas

// Usage:
// - Run (default): dotnet run --project .\Build --no-restore
// - Help:          dotnet run --project .\Build --no-restore -- -h
// - FormatAll:     dotnet run --project .\Build --no-restore -- -p FormatAll
// - FormatChanged: dotnet run --project .\Build --no-restore -- -p FormatChanged
// - Build:         dotnet run --project .\Build --no-restore -- -p Build
// - PushClient:    dotnet run --project .\Build --no-restore -- -p PushClient
// - PublishAlpha:  dotnet run --project .\Build --no-restore -- -p PublishAlpha

open System
open System.IO
open Fun.Build
open CliWrap
open CliWrap.Buffered

let commitjiCli = "Commitji.Cli"

// -- Run ----

pipeline "Run" {
    workingDir __SOURCE_DIRECTORY__
    // stage "Debug" { echo $"working dir: %s{__SOURCE_DIRECTORY__}" }

    // 👇 Note the --no-restore → you need to run `dotnet restore` the first time
    stage "Run" { run "dotnet run --project ..\src\Commitji.Cli\ --no-restore" }

    runIfOnlySpecified false
}

// -- Build ----

let cleanFolders (input: string seq) =
    async {
        input
        |> Seq.iter (fun dir ->
            if Directory.Exists(dir) then
                Directory.Delete(dir, true)
        )
    }

pipeline "Build" {
    workingDir __SOURCE_DIRECTORY__
    stage "RestoreTools" { run "dotnet tool restore" }
    stage "Clean" { run (cleanFolders [| "artifacts" |]) }
    stage "CheckFormat" { run "dotnet fantomas build src tests --check" }
    stage "Build" { run "dotnet build -c Release --tl" }
    stage "UnitTests" { run "dotnet test -c Release --tl" }
    stage "Pack" { run $"dotnet pack ./src/%s{commitjiCli} --no-restore -c Release --tl" }
    runIfOnlySpecified true
}

// -- Format ----

let runGitCommand (arguments: string) =
    async {
        let! result =
            Cli.Wrap("git").WithArguments(arguments).WithWorkingDirectory(__SOURCE_DIRECTORY__).ExecuteBufferedAsync().Task
            |> Async.AwaitTask

        return result.ExitCode, result.StandardOutput, result.StandardError
    }

let runCmd file (arguments: string) =
    async {
        let! result = Cli.Wrap(file).WithArguments(arguments).ExecuteAsync().Task |> Async.AwaitTask
        return result.ExitCode
    }

pipeline "FormatChanged" {
    workingDir __SOURCE_DIRECTORY__

    stage "Format" {
        run (fun _ ->
            async {
                let! exitCode, stdout, _stdErr = runGitCommand "status --porcelain"

                if exitCode <> 0 then
                    return exitCode
                else
                    let files =
                        stdout.Split('\n')
                        |> Array.choose (fun line ->
                            let line = line.Trim()

                            if
                                (line.StartsWith("AM", StringComparison.Ordinal) || line.StartsWith("M", StringComparison.Ordinal))
                                && (line.EndsWith(".fs", StringComparison.Ordinal)
                                    || line.EndsWith(".fsx", StringComparison.Ordinal)
                                    || line.EndsWith(".fsi", StringComparison.Ordinal))
                            then
                                Some(line.Replace("AM ", "").Replace("MM ", "").Replace("M ", ""))
                            else
                                None
                        )
                        |> String.concat " "

                    if String.IsNullOrWhiteSpace(files) then
                        printfn "No files to format."
                        return 0
                    else
                        printfn $"Formatting files: %s{files}"
                        return! runCmd "dotnet" $"fantomas {files}"
            }
        )
    }

    runIfOnlySpecified true
}

pipeline "FormatAll" {
    workingDir __SOURCE_DIRECTORY__
    stage "Fantomas" { run "dotnet fantomas build src tests" }
    runIfOnlySpecified true
}

// -- Push / Publish ----

module String =
    let (|NullOrEmpty|NotNullOrEmpty|) (s: string) =
        if String.IsNullOrEmpty(s) then
            NullOrEmpty
        else
            NotNullOrEmpty s

let nugetKey =
    match Environment.GetEnvironmentVariable("NUGET_KEY") with
    | String.NotNullOrEmpty key -> key
    | String.NullOrEmpty ->
        try
            File.ReadAllText "nuget-key.txt"
        with _ ->
            ""

let pushPackage nupkg =
    async {
        let! result =
            Cli.Wrap("dotnet").WithArguments($"nuget push %s{nupkg} --api-key %s{nugetKey} --source https://api.nuget.org/v3/index.json").ExecuteAsync().Task
            |> Async.AwaitTask

        return result.ExitCode
    }

pipeline "PushClient" {
    workingDir __SOURCE_DIRECTORY__

    verify (fun _ ->
        printf "Verify the NuGet API key: "

        match nugetKey with
        | String.NullOrEmpty ->
            printfn "✖️ NUGET_KEY environment variable or .nuget-key file must be set"
            false
        | _ ->
            printfn "✔️ set"
            true
    )

    stage "Pack" { // ↩
        run $"dotnet pack ./src/%s{commitjiCli} -c Release --tl"
    }

    stage "Push" {
        run (fun _ ->
            async {
                return!
                    Directory.EnumerateFiles("artifacts/package/release", $"%s{commitjiCli}.*.nupkg", SearchOption.TopDirectoryOnly)
                    |> Seq.tryExactlyOne
                    |> Option.map pushPackage
                    |> Option.defaultValue (
                        async {
                            printfn $"%s{commitjiCli} package was not found."
                            return -1
                        }
                    )
            }
        )
    }

    runIfOnlySpecified true
}

pipeline "PublishAlpha" {
    workingDir __SOURCE_DIRECTORY__
    stage "Clean" { run (cleanFolders [| "artifacts" |]) }
    stage "Build" { run "dotnet build -c Release --tl" }
    stage "Pack" { run "dotnet pack --no-restore -c Release --tl" }

    stage "Publish" {
        run (fun _ ->
            async {
                let nugetPackages =
                    Directory.EnumerateFiles("artifacts/package/release", "*.nupkg", SearchOption.TopDirectoryOnly)
                    |> Seq.filter (fun nupkg -> not (nupkg.Contains(commitjiCli)))
                    |> Seq.toArray

                let! nugetExitCodes = nugetPackages |> Array.map pushPackage |> Async.Sequential

                return Seq.sum nugetExitCodes
            }
        )
    }

    runIfOnlySpecified true
}

// -- Help ----

tryPrintPipelineCommandHelp ()