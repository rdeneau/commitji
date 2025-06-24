#r "nuget: Fun.Build, 1.0.3"
#r "nuget: CliWrap, 3.6.4"
#r "nuget: FSharp.Data, 6.3.0"
#r "nuget: Ionide.KeepAChangelog, 0.1.8"
#r "nuget: Humanizer.Core, 2.14.1"

open System
open System.IO
open Fun.Build
open CliWrap
open CliWrap.Buffered
open FSharp.Data
open System.Xml.Linq
open System.Xml.XPath
open Ionide.KeepAChangelog
open Ionide.KeepAChangelog.Domain
open SemVersion
open Humanizer

let (</>) a b = Path.Combine(a, b)

let cleanFolders (input: string seq) =
    async {
        input
        |> Seq.iter (fun dir ->
            if Directory.Exists(dir) then
                Directory.Delete(dir, true)
        )
    }

let pushPackage nupkg =
    async {
        let key = Environment.GetEnvironmentVariable("NUGET_KEY")

        let! result =
            Cli.Wrap("dotnet").WithArguments($"nuget push {nupkg} --api-key {key} --source https://api.nuget.org/v3/index.json").ExecuteAsync().Task
            |> Async.AwaitTask

        return result.ExitCode
    }

pipeline "Build" {
    workingDir __SOURCE_DIRECTORY__
    stage "RestoreTools" { run "dotnet tool restore" }
    stage "Clean" { run (cleanFolders [| "artifacts" |]) }
    stage "CheckFormat" { run "dotnet fantomas build.fsx src tests --check" }
    stage "Build" { run "dotnet build -c Release --tl" }
    stage "UnitTests" { run "dotnet test -c Release --tl" }
    stage "Pack" { run "dotnet pack --no-restore -c Release --tl" }
    runIfOnlySpecified false
}

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

                    return! runCmd "dotnet" $"fantomas {files}"
            }
        )
    }

    runIfOnlySpecified true
}

pipeline "PushClient" {
    workingDir __SOURCE_DIRECTORY__
    stage "Pack" { run "dotnet pack ./src/Commitji.Cli -c Release --tl" }

    stage "Push" {
        run (fun _ ->
            async {
                return!
                    Directory.EnumerateFiles("artifacts/package/release", "Commitji.Cli.*.nupkg", SearchOption.TopDirectoryOnly)
                    |> Seq.tryExactlyOne
                    |> Option.map pushPackage
                    |> Option.defaultValue (
                        async {
                            printfn "Commitji.Cli package was not found."
                            return -1
                        }
                    )
            }
        )
    }

    runIfOnlySpecified true
}

pipeline "FormatAll" {
    workingDir __SOURCE_DIRECTORY__
    stage "Fantomas" { run "dotnet fantomas build.fsx src tests" }
    runIfOnlySpecified true
}

let deps = __SOURCE_DIRECTORY__ </> ".deps"

let updateFileRaw (file: FileInfo) =
    let lines = File.ReadAllLines file.FullName

    let updatedLines =
        lines
        |> Array.map (fun line ->
            if line.Contains("FSharp.Compiler") then
                line.Replace("FSharp.Compiler", "Fantomas.FCS")
            elif line.Contains("[<TailCall>]") then
                line.Replace("[<TailCall>]", "[<Microsoft.FSharp.Core.TailCall>]")
            else
                line
        )

    File.WriteAllLines(file.FullName, updatedLines)

let downloadCompilerFile commitHash relativePath =
    async {
        let file = FileInfo(deps </> commitHash </> relativePath)

        if file.Exists && file.Length <> 0 then
            return ()
        else
            file.Directory.Create()
            let fs = file.Create()
            let fileName = Path.GetFileName(relativePath)

            let url =
                $"https://raw.githubusercontent.com/dotnet/fsharp/{commitHash}/{relativePath}"

            let! response = Http.AsyncRequestStream(url, headers = [| "Content-Disposition", $"attachment; filename=\"{fileName}\"" |])

            if response.StatusCode <> 200 then
                printfn $"Could not download %s{relativePath}"

            do! Async.AwaitTask(response.ResponseStream.CopyToAsync(fs))
            fs.Close()

            updateFileRaw file
    }

type GithubRelease = {
    Version: string
    Title: string
    Date: DateTime
    PublishedDate: string option
    Draft: string
}

let mkGithubRelease (v: SemanticVersion, d: DateTime, cd: ChangelogData option) =
    match cd with
    | None -> failwith "Each release is expected to have at least one section."
    | Some cd ->
        let version = $"{v.Major}.{v.Minor}.{v.Patch}"

        let title =
            let month = d.ToString("MMMM")
            let day = d.Day.Ordinalize()
            $"{month} {day} Release"

        let prefixedVersion = $"v{version}"

        let publishDate =
            let cmdResult =
                Cli.Wrap("gh").WithArguments($"release view {prefixedVersion} --json publishedAt -t \"{{{{.publishedAt}}}}\"").WithValidation(CommandResultValidation.None).ExecuteBufferedAsync().Task.Result

            if cmdResult.ExitCode <> 0 then
                None
            else
                let output = cmdResult.StandardOutput.Trim()
                let lastIdx = output.LastIndexOf("Z", StringComparison.Ordinal)
                Some(output.Substring(0, lastIdx))

        let sections =
            [
                "Added", cd.Added
                "Changed", cd.Changed
                "Fixed", cd.Fixed
                "Deprecated", cd.Deprecated
                "Removed", cd.Removed
                "Security", cd.Security
                yield! (Map.toList cd.Custom)
            ]
            |> List.choose (fun (header, lines) ->
                if lines.IsEmpty then
                    None
                else
                    lines |> List.map (fun line -> line.TrimStart()) |> String.concat "\n" |> sprintf "### %s\n%s" header |> Some
            )
            |> String.concat "\n\n"

        let draft =
            $"""# {version}

{sections}"""

        {
            Version = version
            Title = title
            Date = d
            PublishedDate = publishDate
            Draft = draft
        }

let getReleaseNotes currentRelease (lastRelease: GithubRelease) =
    $"""{currentRelease.Draft}

[https://www.nuget.org/packages/commitji/{currentRelease.Version}](https://www.nuget.org/packages/fantomas/{currentRelease.Version})
    """

let getCurrentAndLastReleaseFromChangelog () =
    let changelog = FileInfo(__SOURCE_DIRECTORY__ </> "CHANGELOG.md")

    let changeLogResult =
        match Parser.parseChangeLog changelog with
        | Error error -> failwithf "%A" error
        | Ok result -> result

    let lastReleases =
        changeLogResult.Releases
        |> List.filter (fun (v, _, _) -> String.IsNullOrEmpty v.Prerelease)
        |> List.sortByDescending (fun (_, d, _) -> d)
        |> List.take 2

    match lastReleases with
    | [ current; last ] -> mkGithubRelease current, mkGithubRelease last
    | _ -> failwith "Could not find the current and last release from CHANGELOG.md"

pipeline "Release" {
    workingDir __SOURCE_DIRECTORY__

    stage "Release" {
        run (fun _ ->
            async {
                let currentRelease, lastRelease = getCurrentAndLastReleaseFromChangelog ()

                if Option.isSome currentRelease.PublishedDate then
                    return 0
                else
                    // Push packages to NuGet
                    let nugetPackages =
                        Directory.EnumerateFiles("artifacts/package/release", "*.nupkg", SearchOption.TopDirectoryOnly)
                        |> Seq.filter (fun nupkg -> not (nupkg.Contains("Commitji.Cli")))
                        |> Seq.toArray

                    let! nugetExitCodes = nugetPackages |> Array.map pushPackage |> Async.Sequential

                    let notes = getReleaseNotes currentRelease lastRelease
                    let noteFile = Path.GetTempFileName()
                    File.WriteAllText(noteFile, notes)
                    let files = nugetPackages |> String.concat " "

                    // We create a draft release for minor and majors. Those that requires a manual publish.
                    // This is to allow us to add additional release notes when it makes sense.
                    let! draftResult =
                        let isDraft =
                            let isRevision = lastRelease.Version.Split('.') |> Array.last |> int |> (<>) 0
                            if isRevision then String.Empty else "--draft"

                        Cli
                            .Wrap("gh")
                            .WithArguments($"release create v{currentRelease.Version} {files} {isDraft} --title \"{currentRelease.Title}\" --notes-file \"{noteFile}\"")
                            .WithValidation(CommandResultValidation.None)
                            .ExecuteAsync()
                            .Task
                        |> Async.AwaitTask

                    if File.Exists noteFile then
                        File.Delete(noteFile)

                    return Seq.max [| yield! nugetExitCodes; yield draftResult.ExitCode |]
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
        run (fun ctx ->
            async {
                let nugetPackages =
                    Directory.EnumerateFiles("artifacts/package/release", "*.nupkg", SearchOption.TopDirectoryOnly)
                    |> Seq.filter (fun nupkg -> not (nupkg.Contains("Commitji.Cli")))
                    |> Seq.toArray

                let! nugetExitCodes = nugetPackages |> Array.map pushPackage |> Async.Sequential

                return Seq.sum nugetExitCodes
            }
        )
    }

    runIfOnlySpecified true
}

tryPrintPipelineCommandHelp ()