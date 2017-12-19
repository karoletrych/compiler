// --------------------------------------------------------------------------------------
// FAKE build script
// --------------------------------------------------------------------------------------

#r "./packages/FAKE/tools/FakeLib.dll"

open Fake
open System



// --------------------------------------------------------------------------------------
// Build variables
// --------------------------------------------------------------------------------------

open System.IO

let appReferences = !! "/**/*.fsproj"
let srcAppReferences = !! "/src/**/*.fsproj"
let releaseDir  = "./release/"
let version = "0.2"
let buildDir = "./src/Compiler/bin/Release/"

let buildDirs = 
    appReferences
    |> Seq.map Path.GetDirectoryName
    |> Seq.collect (fun p -> [Path.Combine(p,"obj"); Path.Combine(p, "bin")])
let dotnetcliVersion = "2.0.3"
let mutable dotnetExePath = "dotnet"

// --------------------------------------------------------------------------------------
// Helpers
// --------------------------------------------------------------------------------------



let runDotnet workingDir args =
    let result =
        ExecProcess (fun info ->
            info.FileName <- dotnetExePath
            info.WorkingDirectory <- workingDir
            info.Arguments <- args) TimeSpan.MaxValue
    if result <> 0 then failwithf "dotnet %s failed" args

// --------------------------------------------------------------------------------------
// Targets
// --------------------------------------------------------------------------------------


Target "Clean" (fun _ ->
    CleanDirs buildDirs 
    CleanDir releaseDir
)

Target "InstallDotNetCLI" (fun _ ->
    dotnetExePath <- DotNetCli.InstallDotNetSDK dotnetcliVersion
)

Target "Restore" (fun _ ->
    appReferences
    |> Seq.iter (fun p ->
        let dir = System.IO.Path.GetDirectoryName p
        runDotnet dir "restore"
    )
)

Target "Build" (fun _ ->
    appReferences
    |> Seq.iter (fun p ->
        let dir = System.IO.Path.GetDirectoryName p
        runDotnet dir "build"
    )
)

Target "RestoreSrc" (fun _ ->
    srcAppReferences
    |> Seq.iter (fun p ->
        let dir = System.IO.Path.GetDirectoryName p
        runDotnet dir "restore"
    )
)

Target "BuildRelease" (fun _ ->
    srcAppReferences
    |> Seq.iter (fun p ->
        let dir = System.IO.Path.GetDirectoryName p
        runDotnet dir "build -c Release"
    )
)

Target "BuildDebug" (fun _ ->
    srcAppReferences
    |> Seq.iter (fun p ->
        let dir = System.IO.Path.GetDirectoryName p
        runDotnet dir "build"
    )
)

Target "IntegrationTest" (fun () ->
    let args = getBuildParam "args"
    ExecProcessAndReturnMessages  (fun info ->
        info.FileName <- "test/Compiler.IntegrationTests/bin/Debug/net461/Compiler.IntegrationTests.exe"
        info.WorkingDirectory <- "test/Compiler.IntegrationTests/bin/Debug/net461/"
        info.Arguments <- args) TimeSpan.MaxValue
    |> printfn "%A"
     
)

Target "Zip" (fun _ ->
    let zipPath = releaseDir + "Compiler." + version + ".zip" 

    !! (buildDir + "/**/*.*") 
    |> Zip buildDir zipPath
)


// --------------------------------------------------------------------------------------
// Build order
// --------------------------------------------------------------------------------------

"Clean"
  ==> "InstallDotNetCLI"
  ==> "Restore"
  ==> "Build"

"RestoreSrc"
  ==> "BuildRelease"

"RestoreSrc"
 ==> "BuildDebug"

"BuildDebug"
 ==> "IntegrationTest"


"BuildRelease"
  ==> "Zip"

RunTargetOrDefault "Build"
