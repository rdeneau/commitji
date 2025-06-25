/**
 * @type {import('semantic-release').GlobalConfig}
 */
export default {
    branches: ["main"],
    plugins: [
        "@semantic-release/commit-analyzer",
        "@semantic-release/release-notes-generator",
        [
            "@semantic-release/exec",
            {
                "prepareCmd": "dotnet pack .\\src\\Commitji.Cli\\Commitji.Cli.fsproj --configuration Release -p:PackageVersion=${nextRelease.version} -p:PackageReleaseNotes=\"${nextRelease.notes}\" --output ./nuget",
                "publishCmd": "dotnet nuget push ./nuget/*.nupkg --api-key $NUGET_KEY --source https://api.nuget.org/v3/index.json --skip-duplicate"
            }
        ],
        "@semantic-release/github"
    ]
};