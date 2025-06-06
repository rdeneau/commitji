namespace Commitji.Model;

/// <summary>
/// Git commit prefix, based on a variation of Angular conventional commits.
/// https://github.com/angular/angular/blob/main/contributing-docs/commit-message-guidelines.md
/// </summary>
public record Prefix(string Code, string Description)
{
    public static readonly Prefix Feat = new("feat", "A new feature");
    public static readonly Prefix Fix = new("fix", "A bug fix");
    public static readonly Prefix Docs = new("docs", "Documentation only changes");
    public static readonly Prefix Refactor = new("refactor", "A code change that does not alter the functionality");
    public static readonly Prefix Perf = new("perf", "A code change that improves performance");
    public static readonly Prefix Test = new("test", "Adding missing tests");
    public static readonly Prefix Chore = new("chore", "Any other changes finalized: config, build, ci, dependencies...");
    public static readonly Prefix Wip = new("wip", "Work in progress, not yet finalized");

    public static readonly IReadOnlyList<Prefix> All = new List<Prefix>
    {
        Feat,
        Fix,
        Docs,
        Refactor,
        Perf,
        Test,
        Chore,
        Wip,
    };

    private static readonly int CodeMaxWidth = All.Max(prefix => prefix.Code.Length);

    public static string Format(string item, string description)=> $"{item.PadRight(CodeMaxWidth)} → {description}";

    public string Explain() => Format(Code, Description);
}