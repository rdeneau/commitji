namespace Commitji.Model;

public record SemVerChange(string Name, string Format, bool BreakingChange, Prefix? Prefix)
{
    public static readonly IReadOnlyList<SemVerChange> All = new List<SemVerChange>
    {
        new("Major", "+1._._", true, null),
        new("Minor", "_.+1._", false, Prefix.Feat),
        new("Patch", "_._.+1", false, Prefix.Fix),
        new("None", "", false, null)
    };
}