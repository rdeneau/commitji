namespace Commitji.Model;

public record BreakingChange(Emoji? Emoji = null, Prefix? Prefix = null, bool Checked = false, bool Disabled = false)
{
    public BreakingChange(Prefix Prefix, bool Checked = false, bool Disabled = false) : this(null, Prefix, Checked, Disabled)
    {
    }

    public static readonly IReadOnlyList<BreakingChange> All = new List<BreakingChange>
    {
        new(Prefix: null, Emoji: null, Checked: false, Disabled: true),
        new(Emoji.Boom, Checked: true, Disabled: true),
        new(Prefix.Feat, Disabled: false),
        new(Prefix.Fix, Disabled: false),
    };
}