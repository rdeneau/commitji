namespace Commitji.Model;

/// <summary>
/// Gitmoji emoji, from https://gitmoji.dev
/// </summary>
public record Emoji(string Char, string Code, string Description)
{
    public static readonly Emoji AdhesiveBandage = new("🩹", "adhesive_bandage", "Simple fix for a non-critical issue.");
    public static readonly Emoji Airplane = new("✈️", "airplane", "Improve offline support.");
    public static readonly Emoji Alembic = new("⚗️", "alembic", "Perform experiments.");
    public static readonly Emoji Alien = new("👽️", "alien", "Update code due to external API changes.");
    public static readonly Emoji Ambulance = new("🚑", "ambulance", "Critical hotfix.");
    public static readonly Emoji ArrowDown = new("⬇️", "arrow_down", "Downgrade dependencies.");
    public static readonly Emoji ArrowUp = new("⬆️", "arrow_up", "Upgrade dependencies.");
    public static readonly Emoji Art = new("🎨", "art", "Improve structure / format of the code.");
    public static readonly Emoji Beers = new("🍻", "beers", "Write code drunkenly.");
    public static readonly Emoji Bento = new("🍱", "bento", "Add or update assets.");
    public static readonly Emoji Bookmark = new("🔖", "bookmark", "Release / Version tags.");
    public static readonly Emoji Boom = new("💥", "boom", "Introduce breaking changes.");
    public static readonly Emoji Bricks = new("🧱", "bricks", "Infrastructure related changes.");
    public static readonly Emoji Bug = new("🐛", "bug", "Fix a bug.");
    public static readonly Emoji BuildingConstruction = new("🏗️", "building_construction", "Make architectural changes.");
    public static readonly Emoji Bulb = new("💡", "bulb", "Add or update comments in source code.");
    public static readonly Emoji BustsInSilhouette = new("👥", "busts_in_silhouette", "Add or update contributor(s).");
    public static readonly Emoji CameraFlash = new("📸", "camera_flash", "Add or update snapshots.");
    public static readonly Emoji CardFileBox = new("🗃️", "card_file_box", "Perform database related changes.");
    public static readonly Emoji ChartWithUpwardsTrend = new("📈", "chart_with_upwards_trend", "Add or update analytics or track code.");
    public static readonly Emoji ChildrenCrossing = new("🚸", "children_crossing", "Improve user experience / usability.");
    public static readonly Emoji ClosedLockWithKey = new("🔐", "closed_lock_with_key", "Add or update secrets.");
    public static readonly Emoji ClownFace = new("🤡", "clown_face", "Mock things.");
    public static readonly Emoji Coffin = new("⚰️", "coffin", "Remove dead code.");
    public static readonly Emoji Construction = new("🚧", "construction", "Work in progress, not yet finalized.");
    public static readonly Emoji ConstructionWorker = new("👷", "construction_worker", "Add or update CI build system.");
    public static readonly Emoji Dizzy = new("💫", "dizzy", "Add or update animations and transitions.");
    public static readonly Emoji Egg = new("🥚", "egg", "Add or update an easter egg.");
    public static readonly Emoji Fire = new("🔥", "fire", "Remove code or files.");
    public static readonly Emoji GlobeWithMeridians = new("🌐", "globe_with_meridians", "Internationalization and localization.");
    public static readonly Emoji GoalNet = new("🥅", "goal_net", "Catch errors.");
    public static readonly Emoji GreenHeart = new("💚", "green_heart", "Fix CI Build.");
    public static readonly Emoji Hammer = new("🔨", "hammer", "Add or update development scripts.");
    public static readonly Emoji HeavyMinusSign = new("➖", "heavy_minus_sign", "Remove a dependency.");
    public static readonly Emoji HeavyPlusSign = new("➕", "heavy_plus_sign", "Add a dependency.");
    public static readonly Emoji Iphone = new("📱", "iphone", "Work on responsive design.");
    public static readonly Emoji Label = new("🏷️", "label", "Add or update types.");
    public static readonly Emoji Lipstick = new("💄", "lipstick", "Add or update the UI and style files.");
    public static readonly Emoji Lock = new("🔒️", "lock", "Fix security or privacy issues.");
    public static readonly Emoji LoudSound = new("🔊", "loud_sound", "Add or update logs.");
    public static readonly Emoji Mag = new("🔍️", "mag", "Improve SEO.");
    public static readonly Emoji Memo = new("📝", "memo", "Add or update documentation.");
    public static readonly Emoji MoneyWithWings = new("💸", "money_with_wings", "Add sponsorships or money related infrastructure.");
    public static readonly Emoji MonocleFace = new("🧐", "monocle_face", "Data exploration/inspection.");
    public static readonly Emoji Mute = new("🔇", "mute", "Remove logs.");
    public static readonly Emoji Necktie = new("👔", "necktie", "Add or update business logic.");
    public static readonly Emoji Package = new("📦", "package", "Add or update compiled files or packages.");
    public static readonly Emoji PageFacingUp = new("📄", "page_facing_up", "Add or update license.");
    public static readonly Emoji PassportControl = new("🛂", "passport_control", "Work on code related to authorization, roles and permissions.");
    public static readonly Emoji Pencil2 = new("✏️", "pencil2", "Fix typos.");
    public static readonly Emoji Poop = new("💩", "poop", "Write bad code that needs to be improved.");
    public static readonly Emoji Pushpin = new("📌", "pushpin", "Pin dependencies to specific versions.");
    public static readonly Emoji Recycle = new("♻️", "recycle", "Refactor code: without changing its behavior.");
    public static readonly Emoji Rewind = new("⏪", "rewind", "Revert changes.");
    public static readonly Emoji Rocket = new("🚀", "rocket", "Deploy stuff.");
    public static readonly Emoji RotatingLight = new("🚨", "rotating_light", "Fix compiler / linter warnings.");
    public static readonly Emoji SafetyVest = new("🦺", "safety_vest", "Add or update code related to validation.");
    public static readonly Emoji Seedling = new("🌱", "seedling", "Add or update seed files.");
    public static readonly Emoji SeeNoEvil = new("🙈", "see_no_evil", "Add or update a .gitignore file.");
    public static readonly Emoji Sparkles = new("✨", "sparkles", "Introduces a new feature.");
    public static readonly Emoji SpeechBalloon = new("💬", "speech_balloon", "Add or update text and literals.");
    public static readonly Emoji Stethoscope = new("🩺", "stethoscope", "Add or update healthcheck.");
    public static readonly Emoji Tada = new("🎉", "tada", "Begin a project.");
    public static readonly Emoji Technologist = new("🧑‍💻", "technologist", "Improve developer experience.");
    public static readonly Emoji TestTube = new("🧪", "test_tube", "Add a failing test.");
    public static readonly Emoji Thread = new("🧵", "thread", "Add or update code related to multithreading or concurrency.");
    public static readonly Emoji TriangularFlagOnPost = new("🚩", "triangular_flag_on_post", "Add, update, or remove feature flags.");
    public static readonly Emoji Truck = new("🚚", "truck", "Move or rename resources (e.g.: files, paths, routes).");
    public static readonly Emoji TwistedRightwardsArrows = new("🔀", "twisted_rightwards_arrows", "Merge branches.");
    public static readonly Emoji Wastebasket = new("🗑️", "wastebasket", "Deprecate code that needs to be cleaned up.");
    public static readonly Emoji Wheelchair = new("♿️", "wheelchair", "Improve accessibility.");
    public static readonly Emoji WhiteCheckMark = new("✅", "white_check_mark", "Add, update, or pass tests.");
    public static readonly Emoji Wrench = new("🔧", "wrench", "Add or update configuration files.");
    public static readonly Emoji Zap = new("⚡️", "zap", "Improve performance.");

    public static readonly IReadOnlyList<Emoji> All = new List<Emoji>
    {
        AdhesiveBandage,
        Airplane,
        Alembic,
        Alien,
        Ambulance,
        ArrowDown,
        ArrowUp,
        Art,
        Beers,
        Bento,
        Bookmark,
        Boom,
        Bricks,
        Bug,
        BuildingConstruction,
        Bulb,
        BustsInSilhouette,
        CameraFlash,
        CardFileBox,
        ChartWithUpwardsTrend,
        ChildrenCrossing,
        ClosedLockWithKey,
        ClownFace,
        Coffin,
        Construction,
        ConstructionWorker,
        Dizzy,
        Egg,
        Fire,
        GlobeWithMeridians,
        GoalNet,
        GreenHeart,
        Hammer,
        HeavyMinusSign,
        HeavyPlusSign,
        Iphone,
        Label,
        Lipstick,
        Lock,
        LoudSound,
        Mag,
        Memo,
        MoneyWithWings,
        MonocleFace,
        Mute,
        Necktie,
        Package,
        PageFacingUp,
        PassportControl,
        Pencil2,
        Poop,
        Pushpin,
        Recycle,
        Rewind,
        Rocket,
        RotatingLight,
        SafetyVest,
        Seedling,
        SeeNoEvil,
        Sparkles,
        SpeechBalloon,
        Stethoscope,
        Tada,
        Technologist,
        TestTube,
        Thread,
        TriangularFlagOnPost,
        Truck,
        TwistedRightwardsArrows,
        Wastebasket,
        Wheelchair,
        WhiteCheckMark,
        Wrench,
        Zap
    };

    private static readonly int CodeMaxWidth = All.Max(emoji => emoji.Code.Length);

    private static string Format(string character, string item, string description) =>
        $"{character} {item.PadRight(CodeMaxWidth)} → {description}";

    public static string Format(string item, string description) =>
        Format(" ", item, description);

    public string Explain() =>
        Format(Char, $":{Code}:", Description);
}