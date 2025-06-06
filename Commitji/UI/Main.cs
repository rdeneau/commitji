using Commitji.Model;

namespace Commitji.UI;

public partial class Main : Form
{
    private InputMode _currentMode = InputMode.Prefix;
    private InputMode _startMode = InputMode.Prefix;

    private bool _shouldSelectFirstMatch;

    private string? _selectedPrefix;
    private string? _selectedEmoji;

    // ReSharper disable once InconsistentNaming
    private static readonly string NL = Environment.NewLine;

    public Main()
    {
        InitializeComponent();
        UpdateHint();
        UpdateMode();
        inputTextBox.KeyDown += InputTextBoxOnKeyDown;
        inputTextBox.TextChanged += InputTextBoxOnTextChanged;
    }

    private void InputTextBoxOnKeyDown(object? sender, KeyEventArgs e)
    {
        var hasInput = inputTextBox.Text.Length > 0;
        _shouldSelectFirstMatch = false;

        switch (e.KeyCode)
        {
            case Keys.Escape:
                Close();
                break;

            case Keys.Delete:
                if (hasInput)
                {
                    inputTextBox.Clear();
                }
                else
                {
                    switch (_currentMode)
                    {
                        case InputMode.Prefix:
                            _selectedEmoji = null;
                            break;

                        case InputMode.Emoji:
                            if (_selectedPrefix != null)
                            {
                                _selectedPrefix = null;
                            }
                            else if (_startMode == InputMode.Emoji)
                            {
                                _startMode = InputMode.Prefix;
                            }

                            break;
                    }
                }

                UpdateMode();
                UpdateHint();

                e.Handled = true;
                break;

            case Keys.Enter:
                if (_currentMode == InputMode.Completed)
                {
                    Clipboard.SetText($"{_selectedPrefix}: {_selectedEmoji} ");

                    // TODO RDE: Indicate that the commit message has been copied to the clipboard
                    // TODO RDE: and the elapsed time before closing the window
                    var timer = new System.Windows.Forms.Timer();
                    timer.Interval = 3000; // 3 seconds
                    timer.Tick += (_, _) =>
                    {
                        timer.Stop();
                        Close();
                    };
                    timer.Start();
                    break;
                }

                _shouldSelectFirstMatch = true;
                if (TryAutoCompleteSelection())
                {
                    UpdateMode();
                    UpdateHint();
                }

                e.Handled = true;
                break;

            case Keys.Oem1: // US: ':' or ';' key
            case Keys.OemQuestion: // FR: ':'
                if (_currentMode == InputMode.Prefix && string.IsNullOrEmpty(inputTextBox.Text))
                {
                    _startMode = InputMode.Emoji;
                    UpdateMode();
                    UpdateHint();
                }

                e.Handled = true;
                break;
        }
    }

    private void InputTextBoxOnTextChanged(object? sender, EventArgs e)
    {
        TryAutoCompleteSelection();
        UpdateMode();
        UpdateHint();
    }

    private (string Input, bool HasInput, bool HasEmoji, bool HasPrefix) ComputeState()
    {
        var input = inputTextBox.Text.Trim();
        return (
            Input: input,
            HasInput: !string.IsNullOrEmpty(input),
            HasEmoji: _selectedEmoji != null,
            HasPrefix: _selectedPrefix != null
        );
    }

    private bool TryAutoCompleteSelection()
    {
        var state = ComputeState();
        switch (_currentMode)
        {
            case InputMode.Prefix:
                if (!state.HasInput)
                    return false;

                var matchingPrefixes =
                    Prefix.All
                        .Where(prefix => prefix.Code.StartsWith(state.Input, StringComparison.CurrentCultureIgnoreCase))
                        .ToList();

                switch (matchingPrefixes.Count)
                {
                    case 1:
                    case > 1 when _shouldSelectFirstMatch:
                        _selectedPrefix = matchingPrefixes[0].Code;
                        _shouldSelectFirstMatch = false;
                        inputTextBox.Clear();
                        return true;

                    default:
                        return false;
                }

            case InputMode.Emoji:
                if (!state.HasInput)
                    return false;

                var matchingEmojis =
                    Emoji.All
                        .Where(emoji => emoji.Code.StartsWith(state.Input, StringComparison.CurrentCultureIgnoreCase))
                        .ToList();

                switch (matchingEmojis.Count)
                {
                    case 1:
                    case > 1 when _shouldSelectFirstMatch:
                        _selectedEmoji = matchingEmojis[0].Char;
                        _shouldSelectFirstMatch = false;
                        inputTextBox.Clear();
                        return true;

                    default:
                        return false;
                }

            default:
                return false;
        }
    }

    private void UpdateMode()
    {
        _currentMode =
            (_startMode, _selectedPrefix, _selectedEmoji) switch
            {
                { _selectedEmoji: not null, _selectedPrefix: not null } => InputMode.Completed,
                { _startMode: InputMode.Emoji, _selectedEmoji: not null } => InputMode.Prefix,
                { _startMode: InputMode.Prefix, _selectedPrefix: not null } => InputMode.Emoji,
                _ => _startMode
            };

        selectionLabel.Text = $@"{_selectedPrefix ?? "tbd"}: {_selectedEmoji ?? "❔"}";
    }

    private void UpdateHint()
    {
        // TODO RDE: indicate the current mode in the hint
        var state = ComputeState();
        switch (_currentMode)
        {
            case InputMode.Prefix:
                var prefixes = state.HasInput
                    ? Prefix.All.Where(prefix => prefix.Code.StartsWith(state.Input, StringComparison.CurrentCultureIgnoreCase)).ToList()
                    : Prefix.All;

                var colonKeyHint =
                    state switch
                    {
                        { HasInput: false } => NL + Prefix.Format("[:]", "switch to emoji selection"),
                        _ => string.Empty
                    };

                var deleteKeyHint =
                    state switch
                    {
                        { HasInput: true } => NL + Prefix.Format("[Delete]", $"clear the input: {state.Input}"),
                        { HasEmoji: true } => NL + Prefix.Format("[Delete]", $"delete the selected emoji: {_selectedEmoji}"),
                        _ => string.Empty
                    };

                var enterKeyHint =
                    state switch
                    {
                        { HasInput: true } when prefixes.Count > 0 => NL + Prefix.Format("[Enter]", $"select the prefix: {prefixes[0].Code}"),
                        _ => string.Empty
                    };

                hintTextBox.Text =
                    string.Join(NL, prefixes.Select(x => x.Explain())) +
                    colonKeyHint +
                    deleteKeyHint +
                    enterKeyHint +
                    NL + Prefix.Format("[Escape]", "exit");

                break;

            case InputMode.Emoji:
                var emojis = state.HasInput
                    ? Emoji.All.Where(emoji => emoji.Code.StartsWith(state.Input, StringComparison.CurrentCultureIgnoreCase)).ToList()
                    : Emoji.All;

                deleteKeyHint =
                    state switch
                    {
                        { HasInput: true } => NL + Emoji.Format("[Delete]", $"clear the input: {state.Input}"),
                        { HasPrefix: true } => NL + Emoji.Format("[Delete]", $"delete the selected prefix: {_selectedPrefix}"),
                        _ => string.Empty
                    };

                enterKeyHint =
                    state switch
                    {
                        { HasInput: true } when emojis.Count > 0 => NL + Emoji.Format("[Enter]", $"select the emoji: {emojis[0].Char}"),
                        _ => string.Empty
                    };

                hintTextBox.Text =
                    string.Join(NL, emojis.Select(x => x.Explain())) +
                    deleteKeyHint +
                    enterKeyHint +
                    NL + Emoji.Format("[Escape]", "exit");

                break;

            case InputMode.Completed:
                hintTextBox.Text = "TODO";
                // TODO RDE: unlock breakingChangeCheckbox (if applicable)
                // and update semVerLabel
                break;
        }
    }
}