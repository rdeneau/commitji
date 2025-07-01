module Commitji.Tests.Cli.MarkupShould

open Commitji.Cli
open Commitji.Core.Model
open Commitji.Core.Model.Search
open Swensen.Unquote
open global.Xunit

module ``highlight segment`` =
    let highlightSegment segment =
        Markup.highlightSegmentWith (fun s -> $"[{s}]") segment

    [<Theory>]
    [<InlineData("feat", "f", 0, "[f]eat")>]
    [<InlineData("feat", "a", 2, "fe[a]t")>]
    [<InlineData("feat", "t", 3, "fea[t]")>]
    [<InlineData("feat", "fe", 0, "[fe]at")>]
    [<InlineData("feat", "ea", 1, "f[ea]t")>]
    [<InlineData("feat", "at", 2, "fe[at]")>]
    let ``apply highlight on single hit`` text input hit expected =
        let segment =
            SearchSegment.Searched(SegmentId.Code, SearchedSegmentText { Text = text; Hits = [ hit ] }, String.length input)

        let actual = highlightSegment segment
        actual =! expected

    [<Theory>]
    [<InlineData("test", "t", 0, 3, "[t]es[t]")>]
    [<InlineData("nette", "t", 2, 3, "ne[t][t]e")>]
    let ``apply highlight on double hit`` text input hit1 hit2 expected =
        let segment =
            SearchSegment.Searched(SegmentId.Code, SearchedSegmentText { Text = text; Hits = [ hit1; hit2 ] }, String.length input)

        let actual = highlightSegment segment
        actual =! expected

    [<Theory>]
    [<InlineData("abc", "z")>]
    [<InlineData("abc", "ba")>]
    let ``apply no highlight if not found`` text input =
        let segment =
            SearchSegment.Searched(SegmentId.Code, SearchedSegmentText { Text = text; Hits = [] }, String.length input)

        let actual = highlightSegment segment
        actual =! text

    [<Theory>]
    [<InlineData("abc")>]
    [<InlineData("z")>]
    let ``apply no highlight if not searchable`` text =
        let segment = SearchSegment.NotSearchable(SegmentId.Code, SegmentText text)
        let actual = highlightSegment segment
        actual =! text