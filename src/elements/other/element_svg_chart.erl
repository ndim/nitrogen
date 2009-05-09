% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.
%
% SVG charts for Nitrogen
% Copyright (C) 2009 Hans Ulrich Niedermann
% See MIT-LICENSE for licensing information.

-module(element_svg_chart).
-compile(export_all).
-include("wf.inc").
-include("svg_chart.inc").

reflect() ->
    record_info(fields, svg_chart).

render(_ControlID, #svg_chart{title=test}) ->
    Attrs =
	[{xmlns, "http://www.w3.org/2000/svg"},
	 {version, "1.2"},
	 {baseProfile, "tiny"},
	 {viewBox, "0 0 100 100"},
	 {style, "width: 10em; height: 10em;"}
	],
    Content =
	[wf_tags:emit_tag(desc, "Example SVG chart", []),
	 wf_tags:emit_tag(circle, [{cx, 50}, {cy, 50}, {r,40}]),
	 wf_tags:emit_tag(text, "SVG", [{x, 50}, {y, 50}])
	],
    wf_tags:emit_tag(svg, Content, Attrs);
render(ControlID, Record) when is_record(Record, svg_chart) ->
    %% io:format("General SVG Chart render(): ~p~n", [ControlID]),
    Attrs =
	[{xmlns, "http://www.w3.org/2000/svg"},
	 {version, "1.2"},
	 {baseProfile, "tiny"},
	 {viewBox, "0 0 100 200"},
	 {style, "width: 32em; height: 16em;"},
	 {id, ControlID}
	],
    Content =
	[case Record#svg_chart.title of
	     "" -> "";
	     Title -> wf_tags:emit_tag(desc, Title, [])
	 end,
	 wf_tags:emit_tag(circle, [{cx, 50}, {cy, 50}, {r,30}])
	],
    wf_tags:emit_tag(svg, Content, Attrs).
