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

-define(XMLNS_SVG, "http://www.w3.org/2000/svg").

reflect() ->
    record_info(fields, svg_chart).

render(_ControlID, #svg_chart{title=test}) ->
    Attrs =
	[{'xmlns:svg', ?XMLNS_SVG},
	 {version, "1.2"},
	 {baseProfile, "tiny"},
	 {viewBox, "0 0 100 100"},
	 {style, "width: 16em; "
                 "height: 16em; "
                 "border: solid 1.5pt black; "
                 "padding: 0.5ex; "
                 "margin: 0; "
                 "background-color: #ffdddd; "
         }
	],
    Content =
	[wf_tags:emit_tag('svg:desc', "Example SVG chart", []),
	 wf_tags:emit_tag('svg:circle', [{cx, 50}, {cy, 50}, {r,40},
                                         {'fill', "#88aa88"},
                                         {'fill-opacity', "90%"},
                                         {'stroke', "#8888ff"},
                                         {'stroke-width', 6}
                                        ]),
	 wf_tags:emit_tag('svg:text', "SVG", [{x, 50}, {y, 65},
                                              {'font-family', "serif"},
                                              {'font-weight', "bold"},
                                              {'font-size', 42},
                                              {'fill', "#ffcc00"},
                                              {'fill-opacity', "50%"},
                                              {'stroke', "#ff0000"},
                                              {'stroke-width', 3},
                                              {'text-anchor', "middle"}])
        ],
    wf_tags:emit_tag('svg:svg', Content, Attrs);

render(ControlID, Record) when is_record(Record, svg_chart) ->
    %% io:format("General SVG Chart render(): ~p~n", [ControlID]),
    Attrs =
	[{'xmlns:svg', ?XMLNS_SVG},
	 {version, "1.2"},
	 {baseProfile, "tiny"},
	 {viewBox, "0 0 200 100"},
	 {style, "width: 32em; height: 16em; "
                 "border: solid 1.5pt #0000dd; "
                 "background-color: #ccccff; "
                 "margin: 0; "
                 "padding: 0.5ex; "
         },
	 {id, ControlID}
	],
    Content =
	[case Record#svg_chart.title of
	     "" -> "";
	     Title -> wf_tags:emit_tag('svg:title', Title, [])
	 end,
	 case Record#svg_chart.description of
	     "" -> "";
	     Desc -> wf_tags:emit_tag('svg:desc', Desc, [])
	 end,
	 wf_tags:emit_tag('svg:circle', [{cx, 50}, {cy, 50}, {r,30},
                                         {'fill', "#bb8888"},
                                         {'fill-opacity', "90%"},
                                         {'stroke', "#ff0000"},
                                         {'stroke-width', 5}
                                        ])
	],
    wf_tags:emit_tag('svg:svg', Content, Attrs).
