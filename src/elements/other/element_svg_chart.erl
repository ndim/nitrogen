% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.
%
% SVG charts for Nitrogen
% Copyright (C) 2009 Hans Ulrich Niedermann
% See MIT-LICENSE for licensing information.

%%
%% Compatibility notes:
%%  * FF3 needs fill-opacity="0.7", not fill-opacity="70%"
%%    WebKit understands both
%%  * WebKit draws svg:svg elements slightly moved to the side
%%    FF3 draws them bang-on
%%  * WebKit draws text in black
%%    FF3 draws text according to fill and stroke
%%  * FF3, WebKit will not interpret the SVG elements if the
%%    HTTP Content-Type header is "text/html".
%%    "application/xhtml+xml" is proven to work, "text/xml" probably
%%    also does. (Presumably this is about tag soup vs. XML parser).
%%

-module(element_svg_chart).
-compile(export_all).
-include("wf.inc").
-include("svg_chart.inc").


-define(XMLNS_SVG, "http://www.w3.org/2000/svg").
-define(XMLNS_XLINK, "http://www.w3.org/1999/xlink").


list_count(List) ->
    {IdxList, Count} =
	lists:foldl(fun(Elt, {AccList,Index}) ->
			    {[{Index,Elt}|AccList], Index+1}
		    end,
		    {[], 0},
		    List),
    {lists:reverse(IdxList), Count}.


reflect() ->
    record_info(fields, svg_chart).


render_axis(#chart_axis{position=Pos, labels=Labels}) ->
    Content =
	[
	 case Pos of
	     bottom ->
		 wf_tags:emit_tag('svg:path',
				  [{d, "M 1,95 l 198,0"},
				   {stroke, "#008800"},
				   {'stroke-opacity', 1},
				   {'stroke-width', 1}
				  ]);
	     left ->
		 wf_tags:emit_tag('svg:path',
				  [{d, "M 5,1 l 0,98"},
				   {stroke, "#880000"},
				   {'stroke-opacity', 1},
				   {'stroke-width', 1}
				  ])
	 end
	],
    wf_tags:emit_tag('svg:g', Content, [{class, "svg-chart-axis"},
					{transform, "none"}]).


render_plot(line, #chart_data{color=Color, values=Values, line_width=RawLW}) ->
    {IndexValues, ValueCount} = list_count(Values),
    LW = case RawLW of
	     undefined -> 2;
	     RawLW when is_integer(RawLW) -> RawLW
	 end,
    io:format("render_plot(line,...) ~p ~p ~p~n~p~n~p~n",
	      [Color, ValueCount, LW,
	       Values, IndexValues]),
    HScale = 200 / ValueCount,
    [{FirstIdx, FirstValue}|Tail] = IndexValues,
    D = [io_lib:format("M ~w,~w", [(0.5+FirstIdx)*HScale, FirstValue]),
	 [ io_lib:format("L ~w,~w", [(0.5+Idx)*HScale, Val])
	   || {Idx,Val} <- Tail]],
    Content =
	[
	 wf_tags:emit_tag('svg:path',
			  [{d, D},
			   {stroke, Color},
			   {'stroke-opacity', 1},
			   {'stroke-width', LW},
			   {'fill', "none"}
			  ])
	],
    wf_tags:emit_tag('svg:g', Content, [{class, "svg-chart-plot"},
					{transform, %"none"
					 "scale(1, -1) translate(0, -95)"
					}]).


render_legend(DataCount, NumberedData) ->
    LegendFontSize = 7,
    wf_tags:emit_tag
      ('svg:g',
       [
	wf_tags:emit_tag
	('svg:rect',
	 [{x,0}, {y,0},
	  {height, LegendFontSize*DataCount+2},
	  {width, 40},
	  {'fill', "#ffffff"},
	  {'fill-opacity', "1.0"},
	  {'stroke', "#000000"},
	  {'stroke-width', 1}
	 ]),
	[ [wf_tags:emit_tag
	   ('svg:text',
	    Data#chart_data.legend,
	    [{x,10+1},
	     {y,LegendFontSize*(1+Index)},
	     {'font-family', "sans"},
	     {'font-size', LegendFontSize},
	     {'font-weight', "normal"},
	     {'fill', "#000000"},
	     {'fill-opacity', "1.0"},
	     {'stroke', "#000000"},
	     {'stroke-width', 0},
	     {'text-anchor', "left"}
	    ]),
	   wf_tags:emit_tag
	   ('svg:rect',
	    [{x,2},
	     {y,LegendFontSize*(Index) +LegendFontSize div 2 -1},
	     {height, 4},
	     {width, 10-1-2},
	     {'fill', Data#chart_data.color},
	     {'fill-opacity', "1.0"}
	    ])
	  ]
	  || {Index, Data} <- NumberedData ]
       ], [{class, "svg-chart-legend"},
	   {transform, "translate(155,5)"}]).


render_background() ->
    CircID = wf:temp_id(),
    CircRef = lists:flatten(io_lib:format("#~s", [CircID])),
    wf_tags:emit_tag
      ('svg:g',
       [
	wf_tags:emit_tag('svg:circle',
			 [{cx, 50}, {cy, 50}, {r,30},
			  {'fill', "#bb8888"},
			  {'fill-opacity', "0.7"},
			  {'stroke', "#ff0000"},
			  {'stroke-width', 5},
			  {id, CircID}
			 ]),
	wf_tags:emit_tag('svg:use',
			 [{x,150}, {y,50},
			  {'xlink:href', CircRef},
			  {transform, "translate(-50,-50)"}]),
	wf_tags:emit_tag('svg:use',
			 [{x,100}, {y,50},
			  {'xlink:href', CircRef},
			  {transform, "translate(-50,-50)"}]),
	wf_tags:emit_tag('svg:rect',
			 [{x,0}, {y,0},
			  {height, 100},
			  {width, 200},
			  {fill, "#ffffff"},
			  {'fill-opacity', "0.9"}
			 ])
       ],
       [{class, 'svg-chart-background'},
	{transform, "none"}]).


render_test_chart() ->
    Attrs =
	[{'xmlns:svg', ?XMLNS_SVG},
	 {version, "1.2"},
	 {baseProfile, "tiny"},
	 {viewBox, "0 0 100 100"},
	 {id, "svg-chart__test-chart"},
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
                                         {'fill-opacity', "0.9"},
                                         {'stroke', "#8888ff"},
                                         {'stroke-width', 6}
                                        ]),
	 wf_tags:emit_tag('svg:text', "SVG", [{x, 50}, {y, 65},
                                              {'font-family', "serif"},
                                              {'font-weight', "bold"},
                                              {'font-size', 42},
                                              {'fill', "#ffcc00"},
                                              {'fill-opacity', "0.5"},
                                              {'stroke', "#ff0000"},
                                              {'stroke-width', 3},
                                              {'text-anchor', "middle"}])
        ],
    wf_tags:emit_tag('svg:svg', Content, Attrs).


render(_ControlID, #svg_chart{title=test}) ->
    render_test_chart();

render(ControlID, #svg_chart{type=line, width=W, height=H} = Record)
  when is_record(Record, svg_chart) ->
    io:format("===========================================================~n",
	      []),
    Attrs =
	[{'xmlns:svg', ?XMLNS_SVG},
	 {'xmlns:xlink', ?XMLNS_XLINK},
	 {version, "1.2"},
	 {baseProfile, "tiny"},
	 {viewBox, "0 0 200 100"},
	 {style, [ case {W,H} of
		       {none,none} ->
			   "width: 32em; height: 32ex; ";
		       {Wi,Hi} when is_integer(Wi), is_integer(Hi) ->
			   io_lib:format(
			     "width: ~wpx; height: ~wpx; ", [W,H])
		   end,
		   "border: solid 1.5pt #0000dd; "
		   "background-color: #ccccff; "
		   "margin: 0; "
		   "padding: 0.5ex; "
		   ]
         },
	 {id, ControlID}
	],
    Axes = [ render_axis(A) || A <- Record#svg_chart.axes],
    Plots = [ render_plot(line, D) || D <- Record#svg_chart.data],
    {NumberedData, DataCount} = list_count(Record#svg_chart.data),
    Content =
	[case Record#svg_chart.title of
	     "" -> "";
	     Title -> wf_tags:emit_tag('svg:title', Title, [])
	 end,
	 case Record#svg_chart.description of
	     "" -> "";
	     Desc -> wf_tags:emit_tag('svg:desc', Desc, [])
	 end,
	 render_background(),
	 wf_tags:emit_tag('svg:g',
			  [
			   wf_tags:emit_tag('svg:g', Axes,
					    [{class, "svg-chart-axes"},
					     {transform, "none"}]),
			   wf_tags:emit_tag('svg:g', Plots,
					    [{class, "svg-chart-plots"},
					     {transform, "none"}]),
			   render_legend(DataCount, NumberedData)
			  ], [{class, "svg-chart-content"},
			      {transform, "none"}])
	],
    wf_tags:emit_tag('svg:svg', Content, Attrs).
