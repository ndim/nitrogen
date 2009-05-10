%%% -*- erlang -*-
%%% SVG charts
%%% modelled after Nitrogen's google_chart.inc and element_google_chart.erl

-include("wf.inc").

-record(svg_chart,
	{?ELEMENT_BASE(element_svg_chart),
	 type=line,
	 title="",
	 description="",
	 axes=[],
	 data=[]
	}).

-record(chart_axis,
	{position,
	 labels
	}).

-record(chart_data,
	{legend,
	 color,
	 values
	}).
