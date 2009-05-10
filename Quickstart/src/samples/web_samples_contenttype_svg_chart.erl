-module (web_samples_contenttype_svg_chart).
-include ("wf.inc").
-include ("svg_chart.inc").
-compile(export_all).

main() ->
        %% FIXME: set HTTP header in some way:
        %%        Content-Disposition: inline; filename=something.svg
        wf:set_content_type("image/svg+xml"),
        ["<?xml version=\"1.0\" encoding=\"utf-8\"?>\n",
	 line_chart()].

event(_) -> ok.

line_chart() ->
    #svg_chart
	    {title="Line Chart",
	     width=600, height=300,
	     description="Adapted from Google Charts Example",
	     id="svg-chart__line-chart",
	     axes=
	     [#chart_axis
	      {position=bottom,
	       labels=["one", "two", "three", "four"]
	      },
	      #chart_axis
	      {position=left,
	       labels=["a", "b", "c", "d"]
	      }
	     ],
	     data=
	     [#chart_data
	      {legend="Data 1",
	       color="#ff9900",
	       line_width=3,
	       values=[10, 20, 30, 20, 30, 40, 40, 50, 60]
	      },
	      #chart_data
	      {legend="Data 2",
	       color="#2768A9",
	       values=[20, 50, 70, 90, 70, 40, 10,  1,  8]
	      }
	     ]
	    }.
