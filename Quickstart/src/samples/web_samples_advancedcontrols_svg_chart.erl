-module (web_samples_advancedcontrols_svg_chart).
-include ("wf.inc").
-include ("svg_chart.inc").
-compile(export_all).

main() ->
    %% The application/xhtml+xml content type is absolutely necessary for
    %% the page to be interpreted as XML which is absolutely required for
    %% the inline SVG elements to be parsed properly. Verified using
    %% Firefox 3, WebKit-gtk-1.0.0-0.15.svn37790.fc10.
    %% The <meta http-equiv="Content-Type"> value is irrelevant.
    wf:set_content_type("application/xhtml+xml; charset=utf-8"),
    #template { file="./wwwroot/onecolumn.html", bindings=
		[
		 {'Group', learn},
		 {'Item', samples}
		]}.

title() -> "SVG Charts Example".
headline() -> "SVG Charts Example".
right() -> linecount:render().

body() ->
    [
     #h2 { text="Line Chart" },
     #svg_chart
     {title="Line Chart",
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
	values=[10, 20, 30, 20, 30, 40, 40, 50, 60]
       },
       #chart_data
       {legend="Data 2",
	color="#2768A9",
	values=[20, 50, 70, 90, 70, 40, 10,  1,  8]
       }
      ]
     },
     #h2 { text="Test Chart" },
     #svg_chart{title=test}
    ].

event(_) -> ok.
