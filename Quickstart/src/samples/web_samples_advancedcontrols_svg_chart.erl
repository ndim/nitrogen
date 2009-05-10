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
    ChartDivStyle =
	["border: solid 1.5pt #0000dd;",
	 "background-color: #ccccff;",
	 "margin: 0;",
	 "padding: 0.5ex;",
	 "width: 600px;",
	 "height: 300px;"
	],
    ChartDivClass = "svg-chart",
    [
     #h2 { text="Line Chart (inline document)" }, "\n",
     wf_tags:emit_tag('div',
		      [
		       web_samples_contenttype_svg_chart:line_chart()
		      ],
		      [{style, ChartDivStyle},
		       {class, ChartDivClass}]),
     "\n",
     #h2 { text="Line Chart (external object)" }, "\n",
     wf_tags:emit_tag('div',
		      [
		       wf_tags:emit_tag('object',
					[{data, "contenttype_svg_chart"},
					 {type, "image/svg+xml"},
					 {style, ["width:600px;",
						  "height:300px;"]}
					])
		       ],
		      [{style, ChartDivStyle},
		       {class, ChartDivClass}]),
     "\n",
     #h2 { text="Test Chart" }, "\n",
     #svg_chart{title=test, id="svg-chart__test-chart"}, "\n"
    ].

event(_) -> ok.
