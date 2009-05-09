-module (web_samples_advancedcontrols_svg_chart).
-include ("wf.inc").
-include ("svg_chart.inc").
-compile(export_all).

main() -> #template { file="./wwwroot/onecolumn.html", bindings=[
	{'Group', learn},
	{'Item', samples}
]}.

title() -> "SVG Charts Example".
headline() -> "SVG Charts Example".
right() -> linecount:render().

body() ->
    %% The application/xhtml+xml content type is absolutely necessary for
    %% the page to be interpreted as XML which is absolutely required for
    %% the inline SVG elements to be parsed properly. Verified using
    %% Firefox 3, WebKit-gtk-1.0.0-0.15.svn37790.fc10.
    %% The <meta http-equiv="Content-Type"> value is irrelevant.
    wf_platform:set_content_type("application/xhtml+xml; charset=utf-8"),
    [
     #h2 { text="Test Chart" },
     #svg_chart{title=test},
     #h2 { text="Proper Chart" },
     #svg_chart{title="Titled Chart"}
    ].

event(_) -> ok.
