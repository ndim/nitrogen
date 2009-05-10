<?xml version="1.0" encoding="utf-8"?>
<!--+
    | extract-svg.xsl - extract all svg:svg elements from an XML document
    | Copyright (C) 2009 Hans Ulrich Niedermann
    | See MIT-LICENSE for licensing information.
    |
    | This is useful as a debugging aid for element_svg_chart.erl.
    | Example usage:
    |    $ elinks -source \
    |    +  "http://localhost:8000/web/samples/advancedcontrols_svg_chart" \
    |    +  xsltproc extract-svg.xsl -
    |    $ elinks -source \
    |    +  "http://localhost:8000/web/samples/advancedcontrols_svg_chart" \
    |    +  xsltproc -\/-stringparam svg-id page__svg-chart__line-chart \
    |    +  extract-svg.xsl -
    | (Replace -\/- with two consecutive - characters.)
    +-->
<xsl:transform
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:svg="http://www.w3.org/2000/svg"
  xmlns="http://xml.n-dimensional.de/ns/2009/stuff"
  version="1.0">

  <xsl:output method="xml" version="1.0" encoding="utf-8"
	      indent="yes" media-type="image/svg+xml"/>

  <xsl:param name="svg-id" />

  <xsl:template match="/">
      <xsl:choose>
	<xsl:when test="$svg-id">
	  <xsl:apply-templates select="descendant::svg:svg[@id = $svg-id]"/>
	</xsl:when>
	<xsl:otherwise>
	  <svg-collection>
	    <xsl:apply-templates select="descendant::svg:svg"/>
	  </svg-collection>
	</xsl:otherwise>
      </xsl:choose>
  </xsl:template>

  <xsl:template match="@*|node()">
    <xsl:copy>
      <xsl:apply-templates select="@*|node()"/>
    </xsl:copy>
  </xsl:template>

</xsl:transform>
