package com.logicaldoc.web.stat;

import java.awt.Color;
import java.awt.Paint;

import org.jfree.chart.plot.DefaultDrawingSupplier;

/**
 * This picks the body color for any section of a pie chart
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.1.3
 *
 */
public class ChartDrawingSupplier extends DefaultDrawingSupplier {

	private static final long serialVersionUID = 1L;

	public transient Paint[] paintSequence;

	public int paintIndex;

	public int fillPaintIndex;

	public ChartDrawingSupplier() {
		paintSequence = new Paint[] { new Color(128, 255, 128), new Color(128, 128, 255), new Color(255, 128, 128),
				new Color(242, 179, 235), new Color(13, 0, 255), new Color(242, 255, 0), new Color(157, 157, 157),
				new Color(156, 49, 25), new Color(174, 255, 0), new Color(255, 238, 8) };
	}

	@Override
	public Paint getNextPaint() {
		Paint result = paintSequence[paintIndex % paintSequence.length];
		paintIndex++;
		return result;
	}

	@Override
	public Paint getNextFillPaint() {
		Paint result = paintSequence[fillPaintIndex % paintSequence.length];
		fillPaintIndex++;
		return result;
	}
}