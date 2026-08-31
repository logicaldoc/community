package com.logicaldoc.gui.common.client.widgets;

import com.google.gwt.user.client.Timer;
import com.smartgwt.client.widgets.Window;

/**
 * A window with utility method to force a delayed redraw of the content
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.1.1
 */
public abstract class DelayedRedrawWindow extends Window {

	/**
	 * Forces the redraw after waiting some milliseconds
	 */
	protected void delayedRedraw() {
		new Timer() {
			public void run() {
				redraw();
			}
		}.schedule(200);
	}
}