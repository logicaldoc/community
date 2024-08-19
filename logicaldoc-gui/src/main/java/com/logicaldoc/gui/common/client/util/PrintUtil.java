package com.logicaldoc.gui.common.client.util;

import com.logicaldoc.gui.common.client.i18n.I18N;

/**
 * Some printing utility methods
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.6.1
 */
public class PrintUtil {

	private static final String PRINT = "print";

	private PrintUtil() {
	}

	public static void printScreenShot(String id) {
		printScreenShot(id, I18N.message(PRINT));
	}

	public static void printScreenShot(String id, String title) {
		printScreenShot(id, title, I18N.message(PRINT), I18N.message("close"));
	}

	/**
	 * Uses Javascript to take a screenshot of a canvas and prints it
	 * 
	 * @param id identifier of the canvas
	 * @param title title of the window
	 * @param printLabel title of the print button
	 * @param closeLabel title of the close button
	 */
	public static native void printScreenShot(String id, String title, String printLabel, String closeLabel) /*-{
		$wnd.screenshot(id, title, printLabel, closeLabel);
	}-*/;
}
