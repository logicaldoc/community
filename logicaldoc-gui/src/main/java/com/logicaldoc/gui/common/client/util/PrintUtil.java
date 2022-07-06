package com.logicaldoc.gui.common.client.util;

import com.google.gwt.core.client.GWT;
import com.google.gwt.dom.client.Document;
import com.google.gwt.dom.client.Element;
import com.google.gwt.dom.client.NodeList;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.widgets.Canvas;

/**
 * Some printing utility methods
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.6.1
 */
public class PrintUtil {

	public static void printPreview(String title, String htmlContent) {
		StringBuffer content = new StringBuffer("<html><head>\n");

		// Prepare the header
		content.append("<title>" + (title != null ? I18N.message(title) : I18N.message("print")) + "</title>");
		content.append("\n<style>\n");
		content.append(".cell, .cellDarkAltCol, .cellDark{white-space: nowrap;}\n ");
		content.append(".printHeader{white-space: nowrap; font-weight: bold; border:0px solid white;}\n");
		content.append("\n</style>\n");

		content.append("<link href='" + Util.contextPath() + "fontawesome/css/all.css' rel='stylesheet' />\n");

		content.append("<link REL='STYLESHEET' HREF='" + GWT.getModuleBaseURL() + "sc/skins/" + Util.currentSkin()
				+ "/style.css' TYPE='text/css' />");

		content.append(
				"\n<script type='text/javascript'>function printPage(){document.getElementById('printPanel').style.display='none'; window.print(); window.close();}</script>\n");

		content.append("</head><body>\n");

		// Put the panel with the buttons
		content.append(
				"<div id='printPanel' class='printPanel default'><ul><li><a href='javascript:printPage();' id='printButton'>"
						+ I18N.message("print") + "</a></li><li><a href='javascript:window.close();' id='printClose'>"
						+ I18N.message("close") + "</a></li></ul></div>");

		// Get the HTML
		content.append(htmlContent);

		// Some scripts and styles at the end of the document
		content.append("\n<link rel='stylesheet' type='text/css' href='" + Util.contextPath() + "skin/css?tenant="
				+ Session.get().getTenantName() + "' />\n");

		content.append("\n<script src='" + GWT.getModuleBaseURL() + "/sc/modules/ISC_Foundation.js'></script>\n");
		content.append("\n<script src='frontend/sc/modules/ISC_Containers.js'></script>\n");
		content.append("\n<script src='frontend/sc/modules/ISC_Grids.js'></script>\n");
		content.append("\n<script src='frontend/sc/modules/ISC_Forms.js'></script>\n");
		content.append("\n<script src='frontend/sc/modules/ISC_RichTextEditor.js'></script>\n");
		content.append("\n<script src='frontend/sc/modules/ISC_Calendar.js'></script>\n");
		content.append("\n<script src='frontend/sc/modules/ISC_DataBinding.js'></script>\n");

		content.append("\n</body>\n");

		content.append("\n</html>");

		WindowUtils.openHtmlInWindow(title, content.toString());
	}

	public static void printPreview(String title, Canvas canvas) {
		Element element = null;
		NodeList<Element> divs = Document.get().getElementsByTagName("div");
		for (int i = 0; i < divs.getLength(); i++) {
			Element div = divs.getItem(i);
			if (div.getAttribute("eventproxy").equals(canvas.getID())) {
				element = div;
				break;
			}
		}

		printPreview(title, element.getInnerHTML());
	}

	public static void printScreenShot(String id) {
		printScreenShot(id, I18N.message("print"));
	}

	public static void printScreenShot(String id, String title) {
		printScreenShot(id, title, I18N.message("print"), I18N.message("close"));
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
