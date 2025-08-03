package com.logicaldoc.gui.common.client.log;

import com.google.gwt.core.client.GWT;
import com.google.gwt.http.client.RequestTimeoutException;
import com.logicaldoc.gui.common.client.InvalidSessionServerException;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.widgets.SessionTimeout;
import com.smartgwt.client.util.SC;

/**
 * Represents a client work session
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class GuiLog {

	private GuiLog() {
	}

	public static void serverError(Throwable caught) {
		serverError(caught.getMessage(), caught);
	}

	/**
	 * Logs a server error and shows a warning to the user
	 * 
	 * @param message The message to be shown (optional)
	 * @param caught The caught exception (if any)
	 */
	public static void serverError(String message, Throwable caught) {
		LD.clearPrompt();

		if (message == null || message.isEmpty())
			message = "error";

		// Hide download exceptions that normally are raised on double
		// click.
		if ("0".equals(message.trim()))
			return;
		GWT.log("Server error: " + message, caught);

		if (caught instanceof RequestTimeoutException) {
			SC.warn(I18N.message("timeout"));
		} else if (caught instanceof InvalidSessionServerException) {
			SessionTimeout.get().show();
		} else {
			GuiLog.error(caught.getMessage(), null, caught);
		}
	}

	public static void warn(String message, String detail) {
		GWT.log("warn: " + message, null);
		EventPanel.get().warn(message, detail);
	}

	public static void error(String message, String detail, Throwable caught) {
		// Hide download exceptions that normally are raised on double
		// click.
		if ("0".equals(message))
			return;

		GWT.log("error: " + message, caught);
		EventPanel.get().error(message, detail);
	}

	public static void error(String message) {
		error(message, null, null);
	}

	public static void info(String message) {
		info(message, null);
	}

	public static void info(String message, String detail) {
		GWT.log("info: " + message, null);
		EventPanel.get().info(message, detail);
	}

	public static void debug(String message) {
		GWT.log("debug: " + message, null);
	}
}