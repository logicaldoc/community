package com.logicaldoc.gui.common.client.log;

import com.google.gwt.core.client.GWT;
import com.logicaldoc.gui.common.client.InvalidSessionException;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.widgets.ContactingServer;
import com.smartgwt.client.util.SC;

/**
 * Represents a client work session
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class Log {

	private Log() {
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
		try {
			ContactingServer.get().hide();

			// Hide download exceptions that normally are raised on double
			// click.
			if ("0".equals(message.trim()))
				return;

			EventPanel.get().error(I18N.message("servererror") + ": " + message, message);
			GWT.log("Server error: " + message, caught);

			if (caught instanceof InvalidSessionException) {
				// Redirect to the module's login page
				Session.get().close();
				String base = GWT.getHostPageBaseURL();
				Util.redirect(base
						+ (base.endsWith("/") ? GWT.getModuleName() + ".jsp" : "/" + GWT.getModuleName() + ".jsp"));
			} else if (caught instanceof ServerException) {
				SC.warn(caught.getMessage());
			}
		} catch (Throwable t) {
		}
	}

	public static void warn(String message, String detail) {
		try {
			GWT.log("warn: " + message, null);
			EventPanel.get().warn(message, detail);
		} catch (Throwable t) {
		}
	}

	public static void error(String message, String detail, Throwable caught) {
		try {
			// Hide download exceptions that normally are raised on double
			// click.
			if ("0".equals(message))
				return;

			GWT.log("error: " + message, caught);
			EventPanel.get().error(message, detail);
		} catch (Throwable t) {
		}
	}

	public static void info(String message) {
		info(message, null);
	}
	
	public static void info(String message, String detail) {
		try {
			GWT.log("info: " + message, null);
			EventPanel.get().info(message, detail);
		} catch (Throwable t) {
		}
	}

	public static void debug(String message) {
		try {
			GWT.log("debug: " + message, null);
		} catch (Throwable t) {
		}
	}
}