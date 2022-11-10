package com.logicaldoc.gui.common.client.util;

import com.google.gwt.user.client.DOM;
import com.google.gwt.user.client.Element;
import com.google.gwt.user.client.Window.Navigator;
import com.logicaldoc.gui.common.client.beans.GUIInfo;
import com.logicaldoc.gui.common.client.i18n.I18N;

/**
 * Utilities for accessing the javascript Window object
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class WindowUtils {

	public static RequestInfo getRequestInfo() {
		RequestInfo result = new RequestInfo();
		result.setHash(getHash());
		result.setHost(getHost());
		result.setHostName(getHostName());
		result.setHref(getHref());
		result.setPath(getPath());
		result.setPort(getPort());
		result.setProtocol(getProtocol());
		result.setQueryString(getQueryString());
		return result;
	}

	public static native String getAppName() /*-{
		return $wnd.navigator.appName;
	}-*/;

	private static native String getQueryString() /*-{
		return $wnd.location.search;
	}-*/;

	private static native String getProtocol() /*-{
		return $wnd.location.protocol;
	}-*/;

	private static native String getPort() /*-{
		return $wnd.location.port;
	}-*/;

	private static native String getPath() /*-{
		return $wnd.location.pathname;
	}-*/;

	private static native String getHref() /*-{
		return $wnd.location.href;
	}-*/;

	private static native String getHostName() /*-{
		return $wnd.location.hostname;
	}-*/;

	private static native String getHost() /*-{
		return $wnd.location.host;
	}-*/;

	private static native String getHash() /*-{
		return $wnd.location.hash;
	}-*/;

	public static native void reload() /*-{
		$wnd.location.reload(true);
	}-*/;

	public static native void setTitle(String title)/*-{
		$doc.title = title;
		//Change also the main frame window if any
		if ($wnd.parent)
			$wnd.parent.document.title = title;
	}-*/;

	public static void setTitle(GUIInfo info, String prefix) {
		String buf = info.getBranding().getProductName() + " " + info.getRelease()
				+ (info.getLicensee() != null ? " - " + I18N.message("licensedto") + " " + info.getLicensee() : "");
		if (prefix != null) {
			buf = prefix + " - " + buf;
		}
		WindowUtils.setTitle(buf);
	}

	public static void setFavicon(GUIInfo info) {
		try {
			Element link = DOM.getElementById("favicon");
			Element parent = DOM.getParent(link);
			DOM.removeChild(parent, link);

			link = DOM.createElement("link");
			link.setId("favicon");
			link.setAttribute("rel", "shortcut icon");
			link.setAttribute("type", "image/png");
			link.setAttribute("href", info.getBranding().getFaviconSrc());

			DOM.appendChild(parent, link);
		} catch (Throwable t) {
			// Nothing to do
		}
	}

	public static native void openUrl(String url)/*-{
		$wnd.location = url;
	}-*/;

	public static native void openUrlInNewTab(String url)/*-{
	    $wnd.open(url, '_blank', null);
    }-*/;

	public static native void openUrl(String url, String window)/*-{
		$wnd.open(url, window, null);
	}-*/;

	public static native void openUrl(String url, String window, String specs)/*-{
		$wnd.open(url, window, specs);
	}-*/;

	public static native void focus()/*-{
		$wnd.focus();
	}-*/;

	public static native String left()/*-{
		return $wnd.screenX;
	}-*/;

	public static native String top()/*-{
		return $wnd.screenY;
	}-*/;

	public static native String getUserAgent() /*-{
		return navigator.userAgent.toLowerCase();
	}-*/;

	/**
	 * Opens a given HTML document in new popup windows
	 * 
	 * @param title window titlew
	 * @param html the HTML code to display
	 */
	public static native void openHtmlInWindow(String title, String html) /*-{
		var printWindow = $wnd
				.open(
						"",
						title,
						"toolbar=no,location=no,directories=no,status=no,menubar=no,scrollbars=yes,resizable=yes");
		// printWindow.document.body.innerHTML = body;        
		printWindow.document.write(html);
		printWindow.focus();
	}-*/;

	public static boolean isChrome() {
		return getUserAgent().toLowerCase().contains("chrome");
	}

	public static boolean isWindows() {
		String platform = Navigator.getPlatform();
		return platform.trim().toLowerCase().contains("win");
	}
}