package com.logicaldoc.gui.login.client;

import com.google.gwt.core.client.EntryPoint;
import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.Window;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.google.gwt.user.client.ui.RootPanel;
import com.logicaldoc.gui.common.client.CookiesManager;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIInfo;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.services.InfoService;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.util.WindowUtils;
import com.smartgwt.client.util.SC;

/**
 * The Login entry point
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.5
 */
public class Login implements EntryPoint {

	private LoginPanel loginPanel;

	@Override
	public void onModuleLoad() {
		if (RootPanel.get("loadingwrapper-login") == null)
			return;

		GWT.setUncaughtExceptionHandler(caught -> SC.warn("Error", caught.getMessage()));

		declareShowLostDialog(this);

		// Tries to capture locale parameter
		final String lang = Util.detectLocale();
		I18N.setLocale(lang);

		// Tries to capture tenant parameter
		final String tenant = Util.detectTenant();

		// Get grid of scrollbars, and clear out the window's built-in margin,
		// because we want to take advantage of the entire client area.
		Window.enableScrolling(false);
		Window.setMargin("0px");

		InfoService.Instance.get().getInfo(I18N.getLocale(), tenant, true, new AsyncCallback<GUIInfo>() {
			@Override
			public void onFailure(Throwable error) {
				SC.warn(error.getMessage());
			}

			@Override
			public void onSuccess(final GUIInfo info) {
				CookiesManager.saveRelease(info);

				I18N.init(info);

				WindowUtils.setTitle(info, null);

				Feature.init(info);
				Session.get().setInfo(info);

				WindowUtils.setFavicon(info);

				if ("mobile".equals(Util.getJavascriptVariable("j_layout")))
					loginPanel = new MobileLoginPanel(info);
				else
					loginPanel = new LoginPanel(info);

				Login.this.showLogin();
			}

		});
	}

	// Setup the initial visualization of the login panel
	private void showLogin() {
		// Remove the loading frame
		RootPanel.getBodyElement().removeChild(RootPanel.get("loadingwrapper-login").getElement());

		loginPanel.initGUI();
		loginPanel.show();
	}

	public void showLostDialog(String productName) {
		LoginPanel.showLostDialog(productName);
	}

	/**
	 * Declares the javascript function used to display the lost password popup
	 * 
	 * @param login the login module
	 */
	public static native void declareShowLostDialog(Login login) /*-{
		$wnd.showLostDialog = function(productName) {
			return login.@com.logicaldoc.gui.login.client.Login::showLostDialog(Ljava/lang/String;)(productName);
		};
	}-*/;
}