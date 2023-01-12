package com.logicaldoc.gui.frontend.client;

import com.google.gwt.core.client.EntryPoint;
import com.google.gwt.core.client.GWT;
import com.google.gwt.core.client.GWT.UncaughtExceptionHandler;
import com.google.gwt.user.client.Window;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.google.gwt.user.client.ui.RootPanel;
import com.logicaldoc.gui.common.client.CookiesManager;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIInfo;
import com.logicaldoc.gui.common.client.beans.GUISession;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.observer.FolderController;
import com.logicaldoc.gui.common.client.services.InfoService;
import com.logicaldoc.gui.common.client.services.SecurityService;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.util.WindowUtils;
import com.logicaldoc.gui.common.client.websockets.WebSocketListener;
import com.logicaldoc.gui.frontend.client.folder.FolderNavigator;
import com.logicaldoc.gui.frontend.client.panels.MainPanel;
import com.logicaldoc.gui.frontend.client.search.TagsForm;
import com.sksamuel.gwt.websockets.Websocket;
import com.smartgwt.client.types.EdgeName;
import com.smartgwt.client.types.MultiMessageMode;
import com.smartgwt.client.types.NotifyTransition;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.notify.Notify;
import com.smartgwt.client.widgets.notify.NotifySettings;

/**
 * The Frontend entry point
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class Frontend implements EntryPoint {

	private Websocket websocket = null;

	/**
	 * Configure some interface defaults
	 */
	static {
		NotifySettings settings = new NotifySettings();
		settings.setMultiMessageMode(MultiMessageMode.REPLACE);
		settings.setAutoFitMaxWidth(400);
		settings.setSlideSpeed(300);
		settings.setDuration(3000);
		settings.setMultiMessageMode(MultiMessageMode.STACK);
		settings.setCanDismiss(true);
		settings.setPosition(EdgeName.T);
		settings.setAppearMethod(NotifyTransition.FADE);
		settings.setDisappearMethod(NotifyTransition.FADE);

		Notify.configureDefaultSettings(settings);
	}

	@Override
	public void onModuleLoad() {
		if (RootPanel.get("loadingwrapper-frontend") == null)
			return;

		GWT.setUncaughtExceptionHandler(new UncaughtExceptionHandler() {
			@Override
			public void onUncaughtException(Throwable caught) {
				// Log unhandled errors only when in devel mode
				if (Session.get().isDevel())
					GuiLog.error(caught.getMessage(), null, caught);
			}

		});

		// Tries to capture locale parameter
		final String locale = Util.detectLocale();
		I18N.setLocale(locale);

		// Tries to capture tenant parameter
		final String tenant = Util.detectTenant();

		// Get grid of scrollbars, and clear out the window's built-in margin,
		// because we want to take advantage of the entire client area.
		Window.enableScrolling(false);
		Window.setMargin("0px");

		declareReloadTrigger(this);
		declareSearchTag(this);
		declareGetCurrentFolderId(this);
		declareCheckPermission(this);
		declareDownload();

		InfoService.Instance.get().getInfo(locale, tenant, false, new AsyncCallback<GUIInfo>() {
			@Override
			public void onFailure(Throwable error) {
				SC.warn(error.getMessage());
			}

			@Override
			public void onSuccess(final GUIInfo info) {
				CookiesManager.saveRelease(info);

				init(info);

				SecurityService.Instance.get().getSession(Util.getLocaleInRequest(), null, new AsyncCallback<GUISession>() {

					@Override
					public void onFailure(Throwable caught) {
						SC.warn(I18N.message("accessdenied") + " - " + caught.getMessage());
					}

					@Override
					public void onSuccess(GUISession session) {
						if (session == null || !session.isLoggedIn()) {
							SC.warn(I18N.message("accessdenied"));
						} else {
							session.getInfo().setUserNo(info.getUserNo());
							init(session.getInfo());
							Session.get().init(session);
							showMain();
							connectWebsockets();
							declareReloadTrigger(Frontend.this);
						}
					}
				});
			}
		});
	}

	public static void showMain() {
		// Remove the loading frame
		RootPanel.getBodyElement().removeChild(RootPanel.get("loadingwrapper-frontend").getElement());
		MainPanel.get().show();
	}

	/**
	 * Triggers the load of the last uploaded files
	 */
	public void triggerReload() {
		FolderNavigator.get().reload();
	}

	public String getCurrentFolderId() {
		return Long.toString(FolderController.get().getCurrentFolder().getId());
	}

	public String checkPermission(String permission) {
		return Boolean.toString(FolderController.get().getCurrentFolder().hasPermission(permission));
	}

	public void searchTag(String tag) {
		TagsForm.searchTag(tag, false);
	}

	public void addTagInCloud(String tag, String weight, String link) {
		TagsForm.searchTag(tag, false);
	}

	private void init(final GUIInfo info) {
		Feature.init(info);
		I18N.init(info);

		WindowUtils.setTitle(info, null);
		WindowUtils.setFavicon(info);

		Session.get().setInfo(info);
		Util.setupDensity(info);
	}

	/**
	 * Install the receiver to get messages from the server (Server Push)
	 */
	public void connectWebsockets() {
		if (Session.get().isServerPushEnabled()) {
			websocket = new Websocket(Util.websocketUrl());
			websocket.addListener(new WebSocketListener());
			websocket.open();
		}
	}

	/**
	 * Declares the javascript function used to check a permission in the
	 * current folder
	 * 
	 * @param frontend the Frontend module
	 */
	public static native void declareCheckPermission(Frontend frontend) /*-{
		$wnd.checkPermission = function(permission) {
			return frontend.@com.logicaldoc.gui.frontend.client.Frontend::checkPermission(Ljava/lang/String;)(permission);
		};
	}-*/;

	/**
	 * Declares the javascript function used to retrieve the current folder ID
	 * 
	 * @param frontend the Frontend module
	 */
	public static native void declareGetCurrentFolderId(Frontend frontend) /*-{
		$wnd.getCurrentFolderId = function() {
			return frontend.@com.logicaldoc.gui.frontend.client.Frontend::getCurrentFolderId()();
		};
	}-*/;

	/**
	 * Declares the javascript function used to trigger the reload of the
	 * current folder
	 * 
	 * @param frontend the Frontend module
	 */
	public static native void declareReloadTrigger(Frontend frontend) /*-{
		$wnd.triggerReload = function() {
			frontend.@com.logicaldoc.gui.frontend.client.Frontend::triggerReload()();
		};
	}-*/;

	/**
	 * Declares the javascript function used to trigger the search for a
	 * specific tag
	 * 
	 * @param frontend the Frontend module
	 */
	public static native void declareSearchTag(Frontend frontend) /*-{
		$wnd.searchTag = function(tag) {
			frontend.@com.logicaldoc.gui.frontend.client.Frontend::searchTag(Ljava/lang/String;)(tag);
		};
	}-*/;

	/**
	 * Declares the javascript function used to download something
	 */
	public static native void declareDownload() /*-{
		$wnd.download = function(url) {
			@com.logicaldoc.gui.common.client.util.Util::download(Ljava/lang/String;)(url);
		};
	}-*/;
}