package com.logicaldoc.gui.common.client.widgets;

import com.google.gwt.dom.client.Element;
import com.google.gwt.user.client.DOM;
import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.controllers.FolderController;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.frontend.client.folder.FolderNavigator;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.widgets.HTMLFlow;
import com.smartgwt.client.widgets.Window;

/**
 * Displays the DropSpot
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.2.3
 */
public class DropSpotPopup extends Window {

	public DropSpotPopup() {
		setTitle(I18N.message("dropspot"));
		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setWidth(com.google.gwt.user.client.Window.getClientWidth());
		setHeight(com.google.gwt.user.client.Window.getClientHeight());
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();
		setWidth100();

		HTMLFlow panel = new HTMLFlow();
		panel.setWidth100();
		panel.setHeight100();
		String url = Util.contextPath() + "dropspot/index.jsp?&sid=" + Session.get().getSid() + "&locale="
				+ I18N.getLocale() + "&docLocale=" + I18N.getDefaultLocaleForDoc() + "&folderId="
				+ FolderController.get().getCurrentFolder().getId();
		panel.setContents("<iframe id='dropboxframe' src='" + url + "' style='border:0px solid white; width:"
				+ (getWidth() - 1) + "px; height:" + (getHeight() - 1)
				+ "px; overflow:hidden;' scrolling='auto' seamless='seamless'></iframe>");
		addMember(panel);

		addResizedHandler(event -> {
			Element element = DOM.getElementById("dropboxframe");
			element.getStyle().setProperty("height", (getHeight() - 1) + "px");
			element.getStyle().setProperty("width", (getWidth() - 1) + "px");
		});

		addCloseClickHandler(event -> FolderNavigator.get().reload());

		declareClose(this);
	}

	public static void openDropSpot() {
		// Just to clean the upload folder
		DocumentService.Instance.get().cleanUploadedFileFolder(new DefaultAsyncCallback<>() {

			@Override
			public void onSuccess(Void result) {
				DropSpotPopup dropSpot = new DropSpotPopup();
				dropSpot.show();
			}
		});
	}

	/**
	 * Triggers the load of the last uploaded files
	 */
	@Override
	public void close() {
		try {
			FolderNavigator.get().reload();
		} catch (Exception e) {
			// Do nothing
		}
		destroy();
	}

	/**
	 * Declares the javascript function used to trigger the close of the current
	 * popup
	 * 
	 * @param popup the popup instance
	 */
	public static native void declareClose(DropSpotPopup popup) /*-{
		$wnd.closeDropSpotPopup = function() {
			popup.@com.logicaldoc.gui.common.client.widgets.DropSpotPopup::close()();
		};
	}-*/;
}