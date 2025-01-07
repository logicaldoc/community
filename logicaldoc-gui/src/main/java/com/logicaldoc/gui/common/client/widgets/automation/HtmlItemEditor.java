package com.logicaldoc.gui.common.client.widgets.automation;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.Util;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.ContentsType;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.widgets.HTMLPane;
import com.smartgwt.client.widgets.HeaderControl;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * This popup window is used to show the HTML content of an Automation item
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.3.4
 */
public class HtmlItemEditor extends Window {

	private VLayout layout = null;

	private FormItem item;

	private ChangedHandler handler;

	public HtmlItemEditor(FormItem item, ChangedHandler handler) {
		this.item = item;
		this.handler = handler;

		HeaderControl closeIcon = new HeaderControl(HeaderControl.CLOSE, event -> onClose());

		setHeaderControls(HeaderControls.HEADER_LABEL, closeIcon);
		setTitle(item.getTitle());

		setWidth(com.google.gwt.user.client.Window.getClientWidth());
		setHeight(com.google.gwt.user.client.Window.getClientHeight());
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();

		addCloseClickHandler(event -> onClose());

		layout = new VLayout();
		layout.setWidth100();

		prepareBody();

		addItem(layout);
	}

	/**
	 * Prepares the popup body
	 */
	private void prepareBody() {
		setSavedHtmlContent(item.getValue() != null ? item.getValue().toString() : "");

		HTMLPane editorPanel = new HTMLPane();
		editorPanel.setID("htmlEditorPanel");
		editorPanel.setShowEdges(false);
		editorPanel.setContentsURL(Util.webEditorUrl(getHeight() - 230));
		editorPanel.setContentsType(ContentsType.PAGE);

		layout.addMember(editorPanel);

		ToolStrip toolStrip = new ToolStrip();
		toolStrip.setWidth100();
		toolStrip.setAlign(Alignment.RIGHT);
		toolStrip.addFill();

		ToolStripButton close = new ToolStripButton();
		close.setTitle(I18N.message("close"));
		toolStrip.addButton(close);
		close.addClickHandler(event -> onClose());

		layout.setMembers(toolStrip, editorPanel);
	}

	private void onClose() {
		String editedHtml = getSavedHtmlContent() != null ? getSavedHtmlContent() : "";
		String originalHtml = item.getValue() != null ? item.getValue().toString() : "";
		if (!editedHtml.equals(originalHtml)) {
			item.setValue(editedHtml);
			if (handler != null)
				handler.onChanged(null);
		}
		setSavedHtmlContent("");
		destroy();
	}
	
	@Override
	public boolean equals(Object other) {
		return super.equals(other);
	}

	@Override
	public int hashCode() {
		return super.hashCode();
	}
	
	/**
	 * Saves the actual item content in a global Javascript variable that can be
	 * accessed by the HTML editor
	 * 
	 * @param value the inputed value to save
	 */
	public static native void setSavedHtmlContent(String value) /*-{
		$wnd.savedHtmlContent = value;
	}-*/;

	public static native String getSavedHtmlContent() /*-{
		return $wnd.savedHtmlContent;
	}-*/;
}