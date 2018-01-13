package com.logicaldoc.gui.frontend.client.metadata.form;

import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.Util;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.ContentsType;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.widgets.HTMLPane;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.events.CloseClickEvent;
import com.smartgwt.client.widgets.events.CloseClickHandler;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * This popup window is used to show the HTML content of a form
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 7.3
 */
public class FormEditor extends Window {

	private HTMLPane editorPanel = new HTMLPane();

	private VLayout layout = null;

	private GUIDocument form;

	public FormEditor(GUIDocument form) {
		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("form") + " " + form.getFileName());

		this.form = form;

		setWidth(com.google.gwt.user.client.Window.getClientWidth());
		setHeight(com.google.gwt.user.client.Window.getClientHeight());
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();

		addCloseClickHandler(new CloseClickHandler() {
			@Override
			public void onCloseClick(CloseClickEvent event) {
				destroy();
			}
		});

		layout = new VLayout();
		layout.setWidth100();

		prepareBody();

		addItem(layout);
	}

	/**
	 * Prepares the popup body
	 */
	private void prepareBody() {
		editorPanel = new HTMLPane();
		editorPanel.setShowEdges(false);
		editorPanel.setContentsURL(Util.webEditorUrl(form.getId(), form.getFileName(), getHeight() - 230));
		editorPanel.setContentsType(ContentsType.PAGE);

		layout.addMember(editorPanel);

		ToolStrip toolStrip = new ToolStrip();
		toolStrip.setWidth100();
		toolStrip.setAlign(Alignment.RIGHT);
		toolStrip.addFill();

		ToolStripButton close = new ToolStripButton();
		close.setTitle(I18N.message("close"));
		toolStrip.addButton(close);
		toolStrip.addSeparator();
		close.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				destroy();
			}
		});

		layout.setMembers(toolStrip, editorPanel);
	}
}