package com.logicaldoc.gui.frontend.client.webcontent;

import java.util.Arrays;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.frontend.client.document.DocumentsPanel;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.ContentsType;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.widgets.HTMLPane;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * This popup window is used to show the HTML document in a WYSIWYG.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.8.1
 */
public class WebcontentEditor extends Window {

	private VLayout layout = null;

	private GUIDocument document;

	public WebcontentEditor(final GUIDocument document) {
		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		if (document.getId() > 0)
			setTitle(I18N.message("editdoc") + ": " + document.getFileName());
		else
			setTitle(I18N.message("createdoc") + ": " + document.getFileName());

		this.document = document;

		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		setWidth100();
		setHeight100();
		centerInPage();

		addCloseClickHandler(event -> unlockAndClose());

		layout = new VLayout();
		layout.setWidth100();

		prepareBody();

		addItem(layout);
	}

	/**
	 * Prepares the popup body
	 */
	private void prepareBody() {
		HTMLPane editorPanel = new HTMLPane();
		editorPanel.setShowEdges(false);
		editorPanel.setContentsURL(Util.webEditorUrl(document.getId(), document.getFileName(), getHeight() - 230));
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
		close.addClickHandler(event -> unlockAndClose());

		layout.setMembers(toolStrip, editorPanel);
	}

	private void unlockAndClose() {
		if (document.getId() != 0)
			DocumentService.Instance.get().unlock(Arrays.asList(WebcontentEditor.this.document.getId()),
					new AsyncCallback<Void>() {
						@Override
						public void onFailure(Throwable caught) {
							GuiLog.serverError(caught);
							destroy();
						}

						@Override
						public void onSuccess(Void result) {
							DocumentsPanel.get().refresh();
							destroy();
						}
					});
		else
			destroy();
	}
}