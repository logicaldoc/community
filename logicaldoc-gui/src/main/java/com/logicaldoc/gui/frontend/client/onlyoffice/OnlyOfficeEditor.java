package com.logicaldoc.gui.frontend.client.onlyoffice;

import java.util.Arrays;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.controllers.DocumentController;
import com.logicaldoc.gui.common.client.controllers.FolderController;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.DocUtil;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.frontend.client.document.DocumentsPanel;
import com.logicaldoc.gui.frontend.client.google.GoogleService;
import com.logicaldoc.gui.frontend.client.google.GoogleUtil;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.ContentsType;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.widgets.HTMLFlow;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * This popup window is used to open the document in OnlyOffice editor.
 * 
 * @author Alessandro Gasparini - LogicalDOC
 * @since 8.9.4
 */
public class OnlyOfficeEditor extends Window {

	private HTMLFlow html = new HTMLFlow();

	private VLayout layout = null;

	private GUIDocument document;

	public OnlyOfficeEditor(final GUIDocument document) {
		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		if (document.getId() > 0)
			setTitle(I18N.message("editdoc") + ": " + document.getFileName());
		else
			setTitle(I18N.message("createdoc") + ": " + document.getFileName());

		this.document = document;

		setWidth(com.google.gwt.user.client.Window.getClientWidth());
		setHeight(com.google.gwt.user.client.Window.getClientHeight());
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();
		setMargin(2);

		addCloseClickHandler(event -> {
			if (document.getId() != 0)
				destroy();
		});

		addResizedHandler(event -> reloadBody());

		layout = new VLayout();
		layout.setMargin(1);
		layout.setWidth100();
		layout.setHeight100();
		addItem(layout);

		reloadBody();
	}

	/**
	 * Reloads a preview.
	 */
	private void reloadBody() {
	
		String url = getOnlyOfficeEditorUrl(document.getId(), document.getFileName());

		String iframe = "<iframe src='" + url + "' style='border: 0px solid white; width:" + (getWidth() - 18)
				+ "px; height:" + (getHeight() - 68) + "px' scrolling='no'></iframe>";

		//html.setContents(iframe);
		
		html = new HTMLFlow();
		html.setWidth100();
		html.setHeight100();
		html.setShowEdges(false);
		html.setContents(iframe);

		/*
		ToolStrip toolStrip = new ToolStrip();
		toolStrip.setHeight(20);
		toolStrip.setWidth100();
		toolStrip.setAlign(Alignment.RIGHT);
		toolStrip.addFill();

		ToolStripButton close = new ToolStripButton();
		close.setTitle(I18N.message("close"));
		toolStrip.addButton(close);
		toolStrip.addSeparator();
		close.addClickHandler(event -> destroy());
		*/

		/*
		ToolStripButton cancel = new ToolStripButton();
		cancel.setTitle(I18N.message("cancel"));
		toolStrip.addButton(cancel);
		toolStrip.addSeparator();
		cancel.addClickHandler(event -> DocumentService.Instance.get()
				.unlock(Arrays.asList(OnlyOfficeEditor.this.document.getId()), new AsyncCallback<>() {
					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
						destroy();
					}

					@Override
					public void onSuccess(Void result) {
						DocUtil.markUnlocked(document);
						DocumentController.get().setCurrentDocument(document);
						LD.contactingServer();
						GoogleService.Instance.get().delete(OnlyOfficeEditor.this.document.getExtResId(),
								new AsyncCallback<>() {
									@Override
									public void onFailure(Throwable caught) {
										LD.clearPrompt();
										GuiLog.serverError(caught);
										destroy();
									}

									@Override
									public void onSuccess(Void result) {
										LD.clearPrompt();
										destroy();
									}
								});
					}
				}));

		ToolStripButton checkin = new ToolStripButton();
		checkin.setTitle(document.getId() != 0 ? I18N.message("checkin") : I18N.message("save"));
		toolStrip.addButton(checkin);
		checkin.addClickHandler(event -> {
			if (document.getId() != 0) {
				new DriveCheckin(document, OnlyOfficeEditor.this).show();
			} else {
				LD.contactingServer();
				GoogleService.Instance.get().importDocuments(Arrays.asList(document.getExtResId()),
						FolderController.get().getCurrentFolder().getId(), document.getType(),
						new AsyncCallback<>() {
							@Override
							public void onFailure(Throwable caught) {
								GoogleUtil.handleGoogleServiceError(caught);
							}

							@Override
							public void onSuccess(Void result) {
								DocumentsPanel.get().refresh();

								// Delete the temporary resource in GDrive
								GoogleService.Instance.get().delete(document.getExtResId(), new AsyncCallback<>() {

									@Override
									public void onFailure(Throwable caught) {
										LD.clearPrompt();
										GuiLog.serverError(caught);
										destroy();
									}

									@Override
									public void onSuccess(Void ret) {
										LD.clearPrompt();
										destroy();
									}
								});
							}
						});
			}
		});
		*/

		//layout.setMembers(toolStrip, html);
		layout.setMembers(html);
	}
	
	private String getOnlyOfficeEditorUrl(long docId, String fileName) {
		return Util.contextPath() + "onlyoffice/editor?docId=" + docId + "&fileName="
				+ fileName + "&sid=" + Session.get().getSid();
	}	
}