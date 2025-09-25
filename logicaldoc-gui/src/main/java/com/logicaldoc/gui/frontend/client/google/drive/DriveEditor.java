package com.logicaldoc.gui.frontend.client.google.drive;

import java.util.Arrays;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.controllers.DocumentController;
import com.logicaldoc.gui.common.client.controllers.FolderController;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.DocUtil;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.frontend.client.document.DocumentsPanel;
import com.logicaldoc.gui.frontend.client.google.GoogleAsyncCallback;
import com.logicaldoc.gui.frontend.client.google.GoogleService;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.widgets.HTMLFlow;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * This popup window is used to show the document in Google Drive.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.3
 */
public class DriveEditor extends Window {

	private static final String EDIT_HL = "/edit?hl=";

	private HTMLFlow html = new HTMLFlow();

	private VLayout layout = null;

	private GUIDocument document;

	public DriveEditor(final GUIDocument document) {
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
			else {
				// Creating a new document document, so delete the temporary
				// doc in Google Drive
				LD.contactingServer();
				GoogleService.Instance.get().delete(DriveEditor.this.document.getExtResId(),
						new GoogleAsyncCallback<>() {
							@Override
							public void onSuccess(Void result) {
								LD.clearPrompt();
								destroy();
							}
						});
			}
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
		String url = "https://docs.google.com/document/d/" + document.getExtResId() + EDIT_HL
				+ Session.get().getUser().getLanguage();
		if (Util.isSpreadsheetFile(document.getFileName()))
			url = "https://docs.google.com/spreadsheets/d/" + document.getExtResId() + EDIT_HL
					+ Session.get().getUser().getLanguage();
		if (Util.isPresentationFile(document.getFileName()))
			url = "https://docs.google.com/presentation/d/" + document.getExtResId() + EDIT_HL
					+ Session.get().getUser().getLanguage();

		String iframe = "<iframe src='" + url + "' style='border: 0px solid white; width:" + (getWidth() - 18)
				+ "px; height:" + (getHeight() - 68) + "px' scrolling='no'></iframe>";
		html.setContents(iframe);

		html = new HTMLFlow();
		html.setWidth100();
		html.setHeight100();
		html.setShowEdges(false);
		html.setContents(iframe);

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

		ToolStripButton cancel = new ToolStripButton();
		cancel.setTitle(I18N.message("cancel"));
		toolStrip.addButton(cancel);
		toolStrip.addSeparator();
		cancel.addClickHandler(event -> DocumentService.Instance.get()
				.unlock(Arrays.asList(DriveEditor.this.document.getId()), new DefaultAsyncCallback<>() {
					@Override
					public void onSuccess(Void result) {
						DocUtil.markUnlocked(document);
						DocumentController.get().setCurrentDocument(document);
						LD.contactingServer();
						GoogleService.Instance.get().delete(DriveEditor.this.document.getExtResId(),
								new DefaultAsyncCallback<>() {
									@Override
									public void onFailure(Throwable caught) {
										super.onFailure(caught);
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
				new DriveCheckin(document, DriveEditor.this).show();
			} else {
				LD.contactingServer();
				GoogleService.Instance.get().importDocuments(Arrays.asList(document.getExtResId()),
						FolderController.get().getCurrentFolder().getId(), document.getType(),
						new GoogleAsyncCallback<>() {
							@Override
							public void onSuccess(Void result) {
								DocumentsPanel.get().refresh();

								// Delete the temporary resource in GDrive
								GoogleService.Instance.get().delete(document.getExtResId(),
										new DefaultAsyncCallback<>() {

											@Override
											public void onFailure(Throwable caught) {
												super.onFailure(caught);
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

		layout.setMembers(toolStrip, html);
	}

	@Override
	public boolean equals(Object obj) {
		if (obj instanceof DriveEditor)
			return super.equals(obj);
		else
			return false;
	}

	@Override
	public int hashCode() {
		return super.hashCode();
	}
}