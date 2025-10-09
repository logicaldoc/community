package com.logicaldoc.gui.frontend.client.onlyoffice;

import java.util.Date;

import com.google.gwt.http.client.URL;
import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.Util;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.widgets.HTMLFlow;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * This popup window is used to open the document in OnlyOffice editor.
 * 
 * @author Alessandro Gasparini - LogicalDOC
 * @since 9.0.1
 */
public class OnlyOfficeEditor extends Window {

	private static final String ONLYOFFICE_EDITOR_DOC_ID = "onlyoffice/editor?docId=";

	private static final String FILE_NAME = "&fileName=";

	public OnlyOfficeEditor(final GUIDocument document) {

		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		if (document.getId() > 0)
			setTitle(I18N.message("editdoc") + ": " + document.getFileName());
		else
			setTitle(I18N.message("createdoc") + ": " + document.getFileName());

		setWidth(com.google.gwt.user.client.Window.getClientWidth());
		setHeight(com.google.gwt.user.client.Window.getClientHeight());
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();
		setMargin(2);

		VLayout layout = new VLayout();
		layout.setMargin(1);
		layout.setWidth100();
		layout.setHeight100();
		addItem(layout);

		addCloseClickHandler(event -> onClose(document.getId()));

		addResizedHandler(event -> reloadBody(document, layout));

		if ("fillForms".equals(document.getComment())) {
			// the document is being edited, so declare the editing
			OnlyOfficeService.Instance.get().startFilling(document.getId(), new DefaultAsyncCallback<>() {
				@Override
				public void handleSuccess(Void result) {
					reloadBody(document, layout);
				}
			});
		} else {
			// the document is being edited, so declare the editing
			OnlyOfficeService.Instance.get().startEditing(document.getId(), new DefaultAsyncCallback<>() {
				@Override
				public void handleSuccess(Void result) {
					reloadBody(document, layout);
				}
			});
		}
	}

	private void onClose(long docId) {
		// the document is being edited, so declare the end of editing
		OnlyOfficeService.Instance.get().endEditing(docId, new DefaultAsyncCallback<>() {
			@Override
			public void handleSuccess(Void result) {
				destroy();
			}
		});
	}

	private void reloadBody(GUIDocument document, VLayout layout) {
		String iframe = "<iframe src='" + getOnlyOfficeEditorUrl(document) + "' style='border: 0px solid white; width:"
				+ (getWidth() - 18) + "px; height:" + (getHeight() - 68) + "px' scrolling='no'></iframe>";

		HTMLFlow html = new HTMLFlow();
		html.setWidth100();
		html.setHeight100();
		html.setShowEdges(false);
		html.setContents(iframe);
		layout.setMembers(html);
	}

	private String getOnlyOfficeEditorUrl(GUIDocument document) {
		String finalUrl;

		if (document.getId() > 0) {
			finalUrl = Util.contextPath() + ONLYOFFICE_EDITOR_DOC_ID + document.getId() + FILE_NAME
					+ URL.encode(document.getFileName());

			// fillforms mode
			if ((document.getComment() != null) && document.getComment().equals("fillForms")) {
				finalUrl = Util.contextPath() + ONLYOFFICE_EDITOR_DOC_ID + document.getId() + FILE_NAME
						+ URL.encode(document.getFileName()) + "&mode=fillForms";
			}
		} else {
			finalUrl = Util.contextPath() + ONLYOFFICE_EDITOR_DOC_ID + document.getId() + FILE_NAME
					+ URL.encode(document.getFileName()) + "&fileExt=" + document.getType() + "&folderId="
					+ document.getFolder().getId();
		}

		finalUrl += "&sid=" + Session.get().getSid() + "&rd=" + new Date().getTime();

		return finalUrl;
	}
}