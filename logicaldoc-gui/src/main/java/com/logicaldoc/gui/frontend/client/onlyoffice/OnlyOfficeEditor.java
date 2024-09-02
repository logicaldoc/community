package com.logicaldoc.gui.frontend.client.onlyoffice;

import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;

import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.Util;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.widgets.HTMLFlow;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * This popup window is used to open the document in OnlyOffice editor.
 * 
 * @author Alessandro Gasparini - LogicalDOC
 * @since 9.0
 */
public class OnlyOfficeEditor extends Window {

	private HTMLFlow html = new HTMLFlow();

	private VLayout layout = null;

	private GUIDocument document;

	public OnlyOfficeEditor(final GUIDocument document) throws UnsupportedEncodingException {
		
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

		addCloseClickHandler(event -> destroy());

		addResizedHandler(event -> {
			try {
				reloadBody();
			} catch (UnsupportedEncodingException e) {
				GuiLog.error(e.getMessage());
			}
		});

		layout = new VLayout();
		layout.setMargin(1);
		layout.setWidth100();
		layout.setHeight100();
		addItem(layout);

		reloadBody();
	}

	/**
	 * Reloads a preview.
	 * @throws UnsupportedEncodingException 
	 */
	private void reloadBody() throws UnsupportedEncodingException {
	
		String url = getOnlyOfficeEditorUrl();

		String iframe = "<iframe src='" + url + "' style='border: 0px solid white; width:" + (getWidth() - 18)
				+ "px; height:" + (getHeight() - 68) + "px' scrolling='no'></iframe>";
		
		html = new HTMLFlow();
		html.setWidth100();
		html.setHeight100();
		html.setShowEdges(false);
		html.setContents(iframe);

		layout.setMembers(html);
	}
	
	private String getOnlyOfficeEditorUrl() throws UnsupportedEncodingException {
		
		String finalUrl;
		
		if (document.getId() > 0) {
			finalUrl = Util.contextPath() + "onlyoffice/editor?docId=" + document.getId() 
			+ "&fileName=" + document.getFileName() + "&sid=" + Session.get().getSid();
		} else {
			finalUrl = Util.contextPath() + "onlyoffice/editor?docId=" + document.getId() 
			+ "&fileName=" + URLEncoder.encode(document.getFileName(), "UTF-8") + "&fileExt=" +document.getType() +"&folderId=" +document.getFolder().getId() + "&sid=" + Session.get().getSid();
		}
		
		return finalUrl;
	}	
}