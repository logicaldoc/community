package com.logicaldoc.gui.frontend.client.zoho;

import java.util.Arrays;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.controllers.DocumentController;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.DocUtil;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.util.WindowUtils;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.logicaldoc.gui.frontend.client.services.ZohoService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * This popup window is used to show the document in Zoho.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.5.1
 */
public class ZohoEditor extends Window {

	private VLayout layout = null;

	private GUIDocument document;

	public ZohoEditor(final GUIDocument document) {
		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("editdoc") + ": " + document.getFileName());

		this.document = document;

		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		setAutoSize(true);
		centerInPage();
		setMargin(2);

		addCloseClickHandler(event -> onCancel());

		layout = new VLayout();
		layout.setMargin(1);
		layout.setWidth100();
		layout.setHeight100();
		addItem(layout);

		prepareBody();
	}

	/**
	 * Reloads a preview.
	 */
	private void prepareBody() {
		Label editorUrl = new Label(
				"<span style='text-decoration: underline'>" + I18N.message("clicktoopenzohoeditor") + "</span>");
		editorUrl.setHeight(30);
		editorUrl.setWidth(300);
		editorUrl.setWrap(false);
		editorUrl.addClickHandler(event -> {
			String url = "https://docs.zoho.com/writer/open/";
			if (document.getFileName().toLowerCase().contains(".xls")
					|| document.getFileName().toLowerCase().contains(".ods"))
				url = "https://docs.zoho.com/sheet/ropen.do?rid=";
			else if (document.getFileName().toLowerCase().contains(".ppt")
					|| document.getFileName().toLowerCase().contains(".odp"))
				url = "https://docs.zoho.com/show/open/";
			url += document.getExtResId();

			WindowUtils.openUrlInNewTab(url);
		});

		Label spacer20 = new Label("");
		spacer20.setHeight(20);

		Label clickHint = new Label(I18N.message("clickoncheckin"));
		clickHint.setWrap(false);
		clickHint.setWidth(300);
		clickHint.setHeight(30);

		Label spacer30 = new Label("");
		spacer30.setHeight(30);

		IButton cancel = new IButton(I18N.message("cancel"));
		cancel.setAutoFit(true);
		cancel.addClickHandler(event -> onCancel());

		IButton checkin = new IButton(I18N.message("cancel"));
		checkin.setTitle(document.getId() != 0 ? I18N.message("checkin") : I18N.message("save"));
		checkin.setAutoFit(true);
		checkin.addClickHandler(event -> new ZohoCheckin(document, ZohoEditor.this).show());

		Label hSpacer = new Label("");
		hSpacer.setWidth(15);

		HLayout buttonsContainer = new HLayout();
		buttonsContainer.setAutoWidth();
		buttonsContainer.setHeight(20);
		buttonsContainer.setAlign(Alignment.CENTER);

		buttonsContainer.setMembers(checkin, hSpacer, cancel);

		layout.setMembers(editorUrl, spacer20, clickHint, spacer30, buttonsContainer);
	}

	private void onCancel() {
		DocumentService.Instance.get().unlock(Arrays.asList(ZohoEditor.this.document.getId()),
				new DefaultAsyncCallback<>() {
					@Override
					public void onFailure(Throwable caught) {
						super.onFailure(caught);
						destroy();
					}

					@Override
					public void onSuccess(Void result) {
						DocUtil.markUnlocked(document);
						DocumentController.get().setCurrentDocument(document);
						LD.contactingServer();
						ZohoService.Instance.get().delete(ZohoEditor.this.document.getExtResId(),
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
				});
	}
	
	@Override
	public boolean equals(Object other) {
		return super.equals(other);
	}

	@Override
	public int hashCode() {
		return super.hashCode();
	}
}