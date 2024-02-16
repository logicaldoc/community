package com.logicaldoc.gui.frontend.client.document.signature;

import java.util.Date;
import java.util.List;

import com.google.gwt.http.client.Request;
import com.google.gwt.http.client.RequestBuilder;
import com.google.gwt.http.client.RequestCallback;
import com.google.gwt.http.client.RequestException;
import com.google.gwt.http.client.Response;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.widgets.ImageCropper;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.logicaldoc.gui.frontend.client.services.SignService;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * This popup window is used to apply a digital signature in a visually defined
 * position
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.3.1
 */
public class VisualPositioningDigitalSignatureDialog extends Window {

	private GUIDocument firstSelectedDoc;

	private SpinnerItem pageCursor;

	private HLayout bottom = new HLayout();

	private ImageCropper cropper;

	private List<Long> docIds;

	private String reason;

	public VisualPositioningDigitalSignatureDialog(List<Long> docIds, String reason) {
		this.docIds = docIds;
		this.reason = reason;

		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("signature"));
		setWidth100();
		setHeight100();

		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();

		LD.contactingServer();

		// Prepare the conversion in JPG first
		RequestBuilder builder = new RequestBuilder(RequestBuilder.GET, getPageUrl(1));
		try {
			builder.sendRequest("", new RequestCallback() {
				public void onError(Request request, Throwable exception) {
					GuiLog.serverError(exception);
					LD.clearPrompt();
				}

				public void onResponseReceived(Request request, Response response) {
					DocumentService.Instance.get().getById(docIds.get(0), new AsyncCallback<>() {

						@Override
						public void onFailure(Throwable caught) {
							GuiLog.serverError(caught);
							LD.clearPrompt();
						}

						@Override
						public void onSuccess(GUIDocument doc) {
							LD.clearPrompt();
							firstSelectedDoc = doc;
							initGUI();
						}
					});
				}
			});
		} catch (RequestException e) {
			LD.clearPrompt();
			GuiLog.error(e.getMessage(), null, e);
		}
	}

	private void initGUI() {
		ToolStrip toolStrip = new ToolStrip();
		toolStrip.setHeight(20);
		toolStrip.setWidth100();
		toolStrip.addSpacer(2);

		ToolStripButton sign = new ToolStripButton();
		sign.setTitle(I18N.message("sign"));
		sign.addClickHandler(event -> onSign());

		ToolStripButton close = new ToolStripButton();
		close.setTitle(I18N.message("close"));
		close.addClickHandler(event -> destroy());

		ToolStripButton zoomIn = new ToolStripButton();
		zoomIn.setTitle(I18N.message("zoomin"));
		zoomIn.addClickHandler(event -> {
			if (cropper != null)
				cropper.resize(100);
		});

		ToolStripButton zoomOut = new ToolStripButton();
		zoomOut.setTitle(I18N.message("zoomout"));
		zoomOut.addClickHandler(event -> {
			if (cropper != null)
				cropper.resize(-100);
		});

		pageCursor = ItemFactory.newSpinnerItem("page", "page", 1, 1,
				firstSelectedDoc.getPreviewPages() > 0 ? firstSelectedDoc.getPreviewPages() : 1);
		pageCursor.setHint("/" + (firstSelectedDoc.getPreviewPages() > 0 ? firstSelectedDoc.getPreviewPages() : 1));
		pageCursor.setSaveOnEnter(true);
		pageCursor.setImplicitSave(true);
		pageCursor.addChangedHandler(event -> showPage((Integer) pageCursor.getValue()));

		toolStrip.addFormItem(pageCursor);
		toolStrip.addButton(zoomIn);
		toolStrip.addButton(zoomOut);
		toolStrip.addSeparator();
		toolStrip.addButton(sign);
		toolStrip.addButton(close);

		addItem(toolStrip);
		addItem(bottom);

		showPage(1);
	}

	private void showPage(int page) {
		if (cropper != null)
			bottom.removeMembers(bottom.getMembers());

		cropper = new ImageCropper(getPageUrl(page), 100, 0);
		bottom.addMember(cropper);
	}

	private String getPageUrl(int page) {
		return Util.contextPath() + "convertjpg?docId=" + docIds.get(0) + "&page=" + page + "&random="
				+ new Date().getTime();
	}

	public void onSign() {
		int page = Integer.parseInt(pageCursor.getValueAsString());

		String exprX = "$PAGE_WIDTH * " + (double) cropper.getSelectionXCoordinate() / (double) cropper.getImageWidth();
		String exprY = "$PAGE_HEIGHT * "
				+ (double) cropper.getSelectionYCoordinate() / (double) cropper.getImageHeight();
		String exprW = "$PAGE_WIDTH * " + (double) cropper.getSelectionWidth() / (double) cropper.getImageWidth();

		LD.contactingServer();

		SignService.Instance.get().signDocuments(docIds, reason, page, exprX, exprY, exprW, new AsyncCallback<>() {

			@Override
			public void onFailure(Throwable caught) {
				LD.clearPrompt();
				GuiLog.serverError(caught);
			}

			@Override
			public void onSuccess(Void arg0) {
				destroy();
				GuiLog.info(I18N.message("event.signed"), null);
				LD.clearPrompt();
			}
		});
	}
}