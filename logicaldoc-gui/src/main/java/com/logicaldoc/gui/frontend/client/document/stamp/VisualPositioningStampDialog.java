package com.logicaldoc.gui.frontend.client.document.stamp;

import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.stream.Collectors;

import com.google.gwt.http.client.Request;
import com.google.gwt.http.client.RequestBuilder;
import com.google.gwt.http.client.RequestCallback;
import com.google.gwt.http.client.RequestException;
import com.google.gwt.http.client.Response;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIStamp;
import com.logicaldoc.gui.common.client.controllers.DocumentController;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.widgets.ImageCropper;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.logicaldoc.gui.frontend.client.services.StampService;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.fields.CheckboxItem;
import com.smartgwt.client.widgets.form.fields.RadioGroupItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * This popup window is used to apply a stamp in a visually defined position
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.3.1
 */
public class VisualPositioningStampDialog extends Window {

	private static final String CURRENTPAGE = "currentpage";

	private GUIDocument firstSelectedDoc;

	private SpinnerItem pageCursor;

	private HLayout bottom = new HLayout();

	private GUIStamp stamp;

	private TextItem pageSelection;

	private RadioGroupItem pageOption;

	private ImageCropper cropper;

	private List<GUIDocument> documents;

	public VisualPositioningStampDialog(List<GUIDocument> documents, GUIStamp stamp) {
		this.documents = documents;
		this.stamp = stamp;

		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("applystamp"));
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
					firstSelectedDoc = documents.get(0);

					DocumentService.Instance.get().getById(firstSelectedDoc.getId(), new AsyncCallback<>() {

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

	private void showPage(int page) {
		if (cropper != null)
			bottom.removeMembers(bottom.getMembers());

		cropper = new ImageCropper(getPageUrl(page), 100, stamp.getAspectRatio());
		bottom.addMember(cropper);
	}

	private String getPageUrl(int page) {
		return Util.contextPath() + "convertjpg?docId=" + documents.get(0).getId() + "&page=" + page + "&random="
				+ new Date().getTime();
	}

	public void onApply() {
		int page = Integer.parseInt(pageCursor.getValueAsString());

		stamp.setExprX(
				"$PAGE_WIDTH * " + (double) cropper.getSelectionXCoordinate() / (double) cropper.getImageWidth());
		stamp.setExprY("$PAGE_HEIGHT * "
				+ (1 - ((double) cropper.getSelectionYCoordinate() / (double) cropper.getImageHeight())));
		stamp.setExprW("$PAGE_WIDTH * " + (double) cropper.getSelectionWidth() / (double) cropper.getImageWidth());
		stamp.setExprH("$PAGE_HEIGHT * " + ((double) cropper.getSelectionHeight() / (double) cropper.getImageHeight()));

		if (("01" + CURRENTPAGE).equals(pageOption.getValue())) {
			stamp.setPageOption(GUIStamp.PAGE_OPT_SEL);
			stamp.setPageSelection("" + page);
		} else {
			stamp.setPageOption(Integer.parseInt(pageOption.getValueAsString().substring(2)));
			stamp.setPageSelection(pageSelection.getValueAsString());
		}

		LD.contactingServer();

		StampService.Instance.get().applyStamp(documents.stream().map(d -> d.getId()).collect(Collectors.toList()),
				stamp, new AsyncCallback<>() {

					@Override
					public void onFailure(Throwable caught) {
						LD.clearPrompt();
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(Void result) {
						LD.clearPrompt();
						GuiLog.info(I18N.message("event.stamped"), null);
						for (GUIDocument doc : documents) {
							DocumentService.Instance.get().getById(doc.getId(), new AsyncCallback<>() {

								@Override
								public void onFailure(Throwable caught) {
									GuiLog.serverError(caught);
								}

								@Override
								public void onSuccess(GUIDocument document) {
									DocumentController.get().modified(document);
								}
							});
						}
						showPage(pageCursor.getValueAsInteger());
					}
				});
	}

	private void initGUI() {
		ToolStrip toolStrip = new ToolStrip();
		toolStrip.setHeight(20);
		toolStrip.setWidth100();
		toolStrip.addSpacer(2);

		SelectItem stampSelector = ItemFactory.newStampSelector();
		stampSelector.setTitle(I18N.message("stamp"));
		stampSelector.setWrapTitle(false);
		stampSelector.setRequired(true);
		stampSelector.setValue(stamp.getId());
		stampSelector.addChangedHandler(changed -> StampService.Instance.get()
				.getStamp(Long.parseLong(stampSelector.getValueAsString()), new AsyncCallback<>() {

					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(GUIStamp stmp) {
						VisualPositioningStampDialog.this.stamp = stmp;
					}
				}));

		ToolStripButton apply = new ToolStripButton();
		apply.setTitle(I18N.message("apply"));
		apply.addClickHandler(event -> onApply());

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

		CheckboxItem maintainAspectRatio = ItemFactory.newCheckbox("maintainaspectratio", "maintainaspectratio");
		maintainAspectRatio.setVisible(stamp.getType() != GUIStamp.TYPE_TEXT);
		maintainAspectRatio.setValue(stamp.getType() != GUIStamp.TYPE_TEXT);
		maintainAspectRatio.addChangedHandler(event -> {
			if (cropper != null)
				cropper.setMaintainSelectionAspectRatio((Boolean) event.getValue());
		});

		pageOption = ItemFactory.newRadioGroup("pageOption", "stampin");
		HashMap<String, String> pageOptions = new HashMap<>();
		pageOptions.put("01" + CURRENTPAGE, I18N.message(CURRENTPAGE));
		pageOptions.put("02" + GUIStamp.PAGE_OPT_ALL, I18N.message("allpages"));
		pageOptions.put("03" + GUIStamp.PAGE_OPT_FIRST, I18N.message("firstpage"));
		pageOptions.put("04" + GUIStamp.PAGE_OPT_LAST, I18N.message("lastpage"));
		pageOptions.put("05" + GUIStamp.PAGE_OPT_SEL, I18N.message("selection"));
		pageOption.setValueMap(pageOptions);
		pageOption.setValue("01" + CURRENTPAGE);
		pageOption.setWrap(false);

		pageSelection = ItemFactory.newTextItem("pageSelection", I18N.message("selection"), stamp.getPageSelection());
		pageSelection.setShowTitle(false);

		toolStrip.addFormItem(stampSelector);
		toolStrip.addFormItem(pageCursor);
		toolStrip.addButton(zoomIn);
		toolStrip.addButton(zoomOut);
		toolStrip.addSeparator();
		toolStrip.addFormItem(maintainAspectRatio);
		toolStrip.addSeparator();
		toolStrip.addFormItem(pageOption);
		toolStrip.addFormItem(pageSelection);
		toolStrip.addSeparator();
		toolStrip.addButton(apply);
		toolStrip.addButton(close);

		addItem(toolStrip);
		addItem(bottom);

		showPage(1);
	}
}