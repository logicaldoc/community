package com.logicaldoc.gui.frontend.client.document;

import java.util.Map;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.observer.DocumentController;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.widgets.ContactingServer;
import com.logicaldoc.gui.frontend.client.services.BarcodeService;
import com.logicaldoc.gui.frontend.client.services.ZonalOCRService;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.ButtonItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.form.fields.events.ClickEvent;
import com.smartgwt.client.widgets.form.fields.events.ClickHandler;

/**
 * Shows document's data capture options.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.4.2
 */
public class DocumentCapturePanel extends DocumentDetailTab {
	private DynamicForm form = new DynamicForm();

	private ValuesManager vm = new ValuesManager();

	private boolean processButton = true;

	public DocumentCapturePanel(GUIDocument document, ChangedHandler changedHandler, boolean processButton) {
		super(document, changedHandler);
		setWidth100();
		setHeight100();
		setMembersMargin(20);
		this.processButton = processButton;
		refresh(document.getTemplateId());
	}

	public void refresh(Long documentTemplateId) {
		vm.clearValues();
		vm.clearErrors(false);

		if (form != null)
			form.destroy();

		if (contains(form))
			removeChild(form);
		
		form = new DynamicForm();
		form.setValuesManager(vm);
		form.setTitleOrientation(TitleOrientation.TOP);
		form.setWrapItemTitles(false);

		ButtonItem processOcr = new ButtonItem(I18N.message("process"));
		processOcr.setAutoFit(true);
		processOcr.setEndRow(true);
		processOcr.setDisabled(!updateEnabled || document.getOcrTemplateId() == null);
		processOcr.setColSpan(1);
		processOcr.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				ContactingServer.get().show();
				ZonalOCRService.Instance.get().process(document.getId(), new AsyncCallback<GUIDocument>() {

					@Override
					public void onFailure(Throwable caught) {
						ContactingServer.get().hide();
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(GUIDocument result) {
						ContactingServer.get().hide();
						DocumentController.get().modified(result);
					}
				});
			}
		});

		SelectItem ocrTemplate = ItemFactory.newOCRTemplateSelector(true, documentTemplateId,
				document.getOcrTemplateId());
		ocrTemplate.setWrapTitle(false);
		ocrTemplate.setDisabled(!Feature.enabled(Feature.ZONAL_OCR) || documentTemplateId == null);
		ocrTemplate.addChangedHandler(changedHandler);
		ocrTemplate.addChangedHandler(new ChangedHandler() {

			@Override
			public void onChanged(ChangedEvent event) {
				processOcr.setDisabled(true);
			}
		});

		ButtonItem processBarcode = new ButtonItem(I18N.message("process"));
		processBarcode.setAutoFit(true);
		processBarcode.setEndRow(true);
		processBarcode.setDisabled(!updateEnabled || document.getBarcodeTemplateId() == null);
		processBarcode.setColSpan(1);
		processBarcode.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				ContactingServer.get().show();
				BarcodeService.Instance.get().process(document.getId(), new AsyncCallback<GUIDocument>() {
					@Override
					public void onFailure(Throwable caught) {
						ContactingServer.get().hide();
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(GUIDocument result) {
						ContactingServer.get().hide();
						DocumentController.get().modified(result);
					}
				});
			}
		});

		SelectItem barcodeTemplate = ItemFactory.newBarcodeTemplateSelector(true, documentTemplateId,
				document.getBarcodeTemplateId());
		barcodeTemplate.setWrapTitle(false);
		barcodeTemplate.setDisabled(!Feature.enabled(Feature.BARCODES));
		barcodeTemplate.addChangedHandler(changedHandler);
		barcodeTemplate.addChangedHandler(new ChangedHandler() {

			@Override
			public void onChanged(ChangedEvent event) {
				processOcr.setDisabled(true);
			}
		});

		StaticTextItem ocrProcessed = ItemFactory.newStaticTextItem("zonalocrprocessed", "processedbyzonalocr",
				document.getOcrd() == 1 ? I18N.message("yes") : I18N.message("no"));
		ocrProcessed.setWrapTitle(false);

		StaticTextItem barcodeProcessed = ItemFactory.newStaticTextItem("barcodeprocessed", "processedbybarcode",
				document.getBarcoded() == 1 ? I18N.message("yes") : I18N.message("no"));
		barcodeProcessed.setWrapTitle(false);

		if (processButton)
			form.setItems(ocrTemplate, ocrProcessed, processOcr, barcodeTemplate, barcodeProcessed, processBarcode);
		else
			form.setItems(ocrTemplate, barcodeTemplate);

		addMember(form);
	}

	@SuppressWarnings("unchecked")
	public boolean validate() {
		Map<String, Object> values = (Map<String, Object>) vm.getValues();
		vm.validate();
		if (!vm.hasErrors()) {
			if (values.get("ocrtemplate") == null || values.get("ocrtemplate").toString().isEmpty())
				document.setOcrTemplateId(null);
			else {
				document.setOcrTemplateId(Long.parseLong(values.get("ocrtemplate").toString()));
			}

			if (values.get("barcodetemplate") == null || values.get("barcodetemplate").toString().isEmpty())
				document.setBarcodeTemplateId(null);
			else {
				document.setBarcodeTemplateId(Long.parseLong(values.get("barcodetemplate").toString()));
			}
		}
		return !vm.hasErrors();
	}
}