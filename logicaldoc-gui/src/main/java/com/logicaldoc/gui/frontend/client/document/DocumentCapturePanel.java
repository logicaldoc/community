package com.logicaldoc.gui.frontend.client.document;

import java.util.Map;

import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.controllers.DocumentController;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.frontend.client.services.BarcodeService;
import com.logicaldoc.gui.frontend.client.services.ZonalOCRService;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.ButtonItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;

/**
 * Shows document's data capture options.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.4.2
 */
public class DocumentCapturePanel extends DocumentDetailTab {
	private static final String BARCODETEMPLATE = "barcodetemplate";

	private static final String OCRTEMPLATE = "ocrtemplate";

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

		if (Boolean.TRUE.equals(contains(form)))
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
		processOcr.addClickHandler(event -> {
			LD.contactingServer();
			ZonalOCRService.Instance.get().process(document.getId(), new DefaultAsyncCallback<>() {

				@Override
				public void onFailure(Throwable caught) {
					LD.clearPrompt();
					super.onFailure(caught);
				}

				@Override
				public void onSuccess(GUIDocument result) {
					LD.clearPrompt();
					DocumentController.get().modified(result);
				}
			});
		});

		SelectItem ocrTemplate = ItemFactory.newOCRTemplateSelector(true, documentTemplateId,
				document.getOcrTemplateId());
		ocrTemplate.setWrapTitle(false);
		ocrTemplate.setDisabled(!Feature.enabled(Feature.ZONAL_OCR) || documentTemplateId == null);
		ocrTemplate.addChangedHandler(changedHandler);
		ocrTemplate.addChangedHandler(event -> processOcr.setDisabled(true));

		ButtonItem processBarcode = new ButtonItem(I18N.message("process"));
		processBarcode.setAutoFit(true);
		processBarcode.setEndRow(true);
		processBarcode.setDisabled(!updateEnabled || document.getBarcodeTemplateId() == null);
		processBarcode.setColSpan(1);
		processBarcode.addClickHandler(event -> {
			LD.contactingServer();
			BarcodeService.Instance.get().process(document.getId(), new DefaultAsyncCallback<>() {
				@Override
				public void onFailure(Throwable caught) {
					LD.clearPrompt();
					super.onFailure(caught);
				}

				@Override
				public void onSuccess(GUIDocument result) {
					LD.clearPrompt();
					DocumentController.get().modified(result);
				}
			});
		});

		SelectItem barcodeTemplate = ItemFactory.newBarcodeTemplateSelector(true, documentTemplateId,
				document.getBarcodeTemplateId());
		barcodeTemplate.setWrapTitle(false);
		barcodeTemplate.setDisabled(!Feature.enabled(Feature.BARCODES));
		barcodeTemplate.addChangedHandler(changedHandler);
		barcodeTemplate.addChangedHandler(event -> processOcr.setDisabled(true));

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
	@Override
	public boolean validate() {
		Map<String, Object> values = vm.getValues();
		vm.validate();
		if (Boolean.FALSE.equals(vm.hasErrors())) {
			if (values.get(OCRTEMPLATE) == null || values.get(OCRTEMPLATE).toString().isEmpty())
				document.setOcrTemplateId(null);
			else {
				document.setOcrTemplateId(Long.parseLong(values.get(OCRTEMPLATE).toString()));
			}

			if (values.get(BARCODETEMPLATE) == null || values.get(BARCODETEMPLATE).toString().isEmpty())
				document.setBarcodeTemplateId(null);
			else {
				document.setBarcodeTemplateId(Long.parseLong(values.get(BARCODETEMPLATE).toString()));
			}
		}
		return !vm.hasErrors();
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