package com.logicaldoc.gui.frontend.client.folder;

import java.util.Map;

import com.logicaldoc.gui.common.client.EmptyAsyncCallback;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.frontend.client.services.FolderService;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.ButtonItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;

/**
 * Shows folder's OCR options.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.4.2
 */
public class FolderCapturePanel extends FolderDetailTab {
	private static final String FILLER = "filler";

	private static final String BARCODETEMPLATE = "barcodetemplate";

	private static final String OCRTEMPLATE = "ocrtemplate";

	private DynamicForm form = new DynamicForm();

	private ValuesManager vm = new ValuesManager();

	public FolderCapturePanel(GUIFolder folder, ChangedHandler changedHandler) {
		super(folder, changedHandler);
		setWidth100();
		setHeight100();
		setMembersMargin(20);
		refresh(folder.getTemplateId());
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
		form.setNumCols(3);
		form.setWrapItemTitles(false);

		ButtonItem applySubFolders = new ButtonItem(I18N.message("applytosubfolders"));
		applySubFolders.setAutoFit(true);
		applySubFolders.setEndRow(true);
		applySubFolders.setDisabled(!folder.isWrite());
		applySubFolders.setColSpan(1);
		applySubFolders.addClickHandler(click -> {
			LD.contactingServer();
			FolderService.Instance.get().applyCapture(folder.getId(), new EmptyAsyncCallback<>());
		});

		SelectItem ocrTemplate = ItemFactory.newOCRTemplateSelector(true, documentTemplateId,
				folder.getOcrTemplateId());
		ocrTemplate.setWrapTitle(false);
		ocrTemplate.setDisabled(!Feature.enabled(Feature.ZONAL_OCR) && folder.getTemplateId() == null);
		ocrTemplate.addChangedHandler(changedHandler);
		ocrTemplate.addChangedHandler(changed -> applySubFolders.setDisabled(true));
		ocrTemplate.setDisabled(documentTemplateId == null);

		SelectItem barcodeTemplate = ItemFactory.newBarcodeTemplateSelector(true, documentTemplateId,
				folder.getBarcodeTemplateId());
		barcodeTemplate.setWrapTitle(false);
		barcodeTemplate.setDisabled(!Feature.enabled(Feature.BARCODES));
		barcodeTemplate.addChangedHandler(changedHandler);
		barcodeTemplate.addChangedHandler(changed -> applySubFolders.setDisabled(true));

		SelectItem filler = ItemFactory.newFillerSelector(true, folder.getFillerId());
		filler.setWrapTitle(false);
		filler.setDisabled(!Feature.enabled(Feature.AUTOFILL));
		filler.addChangedHandler(changedHandler);
		filler.addChangedHandler(changed -> applySubFolders.setDisabled(true));

		form.setItems(ocrTemplate, barcodeTemplate, filler, applySubFolders);
		addMember(form);
	}

	@SuppressWarnings("unchecked")
	@Override
	public boolean validate() {
		Map<String, Object> values = vm.getValues();
		vm.validate();
		if (Boolean.FALSE.equals(vm.hasErrors())) {
			if (values.get(OCRTEMPLATE) == null || values.get(OCRTEMPLATE).toString().isEmpty())
				folder.setOcrTemplateId(null);
			else {
				folder.setOcrTemplateId(Long.parseLong(values.get(OCRTEMPLATE).toString()));
			}

			if (values.get(BARCODETEMPLATE) == null || values.get(BARCODETEMPLATE).toString().isEmpty())
				folder.setBarcodeTemplateId(null);
			else {
				folder.setBarcodeTemplateId(Long.parseLong(values.get(BARCODETEMPLATE).toString()));
			}
			
			if (values.get(FILLER) == null || values.get(FILLER).toString().isEmpty())
				folder.setFillerId(null);
			else {
				folder.setFillerId(Long.parseLong(values.get(FILLER).toString()));
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