package com.logicaldoc.gui.frontend.client.folder;

import java.util.Map;

import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
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
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.form.fields.events.ClickEvent;

/**
 * Shows folder's OCR options.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.4.2
 */
public class FolderCapturePanel extends FolderDetailTab {
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
		form.setWrapItemTitles(false);

		ButtonItem applySubFolders = new ButtonItem(I18N.message("applytosubfolders"));
		applySubFolders.setAutoFit(true);
		applySubFolders.setEndRow(true);
		applySubFolders.setDisabled(!folder.isWrite());
		applySubFolders.setColSpan(1);
		applySubFolders.addClickHandler((ClickEvent event) -> {
			LD.contactingServer();
			FolderService.Instance.get().applyOCR(folder.getId(), new DefaultAsyncCallback<>() {

				@Override
				public void onFailure(Throwable caught) {
					LD.clearPrompt();
					super.onFailure(caught);
				}

				@Override
				public void onSuccess(Void v) {
					LD.clearPrompt();
				}
			});
		});

		SelectItem ocrTemplate = ItemFactory.newOCRTemplateSelector(true, documentTemplateId,
				folder.getOcrTemplateId());
		ocrTemplate.setWrapTitle(false);
		ocrTemplate.setDisabled(!Feature.enabled(Feature.ZONAL_OCR) && folder.getTemplateId() == null);
		ocrTemplate.addChangedHandler(changedHandler);
		ocrTemplate.addChangedHandler((ChangedEvent event) -> applySubFolders.setDisabled(true));
		ocrTemplate.setDisabled(documentTemplateId == null);

		SelectItem barcodeTemplate = ItemFactory.newBarcodeTemplateSelector(true, documentTemplateId,
				folder.getBarcodeTemplateId());
		barcodeTemplate.setWrapTitle(false);
		barcodeTemplate.setDisabled(!Feature.enabled(Feature.BARCODES));
		barcodeTemplate.addChangedHandler(changedHandler);
		barcodeTemplate.addChangedHandler((ChangedEvent event) -> applySubFolders.setDisabled(true));

		form.setItems(ocrTemplate, barcodeTemplate, applySubFolders);
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
		}
		return !vm.hasErrors();
	}
}