package com.logicaldoc.gui.frontend.client.folder;

import java.util.Map;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
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
import com.smartgwt.client.widgets.form.fields.events.ClickHandler;

/**
 * Shows folder's OCR options.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.4.2
 */
public class FolderCapturePanel extends FolderDetailTab {
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

		if (contains(form))
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
		applySubFolders.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				LD.contactingServer();
				FolderService.Instance.get().applyOCR(folder.getId(), new AsyncCallback<Void>() {

					@Override
					public void onFailure(Throwable caught) {
						LD.clearPrompt();
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(Void v) {
						LD.clearPrompt();
					}
				});
			}
		});

		SelectItem ocrTemplate = ItemFactory.newOCRTemplateSelector(true, documentTemplateId,
				folder.getOcrTemplateId());
		ocrTemplate.setWrapTitle(false);
		ocrTemplate.setDisabled(!Feature.enabled(Feature.ZONAL_OCR) && folder.getTemplateId() == null);
		ocrTemplate.addChangedHandler(changedHandler);
		ocrTemplate.addChangedHandler(new ChangedHandler() {

			@Override
			public void onChanged(ChangedEvent event) {
				applySubFolders.setDisabled(true);
			}
		});
		ocrTemplate.setDisabled(documentTemplateId == null);

		SelectItem barcodeTemplate = ItemFactory.newBarcodeTemplateSelector(true, documentTemplateId,
				folder.getBarcodeTemplateId());
		barcodeTemplate.setWrapTitle(false);
		barcodeTemplate.setDisabled(!Feature.enabled(Feature.BARCODES));
		barcodeTemplate.addChangedHandler(changedHandler);
		barcodeTemplate.addChangedHandler(new ChangedHandler() {

			@Override
			public void onChanged(ChangedEvent event) {
				applySubFolders.setDisabled(true);
			}
		});

		form.setItems(ocrTemplate, barcodeTemplate, applySubFolders);
		addMember(form);
	}

	@SuppressWarnings("unchecked")
	public boolean validate() {
		Map<String, Object> values = (Map<String, Object>) vm.getValues();
		vm.validate();
		if (!vm.hasErrors()) {
			if (values.get("ocrtemplate") == null || values.get("ocrtemplate").toString().isEmpty())
				folder.setOcrTemplateId(null);
			else {
				folder.setOcrTemplateId(Long.parseLong(values.get("ocrtemplate").toString()));
			}

			if (values.get("barcodetemplate") == null || values.get("barcodetemplate").toString().isEmpty())
				folder.setBarcodeTemplateId(null);
			else {
				folder.setBarcodeTemplateId(Long.parseLong(values.get("barcodetemplate").toString()));
			}
		}
		return !vm.hasErrors();
	}
}