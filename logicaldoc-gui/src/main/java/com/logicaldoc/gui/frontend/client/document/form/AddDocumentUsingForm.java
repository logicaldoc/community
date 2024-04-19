package com.logicaldoc.gui.frontend.client.document.form;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUITemplate;
import com.logicaldoc.gui.common.client.controllers.FolderController;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.frontend.client.services.TemplateService;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.ButtonItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.TextItem;

/**
 * This popup window is used to create a new document based on a selected form.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.3
 */
public class AddDocumentUsingForm extends Window {

	private DynamicForm form = new DynamicForm();

	private SelectItem formSelector;

	public AddDocumentUsingForm() {
		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);

		setTitle(I18N.message("addform"));
		setAutoSize(true);
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();

		formSelector = ItemFactory.newFormSelector();
		formSelector.setWrapTitle(false);
		formSelector.setRequired(true);

		TextItem title = ItemFactory.newTextItem("title", null);
		title.setRequired(true);

		ButtonItem save = new ButtonItem();
		save.setTitle(I18N.message("save"));
		save.setAutoFit(true);
		save.addClickHandler(event -> onSave());

		form.setNumCols(2);
		form.setTitleOrientation(TitleOrientation.TOP);
		form.setFields(title, formSelector, save);
		addItem(form);
	}

	public void onSave() {
		if (!form.validate())
			return;

		long formId = Long.parseLong(form.getValue("form").toString());
		Long templateId = formSelector.getSelectedRecord().getAttributeAsLong("templateId");

		final GUIDocument frm = new GUIDocument();
		frm.setFolder(FolderController.get().getCurrentFolder());
		frm.setFormId(formId);
		frm.setAttributes(null);
		frm.setFileName(form.getValueAsString("title").trim() + ".pdf");
		frm.setLanguage(I18N.getDefaultLocaleForDoc());
		frm.setTemplateId(templateId);

		TemplateService.Instance.get().getTemplate(frm.getTemplateId(), new AsyncCallback<>() {

			@Override
			public void onFailure(Throwable caught) {
				GuiLog.serverError(caught);
			}

			@Override
			public void onSuccess(GUITemplate guiTemplate) {
				frm.setTemplate(guiTemplate.getName());
				frm.setAttributes(guiTemplate.getAttributes());
				FillForm fillForm = new FillForm(frm);
				fillForm.show();
				destroy();
			}
		});
	}
}