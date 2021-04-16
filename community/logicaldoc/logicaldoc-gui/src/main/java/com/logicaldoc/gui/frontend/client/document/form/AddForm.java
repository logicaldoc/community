package com.logicaldoc.gui.frontend.client.document.form;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.frontend.client.document.DocumentsPanel;
import com.logicaldoc.gui.frontend.client.services.FormService;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.ButtonItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ClickEvent;
import com.smartgwt.client.widgets.form.fields.events.ClickHandler;

/**
 * This popup window is used to create a new document based on a selected form.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.3
 */
public class AddForm extends Window {

	private DynamicForm form = new DynamicForm();

	private SelectItem formSelector;

	public AddForm() {
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

		TextItem title = ItemFactory.newTextItem("title", "title", null);
		title.setRequired(true);

		ButtonItem save = new ButtonItem();
		save.setTitle(I18N.message("save"));
		save.setAutoFit(true);
		save.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				onSave();
			}
		});

		form.setNumCols(2);
		form.setTitleOrientation(TitleOrientation.TOP);
		form.setFields(title, formSelector, save);
		addItem(form);
	}

	public void onSave() {
		if (!form.validate())
			return;

		long formId = Long.parseLong(form.getValue("form").toString());
		String templateIdString = formSelector.getSelectedRecord().getAttributeAsString("templateId");

		final GUIDocument frm = new GUIDocument();
		frm.setFolder(Session.get().getCurrentFolder());
		frm.setFormId(formId);
		frm.setAttributes(null);
		frm.setFileName(form.getValueAsString("title").trim() + ".pdf");
		frm.setLanguage(I18N.getDefaultLocaleForDoc());

		if (templateIdString != null && !templateIdString.isEmpty()) {
			frm.setTemplateId(Long.parseLong(templateIdString));
			FillForm fillForm = new FillForm(frm);
			fillForm.show();
			destroy();
		} else {
			FormService.Instance.get().create(frm, new AsyncCallback<GUIDocument>() {
				@Override
				public void onFailure(Throwable caught) {
					GuiLog.serverError(caught);
				}

				@Override
				public void onSuccess(GUIDocument doc) {
					DocumentsPanel.get().refresh();
				}
			});
		}
	}
}