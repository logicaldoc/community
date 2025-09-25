package com.logicaldoc.gui.frontend.client.onlyoffice;

import java.util.LinkedHashMap;

import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.controllers.FolderController;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.Util;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.SubmitItem;
import com.smartgwt.client.widgets.form.fields.TextItem;

/**
 * This popup window is used to create a new document using OnlyOffice plugin.
 * 
 * @author Alessandro Gasparini - LogicalDOC
 * @since 9.1
 */
public class OnlyOfficeCreate extends Window {

	public OnlyOfficeCreate() {
		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle("Select document type");
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		setAutoSize(true);
		centerInPage();

		DynamicForm form = new DynamicForm();
		ValuesManager vm = new ValuesManager();
		form.setValuesManager(vm);
		form.setTitleOrientation(TitleOrientation.TOP);
		form.setNumCols(1);

		TextItem fnameField = ItemFactory.newTextItem("filename", null);
		fnameField.setRequired(true);
		fnameField.setWidth(200);

		SelectItem typeSelector = OnlyOfficeCreate.newDoctypeSelector("docx");

		SubmitItem create = new SubmitItem();
		create.setTitle(I18N.message("create"));
		create.addClickHandler(click -> {
			onCreate(vm);
			click.cancel();
		});

		form.setItems(fnameField, typeSelector, create);

		addItem(form);
	}

	private void onCreate(ValuesManager vm) {

		if (Boolean.FALSE.equals(vm.validate()))
			return;

		String filename = vm.getValueAsString("filename").trim();

		String docType = vm.getValueAsString("doctype");

		GUIDocument vo = new GUIDocument();
		vo.setId(0);
		vo.setFileName(Util.getBaseName(filename) + "." + docType);
		vo.setType(docType);
		vo.setFolder(FolderController.get().getCurrentFolder());

		new OnlyOfficeEditor(vo).show();

		destroy();
	}

	public static SelectItem newDoctypeSelector(String value) {
		SelectItem selector = new SelectItem();

		LinkedHashMap<String, String> opts = new LinkedHashMap<>();
		opts.put("docx", "Document");
		opts.put("xlsx", "Spreadsheeet");
		opts.put("pptx", "Presentation");
		opts.put("docxf", "PDF form");

		selector.setValueMap(opts);
		selector.setName("doctype");
		selector.setTitle("Document Type");
		selector.setValue(value);
		selector.setWidth(100);

		return selector;
	}
}