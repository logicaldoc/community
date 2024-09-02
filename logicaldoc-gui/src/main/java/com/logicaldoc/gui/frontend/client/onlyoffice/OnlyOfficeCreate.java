package com.logicaldoc.gui.frontend.client.onlyoffice;

import java.util.LinkedHashMap;

import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.controllers.FolderController;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.SubmitItem;

/**
 * This popup window is used to create a new web content.
 * 
 * @author Alessandro Gasparini - LogicalDOC
 * @since 9.1
 */
public class OnlyOfficeCreate extends Window {

	private SubmitItem create;

	private ValuesManager vm;

	public OnlyOfficeCreate() {
		
		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle("Select document type");
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);		
		setAutoSize(true);
		centerInPage();

		DynamicForm form = new DynamicForm();
		vm = new ValuesManager();
		form.setValuesManager(vm);
		form.setTitleOrientation(TitleOrientation.TOP);
		form.setNumCols(1);

		SelectItem template = OnlyOfficeCreate.newDoctypeSelector("docx");

		create = new SubmitItem();
		create.setTitle(I18N.message("create"));
		create.addClickHandler(event -> onCreate());

		form.setItems(template, create);

		addItem(form);
	}

	public void onCreate() {
				
		String docType = vm.getValueAsString("doctype");
		
		GUIDocument vo = new GUIDocument();
		vo.setId(0);
		vo.setFileName("new." + docType);
		vo.setType(docType);			
		vo.setFolder(FolderController.get().getCurrentFolder());
		
		OnlyOfficeEditor popup = new OnlyOfficeEditor(vo);
		popup.show();		

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