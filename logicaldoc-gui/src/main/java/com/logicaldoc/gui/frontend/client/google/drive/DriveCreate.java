package com.logicaldoc.gui.frontend.client.google.drive;

import java.util.LinkedHashMap;

import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.frontend.client.google.GoogleAsyncCallback;
import com.logicaldoc.gui.frontend.client.google.GoogleService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.SubmitItem;
import com.smartgwt.client.widgets.form.fields.TextItem;

/**
 * This popup window is used to create a new document in Google Drive.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.3
 */
public class DriveCreate extends Window {
	private SubmitItem create;

	private ValuesManager vm;

	public DriveCreate() {
		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("createdoc"));
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		setAutoSize(true);
		centerInPage();

		DynamicForm form = new DynamicForm();
		vm = new ValuesManager();
		form.setValuesManager(vm);
		form.setTitleOrientation(TitleOrientation.TOP);

		TextItem fileName = ItemFactory.newTextItem("filename", null);
		fileName.setRequired(true);
		fileName.setWidth(200);

		SelectItem type = ItemFactory.newSelectItem("type", I18N.message("type"));
		LinkedHashMap<String, String> map = new LinkedHashMap<>();
		map.put("doc", "doc");
		map.put("docx", "docx");
		map.put("txt", "txt");
		map.put("xlsx", "xlsx");
		map.put("pptx", "pptx");
		type.setValueMap(map);
		type.setValue("docx");
		type.setWidth(80);
		type.setEndRow(true);
		type.setRequired(true);

		create = new SubmitItem();
		create.setTitle(I18N.message("create"));
		create.setAlign(Alignment.RIGHT);
		create.addClickHandler(event -> onCreate());

		form.setItems(fileName, type, create);

		addItem(form);
	}

	public void onCreate() {
		if (Boolean.FALSE.equals(vm.validate()))
			return;
		hide();
		LD.contactingServer();

		final String type = vm.getValueAsString("type");
		String filename = vm.getValueAsString("filename");
		if (!filename.toLowerCase().endsWith("." + type))
			filename = filename + "." + type;
		final String fn = filename;

		GoogleService.Instance.get().create(filename, new GoogleAsyncCallback<>() {
			@Override
			public void onSuccess(String resId) {
				GUIDocument document = new GUIDocument();
				document.setFileName(fn);
				document.setType(type);
				document.setExtResId(resId);
				DriveEditor editor = new DriveEditor(document);
				editor.show();
				destroy();
			}
		});
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