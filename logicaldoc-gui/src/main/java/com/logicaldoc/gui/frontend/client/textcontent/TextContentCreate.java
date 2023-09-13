package com.logicaldoc.gui.frontend.client.textcontent;

import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.controllers.FolderController;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.Util;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.SubmitItem;
import com.smartgwt.client.widgets.form.fields.TextItem;

/**
 * This popup window is used to create a new web content.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.6.1
 */
public class TextContentCreate extends Window {
	private static final String TEMPLATE = "template";

	private SubmitItem create;

	private ValuesManager vm;

	public TextContentCreate() {
		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("createtextcontent"));
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

		TextItem filename = ItemFactory.newTextItem("filename", null);
		filename.setRequired(true);
		filename.setWidth(200);

		SelectItem template = ItemFactory.newTemplateSelector(true, null);

		create = new SubmitItem();
		create.setTitle(I18N.message("create"));
		create.addClickHandler(event -> onCreate());

		form.setItems(filename, template, create);

		addItem(form);
	}

	public void onCreate() {
		if (Boolean.FALSE.equals(vm.validate()))
			return;

		String filename = vm.getValueAsString("filename").trim();
		if (!filename.contains("."))
			filename = filename + ".txt";
		if (!Util.isTextFile(filename)) {
			SC.warn(I18N.message("nottextextension"));
			return;
		}

		GUIDocument vo = new GUIDocument();
		if (vm.getValueAsString(TEMPLATE) == null || "".equals(vm.getValueAsString(TEMPLATE)))
			vo.setTemplateId(null);
		else {
			vo.setTemplateId(Long.parseLong(vm.getValueAsString(TEMPLATE)));
		}

		String ext = filename.substring(filename.indexOf('.') + 1);

		vo.setType(ext);
		vo.setFileName(filename);
		vo.setStatus(1);
		vo.setLanguage(I18N.getDefaultLocaleForDoc());
		vo.setFolder(FolderController.get().getCurrentFolder());

		TextContentEditor popup = new TextContentEditor(vo, "");
		popup.show();

		destroy();
	}
}