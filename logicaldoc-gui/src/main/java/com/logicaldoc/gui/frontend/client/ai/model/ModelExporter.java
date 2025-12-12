package com.logicaldoc.gui.frontend.client.ai.model;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.Util;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.ButtonItem;
import com.smartgwt.client.widgets.form.fields.CheckboxItem;

/**
 * This popup window is used to export an AI model.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2.2
 */
public class ModelExporter extends Window {
	
	public ModelExporter(long modelId) {
		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("exportmodel"));
		setAutoSize(true);
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();

		CheckboxItem includeTrainingData = ItemFactory.newCheckbox("includetrainingdata");

		ButtonItem export = new ButtonItem();
		export.setTitle(I18N.message("export"));
		export.addClickHandler(click -> {
			Util.download(Util.contextPath() + "ai/controller?command=export&modelId=" + modelId + "&trainingData="
					+ includeTrainingData.getValue());
			destroy();
		});

		DynamicForm form = new DynamicForm();
		form.setItems(includeTrainingData, export);
		addItem(form);
	}
}