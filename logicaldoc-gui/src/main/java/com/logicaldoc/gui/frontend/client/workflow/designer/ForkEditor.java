package com.logicaldoc.gui.frontend.client.workflow.designer;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.ButtonItem;
import com.smartgwt.client.widgets.form.fields.TextItem;

/**
 * This is the form used for the workflow joins and forks.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.4
 */
public class ForkEditor extends Window {

	public ForkEditor(StateWidget widget) {
		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("editworkflowstate", I18N.message("task")) + " - " + widget.getTransition().getText());
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		setMargin(3);
		setAutoSize(true);
		setWidth(400);
		centerInPage();

		DynamicForm form = new DynamicForm();
		form.setTitleOrientation(TitleOrientation.TOP);
		form.setNumCols(1);

		TextItem name = ItemFactory.newTextItem("name", widget.getTransition().getText());
		name.setRequired(true);

		ButtonItem save = new ButtonItem("save", I18N.message("save"));
		save.setAutoFit(true);
		save.addClickHandler(click -> {
			if (form.validate()) {
				widget.getWFState().setName(form.getValueAsString("name"));
				widget.setContents("<b>" + form.getValueAsString("name") + "</b>");
				widget.getDrawingPanel().getDiagramController().update();
				destroy();
			}
		});

		save.setDisabled(!widget.getDrawingPanel().getWorkflowDesigner().getWorkflow().isLatestVersion());

		form.setItems(name, save);
		addItem(form);
	}
}