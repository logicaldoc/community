package com.logicaldoc.gui.frontend.client.workflow.designer;

import java.util.Map;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.ButtonItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ClickEvent;
import com.smartgwt.client.widgets.form.fields.events.ClickHandler;

/**
 * This is the form used for the workflow joins and forks.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.4
 */
public class StatusDialog extends Window {

	private ValuesManager vm = new ValuesManager();

	private DynamicForm form;

	private StateWidget widget;

	public StatusDialog(StateWidget widget) {
		this.widget = widget;

		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("editworkflowstate", I18N.message("task")));
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		setMargin(3);
		setAutoSize(true);
		setWidth(400);
		centerInPage();

		form = new DynamicForm();
		form.setTitleOrientation(TitleOrientation.TOP);
		form.setNumCols(1);
		form.setValuesManager(vm);

		TextItem name = ItemFactory.newTextItem("name", widget.getTransition().getText());
		name.setRequired(true);

		ButtonItem save = new ButtonItem("save", I18N.message("save"));
		save.setAutoFit(true);
		save.addClickHandler(new ClickHandler() {
			@SuppressWarnings("unchecked")
			public void onClick(ClickEvent event) {
				Map<String, Object> values = (Map<String, Object>) vm.getValues();

				if (vm.validate()) {
					if (vm.validate()) {
						StatusDialog.this.widget.getWfState().setName((String) values.get("name"));
						StatusDialog.this.widget.setContents("<b>" + (String) values.get("name") + "</b>");
						StatusDialog.this.widget.getDrawingPanel().getDiagramController().update();

						destroy();
					}
				}
			}
		});
		
		save.setDisabled(!widget.getDrawingPanel().getWorkflowDesigner().getWorkflow().isLatestVersion());

		form.setItems(name, save);
		addItem(form);
	}
}