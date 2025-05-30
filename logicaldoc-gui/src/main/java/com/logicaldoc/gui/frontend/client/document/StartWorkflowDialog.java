package com.logicaldoc.gui.frontend.client.document;

import java.util.List;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.frontend.client.services.WorkflowService;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.ButtonItem;
import com.smartgwt.client.widgets.form.fields.ColorPickerItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.grid.ListGridRecord;

/**
 * This popup window is used to start a workflow on the selected documents.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.0
 */
public class StartWorkflowDialog extends Window {

	private DynamicForm form = new DynamicForm();

	private SelectItem workflow;

	private TextItem tag;

	private ColorPickerItem color;

	public StartWorkflowDialog(List<Long> ids) {
		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("startworkflow"));
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		setAutoSize(true);
		centerInPage();

		workflow = ItemFactory.newWorkflowSelector(Session.get().getUser().getId());
		workflow.setTitle(I18N.message("chooseworkflow"));
		workflow.setWrapTitle(false);
		workflow.setRequired(true);
		workflow.addChangedHandler(event -> color.setValue(workflow.getSelectedRecord().getAttributeAsString("color")));

		tag = ItemFactory.newTextItem("tag", null);
		tag.setWrapTitle(false);
		tag.setRequired(false);

		color = ItemFactory.newColorPickerItem(null, true, null);
		color.setWrapTitle(false);
		color.setRequired(false);

		ButtonItem start = new ButtonItem();
		start.setTitle(I18N.message("startworkflow"));
		start.setAutoFit(true);
		start.addClickHandler(event -> onStart(ids));

		form.setNumCols(3);
		form.setTitleOrientation(TitleOrientation.TOP);
		form.setFields(workflow, tag, color, start);
		addItem(form);
	}

	public void onStart(List<Long> ids) {
		if (!form.validate())
			return;

		ListGridRecord selection = workflow.getSelectedRecord();

		WorkflowService.Instance.get().startWorkflow(selection.getAttributeAsString("name"),
				selection.getAttributeAsString("description"), tag.getValueAsString(), color.getValueAsString(), ids,
				new DefaultAsyncCallback<>() {
					@Override
					public void onSuccess(String result) {
						GuiLog.info(I18N.message("event.workflow.start"), null);
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