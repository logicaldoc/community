package com.logicaldoc.gui.frontend.client.document;

import com.google.gwt.user.client.rpc.AsyncCallback;
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

	public StartWorkflowDialog(Long[] ids) {
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

		tag = ItemFactory.newTextItem("tag", null);
		tag.setWrapTitle(false);
		tag.setRequired(false);

		ButtonItem start = new ButtonItem();
		start.setTitle(I18N.message("startworkflow"));
		start.setAutoFit(true);
		start.addClickHandler(event -> onStart(ids));

		form.setTitleOrientation(TitleOrientation.TOP);
		form.setFields(workflow, tag, start);
		addItem(form);
	}

	public void onStart(Long[] ids) {
		if (!form.validate())
			return;

		ListGridRecord selection = workflow.getSelectedRecord();

		WorkflowService.Instance.get().startWorkflow(selection.getAttributeAsString("name"),
				selection.getAttributeAsString("description"), tag.getValueAsString(), ids, new AsyncCallback<Void>() {

					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(Void result) {
						GuiLog.info(I18N.message("event.workflow.start"), null);
						destroy();
					}
				});
	}
}