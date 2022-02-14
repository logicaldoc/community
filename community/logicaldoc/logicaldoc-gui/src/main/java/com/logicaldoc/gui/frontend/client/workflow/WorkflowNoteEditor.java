package com.logicaldoc.gui.frontend.client.workflow;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.frontend.client.services.WorkflowService;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.ButtonItem;
import com.smartgwt.client.widgets.form.fields.RichTextItem;
import com.smartgwt.client.widgets.form.fields.events.ClickEvent;
import com.smartgwt.client.widgets.form.fields.events.ClickHandler;

/**
 * This is the form used to edit a note in a workflow
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.6
 */
public class WorkflowNoteEditor extends Window {

	private TaskDetailsDialog parentDialog;

	private ButtonItem save;

	private RichTextItem message;

	private DynamicForm noteForm = new DynamicForm();

	public WorkflowNoteEditor(TaskDetailsDialog parentDialog) {
		super();
		this.parentDialog = parentDialog;

		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("note"));
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();
		setAutoSize(true);

		message = ItemFactory.newRichTextItemForNote("message", "message", null);
		message.setWidth(680);
		message.setHeight(230);

		save = new ButtonItem();
		save.setTitle(I18N.message("save"));
		save.setAutoFit(true);
		save.addClickHandler(new ClickHandler() {
			public void onClick(ClickEvent event) {
				onSave();
			}
		});

		noteForm.setItems(message, save);
		addItem(noteForm);
	}

	private void onSave() {
		if (!noteForm.validate())
			return;
		WorkflowService.Instance.get().addNote(parentDialog.getWorkflow().getSelectedTask().getId(),
				message.getValue().toString(), new AsyncCallback<Long>() {
					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(Long noteId) {
						parentDialog.onNewNote(message.getValue().toString());
						destroy();
					}
				});
	}
}