package com.logicaldoc.gui.frontend.client.workflow;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.frontend.client.services.WorkflowService;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.ButtonItem;
import com.smartgwt.client.widgets.form.fields.RichTextItem;

/**
 * This is the form used to add a note in a workflow
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
		save.addClickHandler(event -> onSave());

		noteForm.setItems(message, save);
		addItem(noteForm);
	}

	private void onSave() {
		if (!noteForm.validate())
			return;
		WorkflowService.Instance.get().addNote(parentDialog.getWorkflow().getSelectedTask().getId(), null,
				message.getValue().toString(), new DefaultAsyncCallback<>() {
					@Override
					public void onSuccess(Long noteId) {
						parentDialog.onNewNote();
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