package com.logicaldoc.gui.frontend.client.document.note;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.ButtonItem;
import com.smartgwt.client.widgets.form.fields.RichTextItem;
import com.smartgwt.client.widgets.form.fields.events.ClickEvent;
import com.smartgwt.client.widgets.form.fields.events.ClickHandler;

/**
 * This is the form used to edit note or annotation
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.2
 */
public class NoteUpdateDialog extends Window {

	private NotesPanel notesPanel;

	private ButtonItem save;

	private long noteId;

	private RichTextItem message;

	private long docId;

	private DynamicForm noteForm = new DynamicForm();

	public NoteUpdateDialog(final long docId, final long noteId, String noteMessage, final NotesPanel notesPanel) {
		super();
		this.notesPanel = notesPanel;
		this.noteId = noteId;
		this.docId = docId;

		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("note"));
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();
		setAutoSize(true);

		message = new RichTextItem("name");
		message.setTitle(I18N.message("message"));
		message.setShowTitle(false);
		message.setRequired(true);
		message.setWidth("*");
		message.setHeight("*");
		message.setValue(noteMessage);

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

		DocumentService.Instance.get().updateNote(docId, noteId, message.getValue().toString(),
				new AsyncCallback<Void>() {

					@Override
					public void onFailure(Throwable caught) {
						Log.serverError(caught);
						destroy();
					}

					@Override
					public void onSuccess(Void result) {
						notesPanel.refresh();
						destroy();
					}
				});
	}
}