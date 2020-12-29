package com.logicaldoc.gui.frontend.client.document.note;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.widgets.HeaderControl;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.events.ResizedEvent;
import com.smartgwt.client.widgets.events.ResizedHandler;
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

	private DynamicForm noteForm;

	public NoteUpdateDialog(final long docId, final long noteId, String noteMessage, final NotesPanel notesPanel) {
		super();
		this.notesPanel = notesPanel;
		this.noteId = noteId;
		this.docId = docId;

		HeaderControl maximize = new HeaderControl(HeaderControl.MAXIMIZE, new com.smartgwt.client.widgets.events.ClickHandler() {
			public void onClick(com.smartgwt.client.widgets.events.ClickEvent event) {
				maximize();
			}
		});

		HeaderControl minimize = new HeaderControl(HeaderControl.MINIMIZE, new com.smartgwt.client.widgets.events.ClickHandler() {
			public void onClick(com.smartgwt.client.widgets.events.ClickEvent event) {
				resetDimensions();
			}
		});

		HeaderControl closeIcon = new HeaderControl(HeaderControl.CLOSE, new com.smartgwt.client.widgets.events.ClickHandler() {
			public void onClick(com.smartgwt.client.widgets.events.ClickEvent event) {
				NoteUpdateDialog.this.destroy();
			}
		});
		
		setHeaderControls(HeaderControls.HEADER_LABEL, maximize, minimize, closeIcon);
		setTitle(I18N.message("note"));
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		resetDimensions();

		initGUI(noteMessage);
		
		addResizedHandler(new ResizedHandler() {

			@Override
			public void onResized(ResizedEvent event) {
				initGUI(message != null ? (String) message.getValue() : null);
			}
		});
	}

	private void initGUI(String noteMessage) {
		if (noteForm != null)
			removeItem(noteForm);

		message = ItemFactory.newRichTextItemForNote("message", "message", noteMessage);
		message.setHeight("*");

		save = new ButtonItem();
		save.setTitle(I18N.message("save"));
		save.setAutoFit(true);
		save.addClickHandler(new ClickHandler() {
			public void onClick(ClickEvent event) {
				onSave();
			}
		});

		noteForm = new DynamicForm();
		noteForm.setWidth100();
		noteForm.setHeight100();
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
	
	private void resetDimensions() {
		setWidth(550);
		setHeight(300);
		centerInPage();
	}
}