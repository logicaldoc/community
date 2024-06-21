package com.logicaldoc.gui.frontend.client.document.note;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.widgets.HeaderControl;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.RichTextItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * This is the form used to edit note or annotation
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.2
 */
public class NoteUpdateDialog extends Window {

	private ToolStrip toolStrip;

	private long noteId;

	private String fileVersion;

	private RichTextItem message;

	private long docId;

	private DynamicForm noteForm;

	private ChangedHandler saveHandler;

	public NoteUpdateDialog(final long docId, final long noteId, String fileVersion, String noteMessage,
			final ChangedHandler saveHandler) {
		super();
		this.saveHandler = saveHandler;
		this.noteId = noteId;
		this.docId = docId;
		this.fileVersion = fileVersion;

		HeaderControl maximize = new HeaderControl(HeaderControl.MAXIMIZE, event -> maximize());

		HeaderControl minimize = new HeaderControl(HeaderControl.MINIMIZE, event -> resetDimensions());

		HeaderControl closeIcon = new HeaderControl(HeaderControl.CLOSE, event -> NoteUpdateDialog.this.destroy());

		setHeaderControls(HeaderControls.HEADER_LABEL, maximize, minimize, closeIcon);
		setTitle(I18N.message("note"));
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		resetDimensions();

		initGUI(noteMessage);
	}

	private void initGUI(String noteMessage) {
		if (noteForm != null)
			removeItem(noteForm);

		if (toolStrip != null)
			removeItem(toolStrip);

		ToolStripButton save = new ToolStripButton(I18N.message("save"));
		save.addClickHandler(event -> onSave());
		ToolStripButton close = new ToolStripButton(I18N.message("close"));
		close.addClickHandler(event -> destroy());

		toolStrip = new ToolStrip();
		toolStrip.setWidth100();
		toolStrip.addButton(save);
		toolStrip.addSeparator();
		toolStrip.addButton(close);

		message = ItemFactory.newRichTextItemForNote("message", "message", noteMessage);
		message.setBrowserSpellCheck(false);

		noteForm = new DynamicForm();
		noteForm.setWidth100();
		noteForm.setHeight100();
		noteForm.setItems(message);

		addItem(toolStrip);
		addItem(noteForm);
	}

	private void onSave() {
		if (!noteForm.validate())
			return;

		DocumentService.Instance.get().updateNote(docId, noteId, fileVersion, message.getValue().toString(), new AsyncCallback<>() {

			@Override
			public void onFailure(Throwable caught) {
				GuiLog.serverError(caught);
				destroy();
			}

			@Override
			public void onSuccess(Void result) {
				if (saveHandler != null)
					saveHandler.onChanged(null);
				destroy();
			}
		});
	}

	private void resetDimensions() {
		setWidth(580);
		setHeight(300);
		centerInPage();
	}
}