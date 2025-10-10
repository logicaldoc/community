package com.logicaldoc.gui.frontend.client.document.note;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIDocumentNote;
import com.logicaldoc.gui.common.client.controllers.DocumentController;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.widgets.NoteChangedListener;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.widgets.HeaderControl;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.RichTextItem;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * This is the form used to edit note or annotation
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.2
 */
public class NoteUpdateDialog extends Window {

	private static final String MESSAGE = "message";

	private ToolStrip toolStrip;

	private DynamicForm messageForm;

	private NoteChangedListener changedListener;

	private RichTextItem messageBox;

	private GUIDocumentNote note;

	public NoteUpdateDialog(GUIDocumentNote note, final NoteChangedListener changedListener) {
		super();
		this.note = note;
		this.changedListener = changedListener;

		HeaderControl closeIcon = new HeaderControl(HeaderControl.CLOSE, event -> NoteUpdateDialog.this.destroy());

		setHeaderControls(HeaderControls.HEADER_LABEL, closeIcon);
		setTitle(I18N.message("note"));
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		setAutoSize(true);
		centerInPage();

		addResizedHandler(resized -> {
			if (getWidth() > 600)
				messageBox.setWidth(getWidth() - 10);
			else
				messageBox.setWidth(590);
		});
	}

	@Override
	protected void onDraw() {
		if (messageForm != null)
			removeItem(messageForm);

		if (toolStrip != null)
			removeItem(toolStrip);

		ToolStripButton save = new ToolStripButton(I18N.message("save"));
		save.addClickHandler(event -> onSave());
		ToolStripButton close = new ToolStripButton(I18N.message("close"));
		close.addClickHandler(event -> destroy());

		toolStrip = new ToolStrip();
		toolStrip.addButton(save);
		toolStrip.addSeparator();
		toolStrip.addButton(close);
		toolStrip.addFill();
		toolStrip.setWidth100();
		toolStrip.setMinWidth(590);

		messageBox = ItemFactory.newRichTextItemForNote(MESSAGE, MESSAGE, note.getMessage());
		messageBox.setBrowserSpellCheck(false);

		messageForm = new DynamicForm();
		messageForm.setWidth100();
		messageForm.setHeight100();
		messageForm.setItems(messageBox);

		addItem(toolStrip);
		addItem(messageForm);
	}

	private void onSave() {
		if (!messageForm.validate())
			return;

		note.setMessage(messageForm.getValueAsString(MESSAGE));

		DocumentService.Instance.get().saveNote(note, new DefaultAsyncCallback<>() {

			@Override
			public void onFailure(Throwable caught) {
				super.onFailure(caught);
				destroy();
			}

			@Override
			public void handleSuccess(GUIDocumentNote result) {
				if (changedListener != null)
					changedListener.onChanged(result);
				destroy();
				DocumentService.Instance.get().getById(result.getDocId(), new DefaultAsyncCallback<GUIDocument>() {
					@Override
					protected void handleSuccess(GUIDocument result) {
						DocumentController.get().modified(result);
					}
				});
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