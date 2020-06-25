package com.logicaldoc.gui.frontend.client.document.note;

import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIDocumentNote;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.ColorPickerItem;
import com.smartgwt.client.widgets.form.fields.RichTextItem;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * This is the form used to edit note or annotation
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.2
 */
public class AnnotationEditor extends Window {

	private GUIDocumentNote note;

	private RichTextItem message;

	private ColorPickerItem color;

	private SpinnerItem opacity;

	private DynamicForm noteForm = new DynamicForm();

	public AnnotationEditor(GUIDocumentNote note, boolean writeAllowed) {
		super();
		this.note = note;
		boolean writeEnabled = writeAllowed && (Session.get().getUser().isMemberOf("admin")
				|| note.getUserId() == Session.get().getUser().getId());
		
		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("annotationleftby", note.getUsername(), I18N.formatDate(note.getDate())));
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();
		setWidth(550);
		setHeight(280);

		ToolStrip toolStrip = new ToolStrip();
		toolStrip.setHeight(20);
		toolStrip.setWidth100();
		toolStrip.addSpacer(2);

		ToolStripButton save = new ToolStripButton();
		save.setTitle(I18N.message("save"));
		save.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				onSave();
			}
		});

		ToolStripButton close = new ToolStripButton();
		close.setTitle(I18N.message("close"));
		close.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				destroy();
			}
		});

		color = ItemFactory.newColorItemPicker("color", "color", note.getColor(), false, null);

		opacity = ItemFactory.newSpinnerItem("opacity", "opacity", note.getOpacity(), 1, 100);

		toolStrip.addFormItem(color);
		toolStrip.addFormItem(opacity);
		toolStrip.addSeparator();
		toolStrip.addButton(save);
		toolStrip.addButton(close);
		toolStrip.addFill();

		message = new RichTextItem("message");
		message.setTitle(I18N.message("message"));
		message.setShowTitle(false);
		message.setRequired(true);
		message.setWidth("*");
		message.setHeight(200);
		message.setValue(note.getMessage());

		noteForm.setItems(message);
		
		if(writeEnabled)
			addItem(toolStrip);
		addItem(noteForm);
	}

	void onSave() {
		if (!noteForm.validate())
			return;

		note.setMessage(noteForm.getValueAsString("message"));
		note.setColor(color.getValueAsString());
		note.setOpacity(Integer.parseInt(opacity.getValueAsString()));

		destroy();
	}
}