package com.logicaldoc.gui.frontend.client.docusign;

import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIDocumentNote;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.widgets.HeaderControl;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.ColorPickerItem;
import com.smartgwt.client.widgets.form.fields.ComboBoxItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.HLayout;

/**
 * This is the form used to edit a tab of the electronic signature
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.5
 */
public class ESignatureTabEditor extends Window {

	private GUIDocumentNote note;

	private ColorPickerItem color;

	private SpinnerItem opacity;

	private DynamicForm form = new DynamicForm();

	public ESignatureTabEditor(GUIDocumentNote note) {
		super();
		this.note = note;

		HeaderControl close = new HeaderControl(HeaderControl.CLOSE, new ClickHandler() {
			@Override
			public void onClick(ClickEvent e) {
				if (form.validate())
					destroy();
			}
		});

		setHeaderControls(HeaderControls.HEADER_LABEL, close);

		setTitle(I18N.message("tabcreatedby", note.getUsername(), I18N.formatDate(note.getDate())));
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();
		setAutoSize(true);
	}

	void onSave() {
		if (!form.validate())
			return;

		note.setType(form.getValueAsString("type"));
		note.setMessage(form.getValueAsString("label"));
		note.setColor(color.getValueAsString());
		note.setOpacity(Integer.parseInt(opacity.getValueAsString()));
		note.setRecipient(form.getValueAsString("recipient").trim());
		note.setRecipientEmail(form.getValueAsString("recipientEmail").trim());

		destroy();
	}

	@Override
	protected void onDraw() {
		boolean writeEnabled = Session.get().getUser().isMemberOf("admin")
				|| note.getUserId() == Session.get().getUser().getId();

		color = ItemFactory.newColorItemPicker("color", "color", note.getColor(), false, null);
		color.setRequired(true);

		opacity = ItemFactory.newSpinnerItem("opacity", "opacity", note.getOpacity(), 1, 100);
		opacity.setRequired(true);

		SelectItem type = ItemFactory.newDocuSignTabType(note.getType());
		type.setRequired(true);

		TextItem label = ItemFactory.newTextItem("label", "label", note.getMessage());
		label.setRequired(true);
		label.setWidth(300);

		final TextItem recipient = ItemFactory.newTextItem("recipient", "recipient", note.getRecipient());
		recipient.setRequired(true);
		recipient.setWidth(300);

		final ComboBoxItem recipientEmail = ItemFactory.newEmailComboSelector("recipientEmail", "recipientemail");
		recipientEmail.setRequired(true);
		recipientEmail.setWidth(300);
		recipientEmail.setValue(note.getRecipientEmail());
		recipientEmail.addChangedHandler(new ChangedHandler() {

			@Override
			public void onChanged(ChangedEvent event) {
				ListGridRecord selection = recipientEmail.getSelectedRecord();
				String firstName = selection.getAttributeAsString("firstName");
				String lastName = selection.getAttributeAsString("lastName");
				String fullName = (firstName != null ? firstName : "") + " " + (lastName != null ? lastName : "");
				if (!fullName.isEmpty())
					recipient.setValue(fullName);
			}
		});

		form.setItems(type, label, color, opacity, recipient, recipientEmail);

		IButton save = new IButton(I18N.message("save"));
		save.addClickHandler(new ClickHandler() {

			@Override
			public void onClick(ClickEvent event) {
				if (form.validate()) {
					onSave();
				}
			}
		});
		save.setDisabled(!writeEnabled);

		IButton close = new IButton(I18N.message("close"));
		close.addClickHandler(new ClickHandler() {

			@Override
			public void onClick(ClickEvent event) {
				if (form.validate()) {
					destroy();
				}
			}
		});

		HLayout buttons = new HLayout();
		buttons.setMembersMargin(5);
		buttons.setMargin(2);
		buttons.setMembers(save, close);

		addItem(form);
		addItem(save);
	}

	@Override
	public Boolean shouldDismissOnEscape() {
		return false;
	}
}