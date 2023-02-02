package com.logicaldoc.gui.frontend.client.docusign;

import com.google.gwt.user.client.rpc.AsyncCallback;
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
import com.smartgwt.client.widgets.form.fields.ComboBoxItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.HLayout;

/**
 * This is the dialog to create a new DocuSign tab
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.6.1
 */
public class DocuSignNewTabDialog extends Window {

	private DynamicForm form = new DynamicForm();

	private AsyncCallback<GUIDocumentNote> callback;

	public DocuSignNewTabDialog(AsyncCallback<GUIDocumentNote> callback) {
		super();
		this.callback = callback;

		HeaderControl close = new HeaderControl(HeaderControl.CLOSE, new ClickHandler() {
			@Override
			public void onClick(ClickEvent e) {
				if (form.validate())
					destroy();
			}
		});

		setHeaderControls(HeaderControls.HEADER_LABEL, close);

		setTitle(I18N.message("addtab"));
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();
		setAutoSize(true);
	}

	void onSave() {
		if (!form.validate())
			return;
		if (callback != null) {
			GUIDocumentNote note = new GUIDocumentNote();
			note.setType(form.getValueAsString("type"));
			note.setRecipient(form.getValueAsString("recipient"));
			note.setRecipientEmail(form.getValueAsString("recipientEmail"));
			callback.onSuccess(note);
		}

		destroy();
	}

	@Override
	protected void onDraw() {
		SelectItem type = ItemFactory.newDocuSignTabType("esig-signhere");
		type.setRequired(true);

		final TextItem recipient = ItemFactory.newTextItem("recipient", null);
		recipient.setRequired(true);
		recipient.setWidth(300);

		final ComboBoxItem recipientEmail = ItemFactory.newEmailComboSelector("recipientEmail", "recipientemail");
		recipientEmail.setRequired(true);
		recipientEmail.setWidth(300);
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

		form.setItems(type, recipient, recipientEmail);

		IButton save = new IButton(I18N.message("add"));
		save.addClickHandler(new ClickHandler() {

			@Override
			public void onClick(ClickEvent event) {
				if (form.validate()) {
					onSave();
				}
			}
		});

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