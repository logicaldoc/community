package com.logicaldoc.gui.frontend.client.docusign;

import java.util.List;

import com.logicaldoc.gui.common.client.beans.GUIDocumentNote;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.frontend.client.document.note.AnnotationContextMenu;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.types.VerticalAlignment;
import com.smartgwt.client.widgets.drawing.DrawItem;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.ComboBoxItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.HStack;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.MenuItemSeparator;

/**
 * The context menu used to edit an a DocuSign tab
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.6.1
 */
public class DocuSignTabContextMenu extends AnnotationContextMenu {

	public DocuSignTabContextMenu(DrawItem drawItem, GUIDocumentNote note) {
		super(drawItem, note);
	}

	@Override
	protected void appendAdditionalItems(List<MenuItem> items) {
		items.add(new MenuItemSeparator());
		items.add(prepareRecipientItem());
		items.add(new MenuItemSeparator());
		items.add(prepareTypeItem());
	}

	private MenuItem prepareRecipientItem() {
		final TextItem recipient = ItemFactory.newTextItem("recipient", "name", note.getRecipient());
		recipient.setRequired(true);
		recipient.setWidth(250);
		recipient.addChangedHandler(new ChangedHandler() {

			@Override
			public void onChanged(ChangedEvent event) {
				note.setRecipient((String) event.getValue());
			}
		});

		final ComboBoxItem recipientEmail = ItemFactory.newEmailComboSelector("recipientEmail", "recipientemail");
		recipientEmail.setRequired(true);
		recipientEmail.setWidth(250);
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

				note.setRecipient(fullName);
				note.setRecipientEmail((String) event.getValue());
			}
		});

		final DynamicForm form = new DynamicForm();
		form.setSnapTo("TR");
		form.setNumCols(2);
		form.setTitleOrientation(TitleOrientation.LEFT);
		form.setFields(recipient, recipientEmail);

		final HStack embedded = new HStack(3);
		embedded.setDefaultLayoutAlign(VerticalAlignment.CENTER);
		embedded.setSnapTo("TR");
		embedded.setHeight(60);
		embedded.setMembers(form);

		final MenuItem menuItem = new MenuItem(I18N.message("recipient"));
		menuItem.setShowRollOver(false);
		menuItem.setEmbeddedComponentFields("key");
		menuItem.setEmbeddedComponent(embedded);
		return menuItem;
	}

	private MenuItem prepareTypeItem() {
		SelectItem type = ItemFactory.newDocuSignTabType(note.getType());
		type.setRequired(true);
		type.addChangedHandler(new ChangedHandler() {

			@Override
			public void onChanged(ChangedEvent event) {
				note.setType((String) event.getValue());
			}
		});

		final DynamicForm form = new DynamicForm();
		form.setSnapTo("TR");
		form.setNumCols(4);
		form.setTitleOrientation(TitleOrientation.LEFT);
		form.setFields(type);

		final HStack embedded = new HStack(3);
		embedded.setDefaultLayoutAlign(VerticalAlignment.CENTER);
		embedded.setSnapTo("TR");
		embedded.setHeight100();
		embedded.setMembers(form);

		final MenuItem menuItem = new MenuItem(I18N.message("type"));
		menuItem.setShowRollOver(false);
		menuItem.setEmbeddedComponentFields("key");
		menuItem.setEmbeddedComponent(embedded);
		return menuItem;
	}
}
