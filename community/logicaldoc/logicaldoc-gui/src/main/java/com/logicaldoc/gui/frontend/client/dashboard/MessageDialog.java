package com.logicaldoc.gui.frontend.client.dashboard;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIMessage;
import com.logicaldoc.gui.common.client.beans.GUIUser;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.EventPanel;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.common.client.services.SecurityService;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.frontend.client.services.MessageService;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.ListGridEditEvent;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.events.CloseClickEvent;
import com.smartgwt.client.widgets.events.CloseClickHandler;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.CheckboxItem;
import com.smartgwt.client.widgets.form.fields.IntegerItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.form.validator.IntegerRangeValidator;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.CellContextClickEvent;
import com.smartgwt.client.widgets.grid.events.CellContextClickHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.SectionStack;
import com.smartgwt.client.widgets.layout.SectionStackSection;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.events.MenuItemClickEvent;

/**
 * This is the form used to send messages to other users
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 6.0
 */
public class MessageDialog extends Window {

	private DynamicForm form = new DynamicForm();

	private ListGrid recipientsGrid;

	public MessageDialog() {
		super();

		addCloseClickHandler(new CloseClickHandler() {
			@Override
			public void onCloseClick(CloseClickEvent event) {
				destroy();
			}
		});

		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("sendmessage"));
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		setWidth(450);
		setHeight(510);
		centerInPage();

		form.setMargin(3);
		form.setTitleOrientation(TitleOrientation.TOP);
		form.setNumCols(1);

		TextItem subject = ItemFactory.newTextItem("subject", "subject", "");
		subject.setRequired(true);
		subject.setWidth(250);

		final TextAreaItem message = ItemFactory.newTextAreaItem("message", "message", null);
		message.setHeight(60);
		message.setWidth(400);

		final CheckboxItem confirmation = new CheckboxItem();
		confirmation.setName("confirmation");
		confirmation.setTitle(I18N.message("confirmation"));

		IntegerItem validity = ItemFactory.newIntegerItem("validity", I18N.message("validity"), 1);
		IntegerRangeValidator integerRangeValidator = new IntegerRangeValidator();
		integerRangeValidator.setMin(1);
		validity.setValidators(integerRangeValidator);
		validity.setHint(I18N.message("days"));

		SelectItem priority = ItemFactory.newPrioritySelector("priority", I18N.message("priority"));

		final IButton send = new IButton();
		send.setTitle(I18N.message("send"));
		send.setAutoFit(true);
		send.setMargin(3);
		send.setHeight(30);
		send.addClickHandler(new ClickHandler() {
			public void onClick(ClickEvent event) {
				form.validate();
				if (!form.hasErrors()) {
					GUIMessage message = new GUIMessage();
					message.setSubject(form.getValueAsString("subject"));
					message.setMessage(form.getValueAsString("message"));
					message.setConfirmation("true".equals(form.getValueAsString("confirmation")));
					if (form.getValueAsString("validity") != null)
						message.setValidity(Integer.parseInt(form.getValueAsString("validity")));
					message.setPriority(Integer.parseInt(form.getValue("priority").toString()));

					if (recipientsGrid.getRecords() == null || recipientsGrid.getRecords().length < 1) {
						SC.warn(I18N.message("noselectedrecipients"));
						return;
					}

					long[] ids = new long[recipientsGrid.getRecords().length];
					for (int i = 0; i < ids.length; i++)
						ids[i] = recipientsGrid.getRecords()[i].getAttributeAsLong("id");

					MessageService.Instance.get().save(message, ids, new AsyncCallback<Void>() {

						@Override
						public void onFailure(Throwable caught) {
							Log.serverError(caught);
							destroy();
						}

						@Override
						public void onSuccess(Void result) {
							EventPanel.get().info(I18N.message("messagesent"), null);
							destroy();
						}
					});
				}
			}
		});
		form.setFields(subject, confirmation, validity, priority, message);

		HLayout buttons = new HLayout();
		buttons.setMembers(send);

		SectionStack recipientsStack = prepareRecipientsGrid();

		addItem(recipientsStack);
		addItem(form);
		addItem(buttons);
	}

	private SectionStack prepareRecipientsGrid() {
		SectionStack sectionStack = new SectionStack();
		sectionStack.setWidth100();
		sectionStack.setHeight(200);
		sectionStack.setMargin(6);

		SectionStackSection section = new SectionStackSection("<b>" + I18N.message("recipients") + "</b>");
		section.setCanCollapse(false);
		section.setExpanded(true);

		ListGridField name = new ListGridField("label", I18N.message("name"));
		name.setCanFilter(true);

		ListGridField id = new ListGridField("id", I18N.message(" "));
		id.setWidth(50);
		id.setHidden(true);

		recipientsGrid = new ListGrid();
		recipientsGrid.setShowRecordComponents(true);
		recipientsGrid.setShowRecordComponentsByCell(true);
		recipientsGrid.setAutoFetchData(true);
		recipientsGrid.setCanEdit(false);
		recipientsGrid.setShowHeader(false);
		recipientsGrid.setSelectionType(SelectionStyle.MULTIPLE);
		recipientsGrid.setWidth100();
		recipientsGrid.setEditEvent(ListGridEditEvent.CLICK);
		recipientsGrid.setFields(id, name);

		recipientsGrid.addCellContextClickHandler(new CellContextClickHandler() {
			@Override
			public void onCellContextClick(CellContextClickEvent event) {
				MenuItem delete = new MenuItem();
				delete.setTitle(I18N.message("ddelete"));
				delete.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
					public void onClick(MenuItemClickEvent event) {
						recipientsGrid.removeSelectedData();
					}
				});

				Menu contextMenu = new Menu();
				contextMenu.setItems(delete);
				contextMenu.showContextMenu();

				event.cancel();
			}
		});

		final SelectItem userSelector = ItemFactory.newUserSelector("users", "users", null, true);
		userSelector.setWidth(150);
		userSelector.setMultiple(true);
		userSelector.addChangedHandler(new ChangedHandler() {

			@Override
			public void onChanged(ChangedEvent event) {
				addRecipients(userSelector.getSelectedRecords());
				userSelector.clearValue();
			}
		});

		final SelectItem groupSelector = ItemFactory.newGroupSelector("groups", "groups");
		groupSelector.setWidth(150);
		groupSelector.setMultiple(false);
		groupSelector.addChangedHandler(new ChangedHandler() {

			@Override
			public void onChanged(ChangedEvent event) {
				String groupId = groupSelector.getSelectedRecord().getAttributeAsString("id");
				SecurityService.Instance.get().searchUsers(null, groupId, new AsyncCallback<GUIUser[]>() {
					@Override
					public void onFailure(Throwable caught) {
						Log.serverError(caught);
					}

					@Override
					public void onSuccess(GUIUser[] users) {
						if (users == null || users.length < 1)
							return;

						ListGridRecord[] records = new ListGridRecord[users.length];
						for (int i = 0; i < users.length; i++) {
							records[i] = new ListGridRecord();
							records[i].setAttribute("id", users[i].getId());
							records[i].setAttribute("label", users[i].getFullName());
						}

						addRecipients(records);
						groupSelector.clearValue();
					}
				});
			}
		});

		DynamicForm selectionForm = new DynamicForm();
		selectionForm.setNumCols(4);
		selectionForm.setItems(userSelector, groupSelector);

		section.setItems(recipientsGrid, selectionForm);
		sectionStack.setSections(section);

		return sectionStack;
	}

	/**
	 * Add new users in the recipients grid
	 */
	private void addRecipients(ListGridRecord[] newSelection) {
		if (newSelection == null || newSelection.length < 1)
			return;

		for (int i = 0; i < newSelection.length; i++) {
			ListGridRecord newRec = new ListGridRecord();
			newRec.setAttribute("id", newSelection[i].getAttributeAsString("id"));
			newRec.setAttribute("label", newSelection[i].getAttributeAsString("label"));

			// Iterate over the current recipients avoiding duplicates
			boolean duplicate = false;
			ListGridRecord[] currentRecipients = recipientsGrid.getRecords();
			for (int j = 0; j < currentRecipients.length; j++) {
				ListGridRecord rec = currentRecipients[j];
				if (rec.getAttributeAsString("id").equals(newRec.getAttributeAsString("id"))) {
					duplicate = true;
					break;
				}
			}

			if (!duplicate)
				recipientsGrid.addData(newRec);
		}
	}
}
