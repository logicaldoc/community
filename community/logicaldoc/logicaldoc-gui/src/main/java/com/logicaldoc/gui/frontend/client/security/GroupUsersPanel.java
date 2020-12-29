package com.logicaldoc.gui.frontend.client.security;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.data.UsersDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.common.client.services.SecurityService;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.widgets.InfoPanel;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.ListGridFieldType;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.CellContextClickEvent;
import com.smartgwt.client.widgets.grid.events.CellContextClickHandler;
import com.smartgwt.client.widgets.grid.events.DataArrivedEvent;
import com.smartgwt.client.widgets.grid.events.DataArrivedHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.events.MenuItemClickEvent;

/**
 * This panel shows the list of users in a group.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class GroupUsersPanel extends VLayout {
	private ListGrid list;

	private long groupId;

	public GroupUsersPanel(final long groupId) {
		this.groupId = groupId;
		setWidth100();
		setHeight100();
	}

	@Override
	public void onDraw() {
		final InfoPanel infoPanel = new InfoPanel("");

		ListGridField id = new ListGridField("id", 50);
		id.setHidden(true);

		ListGridField username = new ListGridField("username", I18N.message("username"), 100);
		username.setCanFilter(true);

		ListGridField name = new ListGridField("name", I18N.message("lastname"), 100);
		name.setCanFilter(true);

		ListGridField firstName = new ListGridField("firstName", I18N.message("firstname"), 100);
		firstName.setCanFilter(true);

		ListGridField phone = new ListGridField("phone", I18N.message("phone"), 90);
		phone.setCanFilter(true);

		ListGridField cell = new ListGridField("cell", I18N.message("cell"), 90);
		cell.setCanFilter(true);

		ListGridField email = new ListGridField("email", I18N.message("email"), 200);
		email.setCanFilter(true);

		ListGridField eenabled = new ListGridField("eenabled", " ", 24);
		eenabled.setType(ListGridFieldType.IMAGE);
		eenabled.setCanSort(false);
		eenabled.setAlign(Alignment.CENTER);
		eenabled.setShowDefaultContextMenu(false);
		eenabled.setImageURLPrefix(Util.imagePrefix());
		eenabled.setImageURLSuffix(".gif");
		eenabled.setCanFilter(false);

		ListGridField enabled = new ListGridField("_enabled", I18N.message("enabled"), 55);
		enabled.setCanFilter(true);
		enabled.setHidden(true);

		ListGridField guest = new ListGridField("guest", I18N.message("guest"), 55);
		guest.setCanFilter(true);
		guest.setHidden(true);

		list = new ListGrid();
		list.setEmptyMessage(I18N.message("notitemstoshow"));
		list.setShowRecordComponents(true);
		list.setShowRecordComponentsByCell(true);
		list.setCanFreezeFields(true);
		list.setAutoFetchData(true);
		list.setSelectionType(SelectionStyle.MULTIPLE);
		list.setFilterOnKeypress(true);
		list.setShowFilterEditor(true);
		list.setDataSource(UsersDS.get(groupId));
		list.setFields(id, eenabled, username, firstName, name, email, cell, phone, enabled, guest);

		HLayout buttons = new HLayout();
		buttons.setHeight(25);
		buttons.setMargin(3);

		// Prepare the list for adding a new user
		final DynamicForm userForm = new DynamicForm();
		final SelectItem user = ItemFactory.newUserSelector("user", "adduser", null, false, false);
		user.addChangedHandler(new ChangedHandler() {
			@Override
			public void onChanged(ChangedEvent event) {
				final ListGridRecord selectedRecord = user.getSelectedRecord();
				if (selectedRecord == null)
					return;

				// Check if the selected user is already present in the members
				ListGridRecord[] records = list.getRecords();
				for (ListGridRecord test : records) {
					if (test.getAttribute("id").equals(selectedRecord.getAttribute("id"))) {
						return;
					}
				}

				SecurityService.Instance.get().addUserToGroup(groupId,
						Long.parseLong(selectedRecord.getAttribute("id")), new AsyncCallback<Void>() {

							@Override
							public void onFailure(Throwable caught) {
								Log.serverError(caught);
							}

							@Override
							public void onSuccess(Void ret) {
								// Update the users table
								ListGridRecord record = new ListGridRecord();
								record.setAttribute("id", selectedRecord.getAttribute("id"));
								record.setAttribute("username", selectedRecord.getAttribute("username"));
								record.setAttribute("name", selectedRecord.getAttribute("name"));
								record.setAttribute("firstName", selectedRecord.getAttribute("firstName"));
								record.setAttribute("email", selectedRecord.getAttribute("email"));
								record.setAttribute("phone", selectedRecord.getAttribute("phone"));
								record.setAttribute("cell", selectedRecord.getAttribute("cell"));
								record.setAttribute("eenabled", selectedRecord.getAttribute("eenabled"));
								list.addData(record);
								user.clearValue();
							}
						});
			}

		});

		userForm.setItems(user);
		buttons.addMember(userForm);

		setMembers(infoPanel, list, buttons);

		list.addDataArrivedHandler(new DataArrivedHandler() {
			@Override
			public void onDataArrived(DataArrivedEvent event) {
				infoPanel.setMessage(I18N.message("showusers", Integer.toString(list.getTotalRows())));
			}
		});

		list.addCellContextClickHandler(new CellContextClickHandler() {
			@Override
			public void onCellContextClick(CellContextClickEvent event) {
				showContextMenu();
				event.cancel();
			}
		});
	}

	private void showContextMenu() {
		Menu contextMenu = new Menu();

		final ListGridRecord[] selection = list.getSelectedRecords();

		MenuItem remove = new MenuItem();
		remove.setTitle(I18N.message("removefromgroup"));
		remove.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				if (selection == null || selection.length == 0)
					return;
				final long[] ids = new long[selection.length];
				for (int i = 0; i < selection.length; i++) {
					ids[i] = Long.parseLong(selection[i].getAttribute("id"));
				}

				LD.ask(I18N.message("question"), I18N.message("confirmdelete"), new BooleanCallback() {
					@Override
					public void execute(Boolean value) {
						if (value) {
							SecurityService.Instance.get().removeFromGroup(groupId, ids, new AsyncCallback<Void>() {
								@Override
								public void onFailure(Throwable caught) {
									Log.serverError(caught);
								}

								@Override
								public void onSuccess(Void result) {
									list.removeSelectedData();
									list.deselectAllRecords();
								}
							});
						}
					}
				});
			}
		});

		if (selection == null || selection.length < 1)
			remove.setEnabled(false);

		contextMenu.setItems(remove);
		contextMenu.showContextMenu();
	}
}