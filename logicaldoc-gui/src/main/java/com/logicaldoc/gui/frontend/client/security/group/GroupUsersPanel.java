package com.logicaldoc.gui.frontend.client.security.group;

import java.util.ArrayList;
import java.util.List;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.data.UsersDS;
import com.logicaldoc.gui.common.client.grid.IdListGridField;
import com.logicaldoc.gui.common.client.grid.UserListGridField;
import com.logicaldoc.gui.common.client.grid.formatters.UserCellFormatter;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.services.SecurityService;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.widgets.InfoPanel;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.ListGridFieldType;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;

/**
 * This panel shows the list of users in a group.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class GroupUsersPanel extends VLayout {
	private static final String EMAIL = "email";

	private static final String EENABLED = "eenabled";

	private static final String PHONE = "phone";

	private static final String FIRST_NAME = "firstName";

	private static final String USERNAME = "username";

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

		UserListGridField avatar = new UserListGridField(true);

		ListGridField id = new IdListGridField();

		ListGridField username = new ListGridField(USERNAME, I18N.message(USERNAME), 100);
		username.setCanFilter(true);
		username.setCellFormatter(new UserCellFormatter());

		ListGridField name = new ListGridField("name", I18N.message("lastname"), 100);
		name.setCanFilter(true);
		name.setCellFormatter(new UserCellFormatter());

		ListGridField firstName = new ListGridField(FIRST_NAME, I18N.message("firstname"), 100);
		firstName.setCanFilter(true);
		firstName.setCellFormatter(new UserCellFormatter());

		ListGridField phone = new ListGridField(PHONE, I18N.message(PHONE), 90);
		phone.setCanFilter(true);
		phone.setCellFormatter(new UserCellFormatter());

		ListGridField cell = new ListGridField("cell", I18N.message("cell"), 90);
		cell.setCanFilter(true);
		cell.setCellFormatter(new UserCellFormatter());

		ListGridField email = new ListGridField(EMAIL, I18N.message(EMAIL), 200);
		email.setCanFilter(true);
		email.setCellFormatter(new UserCellFormatter());

		ListGridField enabledIcon = new ListGridField("enabledIcon", " ", 24);
		enabledIcon.setType(ListGridFieldType.IMAGE);
		enabledIcon.setCanSort(false);
		enabledIcon.setAlign(Alignment.CENTER);
		enabledIcon.setShowDefaultContextMenu(false);
		enabledIcon.setImageURLPrefix(Util.imagePrefix());
		enabledIcon.setImageURLSuffix(".gif");
		enabledIcon.setCanFilter(false);

		ListGridField enabled = new ListGridField(EENABLED, I18N.message("enabled"), 55);
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
		list.setFields(id, enabledIcon, avatar, username, firstName, name, email, cell, phone, enabled, guest, enabled);

		HLayout buttons = new HLayout();
		buttons.setHeight(25);
		buttons.setMargin(3);

		// Prepare the list for adding a new user
		final DynamicForm userForm = new DynamicForm();
		final SelectItem user = ItemFactory.newUserSelector("user", "adduser", null, false, false);
		user.addChangedHandler(event -> {
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

			SecurityService.Instance.get().addUserToGroup(groupId, Long.parseLong(selectedRecord.getAttribute("id")),
					new DefaultAsyncCallback<>() {
						@Override
						public void handleSuccess(Void ret) {
							// Update the users table
							ListGridRecord rec = new ListGridRecord();
							rec.setAttribute("id", selectedRecord.getAttribute("id"));
							rec.setAttribute(USERNAME, selectedRecord.getAttribute(USERNAME));
							rec.setAttribute("name", selectedRecord.getAttribute("name"));
							rec.setAttribute(FIRST_NAME, selectedRecord.getAttribute(FIRST_NAME));
							rec.setAttribute(EMAIL, selectedRecord.getAttribute(EMAIL));
							rec.setAttribute(PHONE, selectedRecord.getAttribute(PHONE));
							rec.setAttribute("cell", selectedRecord.getAttribute("cell"));
							rec.setAttribute(EENABLED, selectedRecord.getAttribute(EENABLED));
							list.addData(rec);
							user.clearValue();
						}
					});
		});

		userForm.setItems(user);
		buttons.addMember(userForm);

		setMembers(infoPanel, list, buttons);

		list.addDataArrivedHandler(
				event -> infoPanel.setMessage(I18N.message("showusers", Integer.toString(list.getTotalRows()))));

		list.addCellContextClickHandler(event -> {
			showContextMenu();
			event.cancel();
		});
	}

	private void showContextMenu() {
		Menu contextMenu = new Menu();

		final ListGridRecord[] selection = list.getSelectedRecords();

		MenuItem remove = new MenuItem();
		remove.setTitle(I18N.message("removefromgroup"));
		remove.addClickHandler(event -> {
			if (selection == null || selection.length == 0)
				return;
			final List<Long> ids = new ArrayList<>();
			for (int i = 0; i < selection.length; i++) {
				ids.add(Long.parseLong(selection[i].getAttribute("id")));
			}

			LD.ask(I18N.message("question"), I18N.message("confirmdelete"), confirm -> {
				if (Boolean.TRUE.equals(confirm)) {
					SecurityService.Instance.get().removeFromGroup(groupId, ids, new DefaultAsyncCallback<>() {
						@Override
						public void handleSuccess(Void result) {
							list.removeSelectedData();
							list.deselectAllRecords();
						}
					});
				}
			});
		});

		if (selection == null || selection.length < 1)
			remove.setEnabled(false);

		contextMenu.setItems(remove);
		contextMenu.showContextMenu();
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