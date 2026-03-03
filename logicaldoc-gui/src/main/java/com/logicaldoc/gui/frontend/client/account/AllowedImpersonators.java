package com.logicaldoc.gui.frontend.client.account;

import java.util.ArrayList;
import java.util.List;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.data.ImpersonifiersDS;
import com.logicaldoc.gui.common.client.grid.EnabledListGridField;
import com.logicaldoc.gui.common.client.grid.IdListGridField;
import com.logicaldoc.gui.common.client.grid.RefreshableListGrid;
import com.logicaldoc.gui.common.client.grid.UserListGridField;
import com.logicaldoc.gui.common.client.grid.formatters.UserCellFormatter;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.services.SecurityService;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.types.SortDirection;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;

/**
 * This panel shows the list of the users that can impersonate the current user
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2.3
 */
public class AllowedImpersonators extends com.smartgwt.client.widgets.Window {

	private static final String GROUPS = "groups";

	private static final String GUEST = "guest";

	private static final String USERNAME = "username";

	private RefreshableListGrid list;

	public AllowedImpersonators() {
		super();

		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("allowedimpersonators"));
		setWidth(600);
		setHeight(300);
		setIsModal(true);
		setShowModalMask(true);
		setCanDragResize(true);
		setAutoSize(true);
		centerInPage();

		Label alert=new Label(I18N.message("impersonatorsalert"));
		alert.setWidth100();
		alert.setHeight(50);
		addItem(alert);
		
		prepareGrid();
		addItem(list);
		addButtons();
		list.fetchData();
	}

	private void prepareGrid() {
		ListGridField id = new IdListGridField();
		id.setCellFormatter(new UserCellFormatter());

		ListGridField username = new ListGridField(USERNAME, I18N.message(USERNAME), 100);
		username.setCanFilter(true);
		username.setCellFormatter(new UserCellFormatter());

		ListGridField name = new ListGridField("name", I18N.message("lastname"), 100);
		name.setCanFilter(true);
		name.setCellFormatter(new UserCellFormatter());

		ListGridField firstName = new ListGridField("firstName", I18N.message("firstname"), 100);
		firstName.setCanFilter(true);
		firstName.setCellFormatter(new UserCellFormatter());

		ListGridField enabled = new EnabledListGridField();

		ListGridField guest = new ListGridField(GUEST, I18N.message(GUEST), 55);
		guest.setCanFilter(true);
		guest.setHidden(true);

		ListGridField groups = new ListGridField(GROUPS, I18N.message(GROUPS), 200);
		groups.setCanFilter(true);
		groups.setCellFormatter(new UserCellFormatter());

		UserListGridField avatar = new UserListGridField(true);

		list = new RefreshableListGrid();
		list.setWidth100();
		list.setHeight100();
		list.setEmptyMessage(I18N.message("notitemstoshow"));
		list.setCanFreezeFields(true);
		list.setAutoFetchData(true);
		list.setSelectionType(SelectionStyle.MULTIPLE);
		list.setFilterOnKeypress(true);
		list.setCanEdit(false);
		list.setDataSource(new ImpersonifiersDS());
		list.setFields(id, enabled, avatar, username, firstName, name, groups, guest);
		list.sort(USERNAME, SortDirection.ASCENDING);

		list.addCellContextClickHandler(event -> {
			showContextMenu();
			event.cancel();
		});
	}

	private void showContextMenu() {
		Menu contextMenu = new Menu();

		final ListGridRecord[] selection = list.getSelectedRecords();
		if (selection == null || selection.length == 0)
			return;

		final List<String> usernames = new ArrayList<>();
		for (int i = 0; i < selection.length; i++)
			usernames.add(selection[i].getAttributeAsString(USERNAME));

		MenuItem delete = new MenuItem();
		delete.setTitle(I18N.message("ddelete"));
		delete.addClickHandler(event -> LD.ask(I18N.message("question"), I18N.message("confirmdelete"), answer -> {
			if (Boolean.TRUE.equals(answer)) {
				SecurityService.Instance.get().deleteImpersonifiers(usernames, new DefaultAsyncCallback<>() {
					@Override
					public void handleSuccess(Void result) {
						Session.get().getUser().getImpersonators().removeAll(usernames);
						list.removeSelectedData();
						list.deselectAllRecords();
					}
				});
			}
		}));

		contextMenu.setItems(delete);
		contextMenu.showContextMenu();
	}

	private void addButtons() {
		HLayout buttons = new HLayout();
		buttons.setMembersMargin(4);
		buttons.setWidth100();
		buttons.setHeight(20);

		final DynamicForm userForm = new DynamicForm();
		final SelectItem user = ItemFactory.newUserSelector("user", "adduser", null, true, false);
		userForm.setItems(user);

		user.addChangedHandler(changed -> {
			ListGridRecord selectedRecord = user.getSelectedRecord();
			if (selectedRecord == null)
				return;

			SecurityService.Instance.get().addImpersonifier(selectedRecord.getAttributeAsString(USERNAME),
					new DefaultAsyncCallback<>() {
						@Override
						public void handleSuccess(Void result) {
							list.refresh(new ImpersonifiersDS());
						}
					});
		});
		buttons.addMember(userForm);
		addItem(buttons);
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