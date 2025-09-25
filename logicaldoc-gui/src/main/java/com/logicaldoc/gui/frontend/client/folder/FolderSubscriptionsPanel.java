package com.logicaldoc.gui.frontend.client.folder;

import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.data.SubscriptionsDS;
import com.logicaldoc.gui.common.client.grid.ColoredListGridField;
import com.logicaldoc.gui.common.client.grid.DateListGridField;
import com.logicaldoc.gui.common.client.grid.EventsListGridField;
import com.logicaldoc.gui.common.client.grid.UserListGridField;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.GridUtil;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.frontend.client.services.AuditService;
import com.logicaldoc.gui.frontend.client.subscription.SubscriptionDialog;
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
import com.smartgwt.client.widgets.menu.MenuItemSeparator;

/**
 * This panel shows the subscriptions on a folder.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.1.3
 */
public class FolderSubscriptionsPanel extends FolderDetailTab {

	private static final String USER_ID = "userId";

	private ListGrid list;

	private VLayout container = new VLayout();

	public FolderSubscriptionsPanel(final GUIFolder folder) {
		super(folder, null);
	}

	@Override
	protected void onDraw() {
		container.setMembersMargin(3);
		addMember(container);
		refresh(folder);
	}

	private void refreshList() {
		if (list != null)
			container.removeMember(list);

		ListGridField userId = new ColoredListGridField(USER_ID, USER_ID);
		userId.setWidth(50);
		userId.setCanEdit(false);
		userId.setHidden(true);

		ListGridField userName = new UserListGridField("userName", USER_ID, "user");
		userName.setCanEdit(false);

		ListGridField created = new DateListGridField("created", "subscription");

		ListGridField option = new FolderSubscriptionOptionListGridField();

		ListGridField events = new EventsListGridField();
		events.setWidth("*");

		list = new ListGrid();
		list.setEmptyMessage(I18N.message("notitemstoshow"));
		list.setCanFreezeFields(true);
		list.setSelectionType(SelectionStyle.MULTIPLE);
		list.setAutoFetchData(true);
		list.setDataSource(new SubscriptionsDS(folder.getId(), null));
		list.setFields(userId, userName, created, option, events);
		list.addCellContextClickHandler(event -> {
			showContextMenu();
			event.cancel();
		});

		container.addMember(list, 0);
	}

	void refresh(final GUIFolder folder) {
		super.folder = folder;

		container.removeMembers(container.getMembers());

		refreshList();

		HLayout buttons = new HLayout();
		buttons.setMembersMargin(4);
		buttons.setWidth100();
		buttons.setHeight(20);
		container.addMember(buttons);

		// Prepare the combo for adding a new Group
		final DynamicForm groupForm = new DynamicForm();
		final SelectItem group = ItemFactory.newGroupSelector("group", "addgroup");
		groupForm.setItems(group);
		buttons.addMember(groupForm);

		group.addChangedHandler(event -> {
			ListGridRecord selectedRecord = group.getSelectedRecord();
			if (selectedRecord == null)
				return;
			long groupId = Long.parseLong(selectedRecord.getAttributeAsString("id"));
			AuditService.Instance.get().subscribeFolder(folder.getId(), false, Constants.getAuditDefaultEvents(), null,
					groupId, new DefaultAsyncCallback<>() {
						@Override
						public void onSuccess(Void arg0) {
							refreshList();
						}
					});
		});

		final DynamicForm userForm = new DynamicForm();
		final SelectItem user = ItemFactory.newUserSelector("user", "adduser", null, true, false);
		userForm.setItems(user);

		user.addChangedHandler(event -> {
			ListGridRecord selectedRecord = user.getSelectedRecord();
			if (selectedRecord == null)
				return;
			long userId = Long.parseLong(selectedRecord.getAttributeAsString("id"));
			AuditService.Instance.get().subscribeFolder(folder.getId(), false, Constants.getAuditDefaultEvents(),
					userId, null, new DefaultAsyncCallback<>() {
						@Override
						public void onSuccess(Void arg0) {
							refreshList();
						}
					});
		});

		buttons.addMember(userForm);
	}

	private void showContextMenu() {
		Menu contextMenu = new Menu();

		final ListGridRecord[] selection = list.getSelectedRecords();
		if (selection == null || selection.length == 0)
			return;

		MenuItem delete = new MenuItem();
		delete.setTitle(I18N.message("ddelete"));
		delete.addClickHandler(event -> LD.ask(I18N.message("question"), I18N.message("confirmdelete"), answer -> {
			if (Boolean.TRUE.equals(answer)) {
				AuditService.Instance.get().deleteSubscriptions(GridUtil.getIds(selection), new DefaultAsyncCallback<>() {
					@Override
					public void onSuccess(Void result) {
						list.removeSelectedData();
						list.deselectAllRecords();
					}
				});
			}
		}));

		MenuItem edit = new MenuItem();
		edit.setTitle(I18N.message("edit"));
		edit.addClickHandler(click -> new SubscriptionDialog(list).show());

		contextMenu.setItems(edit, new MenuItemSeparator(), delete);
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