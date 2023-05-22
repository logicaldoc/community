package com.logicaldoc.gui.frontend.client.document;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.data.SubscriptionsDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.widgets.grid.DateListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.EventsListGridField;
import com.logicaldoc.gui.common.client.widgets.grid.UserListGridField;
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

/**
 * This panel shows the subscriptions on a document.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.1.3
 */
public class DocumentSubscriptionsPanel extends DocumentDetailTab {

	private static final String USER_ID = "userId";

	private ListGrid list;

	private VLayout container = new VLayout();

	public DocumentSubscriptionsPanel(final GUIDocument document) {
		super(document, null);
	}

	@Override
	protected void onDraw() {
		addMember(container);
		refresh();
	}

	private void refreshList() {
		if (list != null)
			container.removeMember(list);

		ListGridField userId = new ListGridField(USER_ID, USER_ID, 50);
		userId.setCanEdit(false);
		userId.setHidden(true);

		ListGridField userName = new UserListGridField("userName", USER_ID, "user");
		userName.setCanEdit(false);

		DateListGridField created = new DateListGridField("created", "subscription");

		EventsListGridField events = new EventsListGridField("events", I18N.message("notifyon"));

		list = new ListGrid();
		list.setEmptyMessage(I18N.message("notitemstoshow"));
		list.setCanFreezeFields(true);
		list.setSelectionType(SelectionStyle.MULTIPLE);
		list.setAutoFetchData(true);
		list.setDataSource(new SubscriptionsDS(null, document.getId()));
		list.setFields(userId, userName, created, events);
		list.addCellContextClickHandler(event -> {
			showContextMenu();
			event.cancel();
		});

		container.addMember(list, 0);
	}

	void refresh() {
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
			AuditService.Instance.get().subscribeDocuments(new long[] { document.getId() },
					Constants.AUDIT_DEFAULT_EVENTS, null, groupId, new AsyncCallback<Void>() {

						@Override
						public void onFailure(Throwable caught) {
							GuiLog.serverError(caught);
						}

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
			AuditService.Instance.get().subscribeDocuments(new long[] { document.getId() },
					Constants.AUDIT_DEFAULT_EVENTS, userId, null, new AsyncCallback<Void>() {

						@Override
						public void onFailure(Throwable caught) {
							GuiLog.serverError(caught);
						}

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
		final long[] ids = new long[selection.length];
		for (int i = 0; i < selection.length; i++) {
			ids[i] = Long.parseLong(selection[i].getAttribute("id"));
		}

		MenuItem delete = new MenuItem();
		delete.setTitle(I18N.message("ddelete"));
		delete.addClickHandler(event -> {
			LD.ask(I18N.message("question"), I18N.message("confirmdelete"), (Boolean value) -> {
				if (Boolean.TRUE.equals(value)) {
					AuditService.Instance.get().deleteSubscriptions(ids, new AsyncCallback<Void>() {
						@Override
						public void onFailure(Throwable caught) {
							GuiLog.serverError(caught);
						}

						@Override
						public void onSuccess(Void result) {
							list.removeSelectedData();
							list.deselectAllRecords();
						}
					});
				}
			});
		});

		MenuItem edit = new MenuItem();
		edit.setTitle(I18N.message("edit"));
		edit.addClickHandler(event -> {
			SubscriptionDialog dialog = new SubscriptionDialog(list);
			dialog.show();
		});

		contextMenu.setItems(edit, delete);
		contextMenu.showContextMenu();
	}
}