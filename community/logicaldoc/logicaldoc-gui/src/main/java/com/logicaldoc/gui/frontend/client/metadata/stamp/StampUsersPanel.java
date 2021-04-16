package com.logicaldoc.gui.frontend.client.metadata.stamp;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.data.StampUsersDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.widgets.InfoPanel;
import com.logicaldoc.gui.common.client.widgets.RefreshableListGrid;
import com.logicaldoc.gui.common.client.widgets.grid.AvatarListGridField;
import com.logicaldoc.gui.frontend.client.services.StampService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.ListGridFieldType;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
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
 * This panel shows the list of users assigned to a stamp.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.3
 */
public class StampUsersPanel extends VLayout {

	private RefreshableListGrid list;

	private long stampId;

	public StampUsersPanel(final long stampId) {
		this.stampId = stampId;
		setWidth100();
		setHeight100();
	}

	@Override
	public void onDraw() {
		final InfoPanel infoPanel = new InfoPanel("");

		HLayout buttons = new HLayout();
		buttons.setHeight(25);
		buttons.setMargin(3);

		// Prepare the list for adding a new user
		final DynamicForm userForm = new DynamicForm();
		final SelectItem user = ItemFactory.newUserSelector("user", "adduser", null, true, false);
		user.setMultiple(true);
		user.addChangedHandler(new ChangedHandler() {

			@Override
			public void onChanged(ChangedEvent event) {
				ListGridRecord[] selection = user.getSelectedRecords();
				if (selection == null || selection.length == 0)
					return;

				long ids[] = new long[selection.length];
				for (int i = 0; i < ids.length; i++)
					ids[i] = Long.parseLong(selection[i].getAttributeAsString("id"));

				StampService.Instance.get().addUsers(ids, stampId, new AsyncCallback<Void>() {

					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
						user.clearValue();
					}

					@Override
					public void onSuccess(Void ret) {
						list.refresh(new StampUsersDS(stampId));
						user.clearValue();
					}
				});
			}
		});

		userForm.setItems(user);
		buttons.addMember(userForm);

		AvatarListGridField avatar = new AvatarListGridField();
		
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

		ListGridField enabled = new ListGridField("eenabled", " ", 24);
		enabled.setType(ListGridFieldType.IMAGE);
		enabled.setCanSort(false);
		enabled.setAlign(Alignment.CENTER);
		enabled.setShowDefaultContextMenu(false);
		enabled.setImageURLPrefix(Util.imagePrefix());
		enabled.setImageURLSuffix(".gif");
		enabled.setCanFilter(false);

		list = new RefreshableListGrid();
		list.setEmptyMessage(I18N.message("notitemstoshow"));
		list.setShowRecordComponents(true);
		list.setShowRecordComponentsByCell(true);
		list.setCanFreezeFields(true);
		list.setAutoFetchData(true);
		list.setSelectionType(SelectionStyle.MULTIPLE);
		list.setFilterOnKeypress(true);
		list.setShowFilterEditor(true);
		list.setDataSource(new StampUsersDS(stampId));
		list.setFields(id, enabled, avatar, username, firstName, name, email, cell, phone);

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

		setMembers(infoPanel, list, buttons);
	}

	private void showContextMenu() {
		Menu contextMenu = new Menu();

		final ListGridRecord[] selection = list.getSelectedRecords();

		MenuItem remove = new MenuItem();
		remove.setTitle(I18N.message("remove"));
		remove.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				if (selection == null || selection.length == 0)
					return;
				final long[] ids = new long[selection.length];
				for (int i = 0; i < selection.length; i++)
					ids[i] = Long.parseLong(selection[i].getAttribute("id"));

				LD.ask(I18N.message("question"), I18N.message("confirmdelete"), new BooleanCallback() {
					@Override
					public void execute(Boolean value) {
						if (value) {
							StampService.Instance.get().removeUsers(ids, stampId, new AsyncCallback<Void>() {
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