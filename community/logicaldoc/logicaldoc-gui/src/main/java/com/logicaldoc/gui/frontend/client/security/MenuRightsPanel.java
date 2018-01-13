package com.logicaldoc.gui.frontend.client.security;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIMenu;
import com.logicaldoc.gui.common.client.beans.GUIRight;
import com.logicaldoc.gui.common.client.data.RightsDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.common.client.services.SecurityService;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.smartgwt.client.types.ListGridEditEvent;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.widgets.Button;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.CellContextClickEvent;
import com.smartgwt.client.widgets.grid.events.CellContextClickHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.events.MenuItemClickEvent;

/**
 * This panel shows the security policies of a menu.
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 6.0
 */
public class MenuRightsPanel extends VLayout {

	private RightsDS dataSource;

	private ListGrid list;

	private VLayout container = new VLayout();

	protected GUIMenu menu;

	public MenuRightsPanel(final GUIMenu menu) {
		this.menu = menu;

		container.setMembersMargin(3);
		addMember(container);

		ListGridField entityId = new ListGridField("entityId", "entityId", 50);
		entityId.setCanEdit(false);
		entityId.setHidden(true);

		ListGridField entity = new ListGridField("entity", I18N.message("entity"), 200);
		entity.setCanEdit(false);

		list = new ListGrid();
		list.setEmptyMessage(I18N.message("notitemstoshow"));
		list.setCanFreezeFields(true);
		list.setSelectionType(SelectionStyle.MULTIPLE);
		list.setAutoFetchData(true);
		dataSource = new RightsDS(menu.getId(), false);
		list.setDataSource(dataSource);
		list.setFields(entityId, entity);
		container.addMember(list);
		list.setCanEdit(true);
		list.setEditEvent(ListGridEditEvent.CLICK);
		list.setModalEditing(true);
		list.addCellContextClickHandler(new CellContextClickHandler() {
			@Override
			public void onCellContextClick(CellContextClickEvent event) {
				if (event.getColNum() == 0) {
					Menu contextMenu = setupContextMenu();
					contextMenu.showContextMenu();
				}
				event.cancel();
			}
		});

		HLayout buttons = new HLayout();
		buttons.setMembersMargin(4);
		buttons.setWidth100();
		buttons.setHeight(20);
		container.addMember(buttons);

		Button applyRights = new Button(I18N.message("applyrights"));
		applyRights.setAutoFit(true);
		applyRights.addClickHandler(new ClickHandler() {

			@Override
			public void onClick(ClickEvent event) {
				onApply();
			}
		});
		buttons.addMember(applyRights);

		// Prepare the combo and button for adding a new Group
		final DynamicForm groupForm = new DynamicForm();
		final SelectItem group = ItemFactory.newGroupSelector("group", "addgroup");
		groupForm.setItems(group);
		buttons.addMember(groupForm);

		group.addChangedHandler(new ChangedHandler() {
			@Override
			public void onChanged(ChangedEvent event) {
				ListGridRecord selectedRecord = group.getSelectedRecord();
				if (selectedRecord == null)
					return;

				// Check if the selected user is already present in the rights
				// table
				ListGridRecord[] records = list.getRecords();
				for (ListGridRecord test : records) {
					if (test.getAttribute("entityId").equals(selectedRecord.getAttribute("id"))) {
						group.clearValue();
						return;
					}
				}

				// Update the rights table
				ListGridRecord record = new ListGridRecord();
				record.setAttribute("entityId", selectedRecord.getAttribute("id"));
				record.setAttribute("entity", I18N.message("group") + ": " + selectedRecord.getAttribute("name"));
				record.setAttribute("read", true);
				list.addData(record);
				group.clearValue();
			}
		});

		final DynamicForm userForm = new DynamicForm();
		final SelectItem user = ItemFactory.newUserSelector("user", "adduser", null, false);
		userForm.setItems(user);

		user.addChangedHandler(new ChangedHandler() {
			@Override
			public void onChanged(ChangedEvent event) {
				ListGridRecord selectedRecord = user.getSelectedRecord();
				if (selectedRecord == null)
					return;

				// Check if the selected user is already present in the rights
				// table
				ListGridRecord[] records = list.getRecords();
				for (ListGridRecord test : records) {
					if (test.getAttribute("entityId").equals(selectedRecord.getAttribute("usergroup"))) {
						user.clearValue();
						return;
					}
				}

				// Update the rights table
				ListGridRecord record = new ListGridRecord();
				record.setAttribute("entityId", selectedRecord.getAttribute("usergroup"));
				record.setAttribute("entity", I18N.message("user") + ": " + selectedRecord.getAttribute("label") + " ("
						+ selectedRecord.getAttribute("username") + ")");
				record.setAttribute("read", true);
				list.addData(record);
				user.clearValue();
			}
		});

		buttons.addMember(userForm);
	}

	/**
	 * Create an array of all rights defined
	 */
	public GUIRight[] getRights() {
		ListGridRecord[] records = list.getRecords();
		GUIRight[] tmp = new GUIRight[records.length];

		int i = 0;
		for (ListGridRecord record : records) {
			GUIRight right = new GUIRight();

			right.setName(record.getAttributeAsString("entity"));
			right.setEntityId(Long.parseLong(record.getAttribute("entityId")));

			tmp[i] = right;
			i++;
		}

		return tmp;
	}

	@Override
	public void destroy() {
		super.destroy();
		if (dataSource != null)
			dataSource.destroy();
	}

	/**
	 * Prepares the context menu.
	 */
	private Menu setupContextMenu() {
		Menu contextMenu = new Menu();

		MenuItem deleteItem = new MenuItem();
		deleteItem.setTitle(I18N.message("ddelete"));
		deleteItem.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				ListGridRecord[] selection = list.getSelection();
				if (selection == null || selection.length == 0)
					return;

				LD.ask(I18N.message("question"), I18N.message("confirmdelete"), new BooleanCallback() {
					@Override
					public void execute(Boolean value) {
						if (value) {
							list.removeSelectedData();
						}
					}
				});
			}
		});

		contextMenu.setItems(deleteItem);
		return contextMenu;
	}

	public void onApply() {
		// Apply all rights
		menu.setRights(this.getRights());

		SecurityService.Instance.get().applyRights(menu, new AsyncCallback<Void>() {

			@Override
			public void onFailure(Throwable caught) {
				Log.serverError(caught);
			}

			@Override
			public void onSuccess(Void result) {
				Log.info(I18N.message("appliedrightsmenu"), null);
			}
		});

	}
}