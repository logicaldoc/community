package com.logicaldoc.gui.frontend.client.security;

import java.util.ArrayList;
import java.util.List;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIAccessControlEntry;
import com.logicaldoc.gui.common.client.beans.GUIMenu;
import com.logicaldoc.gui.common.client.data.AccessControlListDS;
import com.logicaldoc.gui.common.client.grid.UserListGridField;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.services.SecurityService;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.smartgwt.client.types.ListGridEditEvent;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.widgets.Button;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.CellContextClickEvent;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.events.MenuItemClickEvent;

/**
 * This panel shows the security policies of a menu.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class MenuSecurityPanel extends VLayout {

	private static final String AVATAR = "avatar";

	private static final String ENTITY = "entity";

	private static final String ENTITY_ID = "entityId";

	private ListGrid aclGrid;

	protected GUIMenu menu;

	boolean withSaveButton = false;

	public MenuSecurityPanel(final GUIMenu menu, boolean withSaveButton) {
		this.menu = menu;
		this.withSaveButton = withSaveButton;
	}

	@Override
	public void onDraw() {
		final VLayout container = new VLayout();
		container.setMembersMargin(3);
		addMember(container);

		ListGridField entityId = new ListGridField(ENTITY_ID, I18N.message(ENTITY_ID.toLowerCase()), 50);
		entityId.setCanEdit(false);
		entityId.setHidden(true);

		ListGridField entity = new UserListGridField(ENTITY, AVATAR, ENTITY);
		entity.setCanEdit(false);

		aclGrid = new ListGrid();
		aclGrid.setEmptyMessage(I18N.message("notitemstoshow"));
		aclGrid.setCanFreezeFields(true);
		aclGrid.setSelectionType(SelectionStyle.MULTIPLE);
		aclGrid.setAutoFetchData(true);
		aclGrid.setDataSource(new AccessControlListDS(menu.getId(), "menu"));
		aclGrid.setFields(entityId, entity);
		aclGrid.setCanEdit(true);
		aclGrid.setEditEvent(ListGridEditEvent.CLICK);
		aclGrid.setModalEditing(true);
		aclGrid.addCellContextClickHandler((CellContextClickEvent event) -> {
			if (event.getColNum() == 0) {
				Menu contextMenu = setupContextMenu();
				contextMenu.showContextMenu();
			}
			event.cancel();
		});

		container.addMember(aclGrid);

		HLayout buttons = prepareButtons();
		container.addMember(buttons);
	}

	private HLayout prepareButtons() {
		HLayout buttons = new HLayout();
		buttons.setMembersMargin(4);
		buttons.setWidth100();
		buttons.setHeight(20);

		Button save = new Button(I18N.message("applyrights"));
		save.setAutoFit(true);
		save.addClickHandler(click -> onSave());
		if (withSaveButton)
			buttons.addMember(save);

		addGroupSelector(buttons);

		addUserSelector(buttons);

		return buttons;
	}

	private void addUserSelector(HLayout buttons) {
		final DynamicForm userForm = new DynamicForm();
		final SelectItem user = ItemFactory.newUserSelector("user", "adduser", null, false, false);
		userForm.setItems(user);

		user.addChangedHandler((ChangedEvent event) -> {
			ListGridRecord selectedRecord = user.getSelectedRecord();
			if (selectedRecord == null)
				return;

			// Check if the selected user is already present in the rights
			// table
			ListGridRecord[] records = aclGrid.getRecords();
			for (ListGridRecord test : records) {
				if (test.getAttribute(ENTITY_ID).equals(selectedRecord.getAttribute("usergroup"))) {
					user.clearValue();
					return;
				}
			}

			// Update the rights table
			ListGridRecord rec = new ListGridRecord();
			rec.setAttribute(ENTITY_ID, selectedRecord.getAttribute("usergroup"));
			rec.setAttribute(ENTITY,
					selectedRecord.getAttribute("label") + " (" + selectedRecord.getAttribute("username") + ")");
			rec.setAttribute(AVATAR, selectedRecord.getAttribute("id"));
			rec.setAttribute(GUIAccessControlEntry.PERMISSION_READ.toLowerCase(), true);
			aclGrid.addData(rec);
			user.clearValue();
		});
		buttons.addMember(userForm);
	}

	private void addGroupSelector(HLayout buttons) {
		// Prepare the combo and button for adding a new Group
		final DynamicForm groupForm = new DynamicForm();
		final SelectItem group = ItemFactory.newGroupSelector("group", "addgroup");
		groupForm.setItems(group);
		buttons.addMember(groupForm);

		group.addChangedHandler(changed -> {
			ListGridRecord selectedRecord = group.getSelectedRecord();
			if (selectedRecord == null)
				return;

			// Check if the selected user is already present in the rights
			// table
			ListGridRecord[] records = aclGrid.getRecords();
			for (ListGridRecord test : records) {
				if (test.getAttribute(ENTITY_ID).equals(selectedRecord.getAttribute("id"))) {
					group.clearValue();
					return;
				}
			}

			// Update the rights table
			ListGridRecord rec = new ListGridRecord();
			rec.setAttribute(ENTITY_ID, selectedRecord.getAttribute("id"));
			rec.setAttribute(ENTITY, selectedRecord.getAttribute("name"));
			rec.setAttribute(AVATAR, "group");
			rec.setAttribute(GUIAccessControlEntry.PERMISSION_READ.toLowerCase(), true);
			aclGrid.addData(rec);
			group.clearValue();
		});
	}

	/**
	 * Create an array of all rights defined
	 * 
	 * @return array of rights
	 */
	public List<GUIAccessControlEntry> getACL() {
		ListGridRecord[] records = aclGrid.getRecords();
		List<GUIAccessControlEntry> acl = new ArrayList<>();
		for (ListGridRecord rec : records) {
			GUIAccessControlEntry ace = new GUIAccessControlEntry();
			ace.setName(rec.getAttributeAsString(ENTITY));
			ace.setEntityId(Long.parseLong(rec.getAttribute(ENTITY_ID)));
			ace.setRead(Boolean.TRUE.equals(rec.getAttributeAsBoolean(GUIAccessControlEntry.PERMISSION_READ.toLowerCase())));
			acl.add(ace);
		}
		return acl;
	}

	/**
	 * Prepares the context menu
	 * 
	 * @return the context menu
	 */
	private Menu setupContextMenu() {
		Menu contextMenu = new Menu();

		MenuItem deleteItem = new MenuItem();
		deleteItem.setTitle(I18N.message("ddelete"));
		deleteItem.addClickHandler((MenuItemClickEvent event) -> {
			ListGridRecord[] selection = aclGrid.getSelectedRecords();
			if (selection == null || selection.length == 0)
				return;

			LD.ask(I18N.message("question"), I18N.message("confirmdelete"), choice -> {
				if (Boolean.TRUE.equals(choice)) {
					aclGrid.removeSelectedData();
					if (!withSaveButton)
						onSave();
				}
			});
		});

		contextMenu.setItems(deleteItem);
		return contextMenu;
	}

	public void onSave() {
		// Apply the ACL
		menu.setAccessControlList(getACL());

		SecurityService.Instance.get().saveACL(menu, new DefaultAsyncCallback<>() {
			@Override
			public void onSuccess(Void result) {
				GuiLog.info(I18N.message("appliedrightsmenu"), null);
			}
		});
	}

	@Override
	public boolean equals(Object obj) {
		if (obj instanceof MenuSecurityPanel)
			return super.equals(obj);
		else
			return false;
	}

	@Override
	public int hashCode() {
		return super.hashCode();
	}
}