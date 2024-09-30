package com.logicaldoc.gui.frontend.client.settings.automation;

import java.util.ArrayList;
import java.util.List;

import com.logicaldoc.gui.common.client.beans.GUIAccessControlEntry;
import com.logicaldoc.gui.common.client.beans.GUIAutomationRoutine;
import com.logicaldoc.gui.common.client.data.AutomationRoutineAclDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.GridUtil;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.widgets.grid.UserListGridField;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.ListGridFieldType;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.widgets.Button;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;

/**
 * This window shows the security policies of the automation routine.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.8.6
 */
public class AutomationRoutineSecurity extends AutomationRoutineDetailsTab {

	private static final String READ = "read";

	private static final String WRITE = "write";

	private static final String AVATAR = "avatar";

	private static final String ENTITY = "entity";

	private static final String ENTITY_ID = "entityId";

	private ListGrid list;

	private VLayout container = new VLayout();

	public AutomationRoutineSecurity(GUIAutomationRoutine routine, ChangedHandler changedHandler) {
		super(routine, changedHandler);
	}

	@Override
	protected void onDraw() {
		setMembers(container);

		ListGridField entityId = new ListGridField(ENTITY_ID, ENTITY_ID);
		entityId.setCanEdit(false);
		entityId.setHidden(true);
		entityId.setAutoFitWidth(true);

		ListGridField entity = new UserListGridField(ENTITY, AVATAR, ENTITY);
		entity.setCanEdit(false);
		entity.setRotateTitle(false);

		ListGridField read = new ListGridField(READ, I18N.message(READ), 80);
		read.setType(ListGridFieldType.BOOLEAN);
		read.setCanEdit(true);
		read.setAutoFitWidth(true);
		if (changedHandler != null)
			read.addChangedHandler(event -> changedHandler.onChanged(null));

		ListGridField write = new ListGridField(WRITE, I18N.message(WRITE), 80);
		write.setType(ListGridFieldType.BOOLEAN);
		write.setCanEdit(true);
		write.setAutoFitWidth(true);
		if (changedHandler != null)
			write.addChangedHandler(event -> changedHandler.onChanged(null));

		list = new ListGrid();
		list.setEmptyMessage(I18N.message("notitemstoshow"));
		list.setCanFreezeFields(true);
		list.setSelectionType(SelectionStyle.MULTIPLE);
		list.setAutoFetchData(true);
		list.setWidth100();
		list.setHeight100();
		list.setMinHeight(200);
		list.setMinWidth(300);

		list.setDataSource(new AutomationRoutineAclDS(routine.getId()));
		list.setFields(entityId, entity, read, write);

		container.addMember(list);

		if (changedHandler != null && routine.isWrite()) {
			addButtons();

			list.addCellContextClickHandler(event -> {
				if (event.getColNum() == 0) {
					Menu contextMenu = setupContextMenu();
					contextMenu.showContextMenu();
				}
				event.cancel();
			});

			list.addEditCompleteHandler(event -> {
				for (ListGridRecord rec : list.getSelectedRecords()) {
					GUIAccessControlEntry acl = routine.getAce(rec.getAttributeAsLong(ENTITY_ID));
					if (acl != null) {
						acl.setWrite(rec.getAttributeAsBoolean(WRITE, false));
						acl.setRead(rec.getAttributeAsBoolean(READ, false));
					}
				}
				changedHandler.onChanged(null);
			});
		}
	}

	private void addButtons() {
		HLayout buttons = new HLayout();
		buttons.setMembersMargin(4);
		buttons.setWidth100();
		buttons.setHeight(20);

		// Prepare the combo and button for adding a new Group
		addGroupSelector(buttons);

		// Prepare the combo and button for adding a new User
		addUserSelector(buttons);

		Button exportButton = new Button(I18N.message("export"));
		exportButton.setAutoFit(true);
		buttons.addMember(exportButton);
		exportButton.addClickHandler((ClickEvent event) -> GridUtil.exportCSV(list, true));

		Button printButton = new Button(I18N.message("print"));
		printButton.setAutoFit(true);
		buttons.addMember(printButton);
		printButton.addClickHandler((ClickEvent event) -> GridUtil.print(list));

		container.addMember(buttons);
	}

	private void addUserSelector(HLayout buttons) {
		final DynamicForm userForm = new DynamicForm();
		final SelectItem user = ItemFactory.newUserSelector("user", "adduser", null, true, false);
		userForm.setItems(user);
		user.addChangedHandler(event -> {
			ListGridRecord selectedRecord = user.getSelectedRecord();
			if (selectedRecord == null)
				return;

			/*
			 * Check if the selected user is already present in the rights table
			 */
			ListGridRecord[] records = list.getRecords();
			for (ListGridRecord test : records) {
				if (test.getAttribute(ENTITY_ID).equals(selectedRecord.getAttribute("usergroup"))) {
					user.clearValue();
					return;
				}
			}

			// Update the rights table
			ListGridRecord rec = new ListGridRecord();
			rec.setAttribute(ENTITY_ID, selectedRecord.getAttribute("usergroup"));
			rec.setAttribute(AVATAR, selectedRecord.getAttribute("id"));
			rec.setAttribute(ENTITY,
					selectedRecord.getAttribute("label") + " (" + selectedRecord.getAttribute("username") + ")");
			rec.setAttribute(READ, true);

			addRecord(rec);

			user.clearValue();
		});
		buttons.addMember(userForm);
	}

	private void addRecord(ListGridRecord rec) {
		list.addData(rec);

		GUIAccessControlEntry ace = new GUIAccessControlEntry();
		ace.setEntityId(rec.getAttributeAsLong(ENTITY_ID));
		ace.setRead(rec.getAttributeAsBoolean(READ, false));
		ace.setWrite(rec.getAttributeAsBoolean(WRITE, false));
		routine.addAce(ace);

		changedHandler.onChanged(null);
	}

	private void addGroupSelector(HLayout buttons) {
		final DynamicForm groupForm = new DynamicForm();
		final SelectItem group = ItemFactory.newGroupSelector("group", "addgroup");
		groupForm.setItems(group);
		buttons.addMember(groupForm);
		group.addChangedHandler((ChangedEvent event) -> {
			ListGridRecord selectedRecord = group.getSelectedRecord();
			if (selectedRecord == null)
				return;

			// Check if the selected user is already present in the rights
			// table
			ListGridRecord[] records = list.getRecords();
			for (ListGridRecord test : records) {
				if (test.getAttribute(ENTITY_ID).equals(selectedRecord.getAttribute("id"))) {
					group.clearValue();
					return;
				}
			}

			// Update the rights table
			ListGridRecord rec = new ListGridRecord();
			rec.setAttribute(ENTITY_ID, selectedRecord.getAttribute("id"));
			rec.setAttribute(AVATAR, "group");
			rec.setAttribute(ENTITY, selectedRecord.getAttribute("name"));
			rec.setAttribute(READ, true);

			addRecord(rec);
			group.clearValue();
		});
	}

	/**
	 * Creates a list of all the ACL
	 * 
	 * @return the array of rights
	 */
	public List<GUIAccessControlEntry> getACL() {
		int totalRecords = list.getRecordList().getLength();
		List<GUIAccessControlEntry> acl = new ArrayList<>();

		for (int i = 0; i < totalRecords; i++) {
			Record rec = list.getRecordList().get(i);
			GUIAccessControlEntry ace = new GUIAccessControlEntry();
			ace.setName(rec.getAttributeAsString(ENTITY));
			ace.setEntityId(Long.parseLong(rec.getAttribute(ENTITY_ID)));
			ace.setWrite("true".equals(rec.getAttributeAsString(WRITE)));
			ace.setRead("true".equals(rec.getAttributeAsString(READ)));
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
		deleteItem.addClickHandler(event -> onDelete());
		contextMenu.setItems(deleteItem);
		return contextMenu;
	}

	private void onDelete() {
		ListGridRecord[] selection = list.getSelectedRecords();
		if (selection == null || selection.length == 0)
			return;

		LD.ask(I18N.message("question"), I18N.message("confirmdelete"), confirm -> {
			if (Boolean.TRUE.equals(confirm)) {
				for (ListGridRecord listGridRecord : selection)
					routine.removeAce(listGridRecord.getAttributeAsLong(ENTITY_ID));
				list.removeSelectedData();
				changedHandler.onChanged(null);
			}
		});
	}

	@Override
	public boolean validate() {
		return true;
	}
}