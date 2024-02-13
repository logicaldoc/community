package com.logicaldoc.gui.frontend.client.metadata.template;

import java.util.ArrayList;
import java.util.List;

import com.logicaldoc.gui.common.client.beans.GUIAccessControlEntry;
import com.logicaldoc.gui.common.client.beans.GUITemplate;
import com.logicaldoc.gui.common.client.data.AccessControlListDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.GridUtil;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.widgets.grid.UserListGridField;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.ListGridFieldType;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.widgets.Button;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;

/**
 * This panel shows the security policies of a template
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 8.7.2
 */
public class TemplateSecurityPanel extends VLayout {
	private static final String READ = "read";

	private static final String WRITE = "write";

	private static final String AVATAR = "avatar";

	private static final String ENTITY = "entity";

	private static final String ENTITY_ID = "entityId";

	protected GUITemplate template;

	protected ChangedHandler changedHandler;

	private ListGrid list;

	private boolean aclInitialized = false;

	public TemplateSecurityPanel(GUITemplate template, ChangedHandler changedHandler) {
		if (template == null) {
			setMembers(TemplatesPanel.SELECT_TEMPLATE);
			return;
		}

		this.template = template;
		this.changedHandler = changedHandler;
		setWidth100();
		setHeight100();
		setMembersMargin(3);
	}

	@Override
	protected void onDraw() {
		refresh(template);
	}

	void refresh(GUITemplate template) {
		removeMembers(getMembers());

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

		ListGridField write = new ListGridField(WRITE, I18N.message(WRITE), 80);
		write.setType(ListGridFieldType.BOOLEAN);
		write.setCanEdit(true);
		write.setAutoFitWidth(true);

		prepareList(template);

		list.setFields(entityId, entity, read, write);

		addMember(list);

		if (template.isWrite()) {
			list.addCellContextClickHandler(event -> {
				if (event.getColNum() == 0)
					setupContextMenu().showContextMenu();
				event.cancel();
			});
			list.addEditCompleteHandler(event -> changedHandler.onChanged(null));
		}

		addButons(template);
	}

	private void addButons(GUITemplate template) {
		HLayout buttons = new HLayout();
		buttons.setMembersMargin(4);
		buttons.setWidth100();
		buttons.setHeight(20);
		addMember(buttons);

		// Prepare the combo and button for adding a new Group
		final DynamicForm groupForm = new DynamicForm();
		final SelectItem group = ItemFactory.newGroupSelector("group", "addgroup");
		groupForm.setItems(group);
		if (template.isWrite())
			buttons.addMember(groupForm);

		group.addChangedHandler(event -> {
			ListGridRecord selectedRecord = group.getSelectedRecord();

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
			list.addData(rec);
			changedHandler.onChanged(null);
			group.clearValue();
		});

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

			// Update the ACL table
			ListGridRecord rec = new ListGridRecord();
			rec.setAttribute(ENTITY_ID, selectedRecord.getAttribute("usergroup"));
			rec.setAttribute(AVATAR, selectedRecord.getAttribute("id"));
			rec.setAttribute(ENTITY,
					selectedRecord.getAttribute("label") + " (" + selectedRecord.getAttribute("username") + ")");
			rec.setAttribute(READ, true);

			list.addData(rec);
			changedHandler.onChanged(null);
			user.clearValue();
		});
		if (template.isWrite())
			buttons.addMember(userForm);

		Button exportButton = new Button(I18N.message("export"));
		exportButton.setAutoFit(true);
		buttons.addMember(exportButton);
		exportButton.addClickHandler(event -> GridUtil.exportCSV(list, true));

		Button printButton = new Button(I18N.message("print"));
		printButton.setAutoFit(true);
		buttons.addMember(printButton);
		printButton.addClickHandler(event -> GridUtil.print(list));
	}

	private void prepareList(GUITemplate template) {
		list = new ListGrid();
		list.setEmptyMessage(I18N.message("notitemstoshow"));
		list.setCanFreezeFields(true);
		list.setSelectionType(SelectionStyle.MULTIPLE);
		list.setAutoFetchData(true);
		list.setWidth100();
		list.setHeight100();
		list.setMinHeight(200);
		list.setMinWidth(300);
		list.setDataSource(new AccessControlListDS(template.getId(), "template"));
		list.addDataArrivedHandler(event -> aclInitialized = true);
	}

	/**
	 * Creates an array of all the ACL
	 * 
	 * @return the complete ACL
	 */
	private List<GUIAccessControlEntry> getACL() {
		int totalRecords = list.getRecordList().getLength();
		List<GUIAccessControlEntry> acl = new ArrayList<>();
		for (int i = 0; i < totalRecords; i++) {
			Record rec = list.getRecordList().get(i);
			GUIAccessControlEntry ace = new GUIAccessControlEntry();
			ace.setName(rec.getAttributeAsString(ENTITY));
			ace.setEntityId(rec.getAttributeAsLong(ENTITY_ID));
			ace.setWrite(rec.getAttributeAsBoolean(WRITE));
			ace.setRead(rec.getAttributeAsBoolean(READ));
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

		LD.ask(I18N.message("question"), I18N.message("confirmdelete"), value -> {
			if (Boolean.TRUE.equals(value)) {
				list.removeSelectedData();
				changedHandler.onChanged(null);
			}
		});
	}

	protected boolean validate() {
		if (aclInitialized)
			try {
				if (template.isWrite())
					template.setAccessControlList(getACL());
			} catch (Exception t) {
				// Nothing to do
			}
		return true;
	}

	public GUITemplate getTemplate() {
		return template;
	}
}