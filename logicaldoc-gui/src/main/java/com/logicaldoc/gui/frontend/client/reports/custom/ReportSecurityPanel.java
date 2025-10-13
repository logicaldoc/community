package com.logicaldoc.gui.frontend.client.reports.custom;

import java.util.ArrayList;
import java.util.List;

import com.logicaldoc.gui.common.client.beans.GUIAccessControlEntry;
import com.logicaldoc.gui.common.client.beans.GUIReport;
import com.logicaldoc.gui.common.client.grid.RefreshableListGrid;
import com.logicaldoc.gui.common.client.grid.UserListGridField;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.smartgwt.client.types.ListGridFieldType;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * Shows report's security policies
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2.2
 */
public class ReportSecurityPanel extends ReportDetailsTab {

	private static final String AVATAR = "avatar";

	private static final String ENTITY = "entity";

	private static final String ENTITY_ID = "entityId";

	private RefreshableListGrid aclGrid;

	public ReportSecurityPanel(GUIReport report, final ChangedHandler changedHandler) {
		super(report, changedHandler);
		setWidth100();
		setHeight100();

		this.report = report;
	}

	@Override
	protected void onDraw() {
		ListGridField entityId = new ListGridField(ENTITY_ID, ENTITY_ID);
		entityId.setCanEdit(false);
		entityId.setHidden(true);
		entityId.setAutoFitWidth(true);

		ListGridField entity = new UserListGridField(ENTITY, AVATAR, ENTITY);
		entity.setCanEdit(false);
		entity.setAutoFitWidth(true);
		entity.setRotateTitle(false);

		ListGridField read = new ListGridField(GUIAccessControlEntry.PERMISSION_READ.toLowerCase(),
				I18N.message(GUIAccessControlEntry.PERMISSION_READ.toLowerCase()));
		read.setType(ListGridFieldType.BOOLEAN);
		read.setCanEdit(true);

		ListGridField write = new ListGridField(GUIAccessControlEntry.PERMISSION_WRITE.toLowerCase(),
				I18N.message(GUIAccessControlEntry.PERMISSION_WRITE.toLowerCase()));
		write.setType(ListGridFieldType.BOOLEAN);
		write.setCanEdit(true);

		aclGrid = new RefreshableListGrid();
		aclGrid.setCanFreezeFields(true);
		aclGrid.setSelectionType(SelectionStyle.MULTIPLE);
		aclGrid.setAutoFetchData(true);
		aclGrid.setFields(entityId, entity, read, write);
		aclGrid.setDataSource(new ReportAclDS(report.getId()));

		HLayout buttons = new HLayout();
		buttons.setMembersMargin(4);
		buttons.setWidth100();
		buttons.setHeight(20);
		addGroupSelector(buttons);
		addUserSelector(buttons);

		VLayout body=new VLayout();
		body.setMembers(aclGrid, buttons);
		addMember(body);
	}

	private void addUserSelector(HLayout buttons) {
		final DynamicForm userForm = new DynamicForm();
		final SelectItem user = ItemFactory.newUserSelector("user", "adduser", null, true, false);
		userForm.setItems(user);

		user.addChangedHandler(changed -> {
			ListGridRecord selectedRecord = user.getSelectedRecord();
			if (selectedRecord == null)
				return;

			/*
			 * Check if the selected user is already present in the rights table
			 */
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
			rec.setAttribute(AVATAR, selectedRecord.getAttribute("id"));
			rec.setAttribute(ENTITY,
					selectedRecord.getAttribute("label") + " (" + selectedRecord.getAttribute("username") + ")");
			rec.setAttribute(GUIAccessControlEntry.PERMISSION_READ.toLowerCase(), true);
			rec.setAttribute(GUIAccessControlEntry.PERMISSION_WRITE.toLowerCase(), false);

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
			rec.setAttribute(AVATAR, "group");
			rec.setAttribute(ENTITY, selectedRecord.getAttribute("name"));
			rec.setAttribute(GUIAccessControlEntry.PERMISSION_READ.toLowerCase(), true);
			rec.setAttribute(GUIAccessControlEntry.PERMISSION_WRITE.toLowerCase(), false);

			aclGrid.addData(rec);
			group.clearValue();
		});
		buttons.addMember(groupForm);
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
			ace.setRead(Boolean.TRUE
					.equals(rec.getAttributeAsBoolean(GUIAccessControlEntry.PERMISSION_READ.toLowerCase())));
			ace.setWrite(Boolean.TRUE
					.equals(rec.getAttributeAsBoolean(GUIAccessControlEntry.PERMISSION_WRITE.toLowerCase())));
			acl.add(ace);
		}
		return acl;
	}

	boolean validate() {
		report.getAccessControlList().clear();
		report.setAccessControlList(getACL());
		return true;
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