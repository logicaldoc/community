package com.logicaldoc.gui.frontend.client.document.note;

import java.util.ArrayList;
import java.util.List;

import com.logicaldoc.gui.common.client.beans.GUIAccessControlEntry;
import com.logicaldoc.gui.common.client.beans.GUIDocumentNote;
import com.logicaldoc.gui.common.client.data.AccessControlListDS;
import com.logicaldoc.gui.common.client.grid.RefreshableListGrid;
import com.logicaldoc.gui.common.client.grid.UserListGridField;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.GridUtil;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.widgets.NoteChangedListener;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.ListGridFieldType;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.widgets.HeaderControl;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * This is the form used to edit note/annotation's ACL
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2.2
 */
public class NoteSecurityDialog extends Window {

	private ToolStrip toolStrip;

	private NoteChangedListener changedListener;

	private static final String AVATAR = "avatar";

	private static final String ENTITY = "entity";

	private static final String ENTITY_ID = "entityId";

	private RefreshableListGrid aclGrid;

	private GUIDocumentNote note;

	public NoteSecurityDialog(GUIDocumentNote note, final NoteChangedListener changedListener) {
		super();
		this.note = note;
		this.changedListener = changedListener;

		HeaderControl closeIcon = new HeaderControl(HeaderControl.CLOSE, event -> NoteSecurityDialog.this.destroy());

		setHeaderControls(HeaderControls.HEADER_LABEL, closeIcon);
		setTitle(I18N.message("security"));
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		setWidth(500);
		setHeight(400);
		centerInPage();
	}

	@Override
	protected void onDraw() {
		if (toolStrip != null)
			removeItem(toolStrip);

		ToolStripButton save = new ToolStripButton(I18N.message("save"));
		save.addClickHandler(event -> onSave());

		ToolStripButton close = new ToolStripButton(I18N.message("close"));
		close.addClickHandler(event -> destroy());

		ToolStripButton export = new ToolStripButton(
				I18N.message(GUIAccessControlEntry.PERMISSION_EXPORT.toLowerCase()));
		export.setAutoFit(true);
		export.addClickHandler(click -> GridUtil.exportCSV(aclGrid, true));

		ToolStripButton print = new ToolStripButton(I18N.message(GUIAccessControlEntry.PERMISSION_PRINT.toLowerCase()));
		print.setAutoFit(true);
		print.addClickHandler(click -> GridUtil.print(aclGrid));

		toolStrip = new ToolStrip();
		toolStrip.addButton(save);

		if (note.getId() != 0L) {
			toolStrip.addSeparator();
			toolStrip.addButton(export);
			toolStrip.addButton(print);
		}

		toolStrip.addSeparator();
		toolStrip.addButton(close);
		toolStrip.addFill();
		toolStrip.setWidth100();
		toolStrip.setMinWidth(590);

		addItem(toolStrip);
		addItem(prepareSecurityPanel());
	}

	private VLayout prepareSecurityPanel() {
		VLayout panel = new VLayout();

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

		ListGridField delete = new ListGridField(GUIAccessControlEntry.PERMISSION_DELETE.toLowerCase(),
				I18N.message("ddelete"));
		delete.setType(ListGridFieldType.BOOLEAN);
		delete.setCanEdit(true);

		ListGridField security = new ListGridField(GUIAccessControlEntry.PERMISSION_SECURITY.toLowerCase(),
				I18N.message(GUIAccessControlEntry.PERMISSION_SECURITY.toLowerCase()));
		security.setType(ListGridFieldType.BOOLEAN);
		security.setCanEdit(true);

		aclGrid = new RefreshableListGrid();
		aclGrid.setCanFreezeFields(true);
		aclGrid.setSelectionType(SelectionStyle.MULTIPLE);
		aclGrid.setAutoFetchData(true);
		aclGrid.setFields(entityId, entity, read, write, delete, security);
		aclGrid.setDataSource(new AccessControlListDS(note.getId(), "note"));

		HLayout buttons = new HLayout();
		buttons.setMembersMargin(4);
		buttons.setWidth100();
		buttons.setHeight(20);
		addGroupSelector(buttons);
		addUserSelector(buttons);

		panel.setMembers(aclGrid, buttons);
		return panel;
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
			rec.setAttribute(GUIAccessControlEntry.PERMISSION_DELETE.toLowerCase(), false);
			rec.setAttribute(GUIAccessControlEntry.PERMISSION_SECURITY.toLowerCase(), false);

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
			rec.setAttribute(GUIAccessControlEntry.PERMISSION_DELETE.toLowerCase(), false);
			rec.setAttribute(GUIAccessControlEntry.PERMISSION_SECURITY.toLowerCase(), false);
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
			ace.setDelete(Boolean.TRUE
					.equals(rec.getAttributeAsBoolean(GUIAccessControlEntry.PERMISSION_DELETE.toLowerCase())));
			ace.setSecurity(Boolean.TRUE
					.equals(rec.getAttributeAsBoolean(GUIAccessControlEntry.PERMISSION_SECURITY.toLowerCase())));
			acl.add(ace);
		}
		return acl;
	}

	private void onSave() {
		note.getAccessControlList().clear();
		note.setAccessControlList(getACL());

		try {
			if (changedListener != null)
				changedListener.onChanged(note);
		} finally {
			destroy();
		}
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