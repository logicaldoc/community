package com.logicaldoc.gui.frontend.client.document.note;

import java.util.ArrayList;
import java.util.List;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIAccessControlEntry;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIDocumentNote;
import com.logicaldoc.gui.common.client.controllers.DocumentController;
import com.logicaldoc.gui.common.client.data.AccessControlListDS;
import com.logicaldoc.gui.common.client.grid.RefreshableListGrid;
import com.logicaldoc.gui.common.client.grid.UserListGridField;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.GridUtil;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.widgets.NoteChangeListener;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.ListGridFieldType;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.widgets.Button;
import com.smartgwt.client.widgets.HeaderControl;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.RichTextItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.tab.Tab;
import com.smartgwt.client.widgets.tab.TabSet;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * This is the form used to edit note or annotation
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.2
 */
public class NoteUpdateDialog extends Window {

	private static final String MESSAGE = "message";

	private ToolStrip toolStrip;

	private DynamicForm messageForm;

	private NoteChangeListener saveHandler;

	private RichTextItem messageBox;

	private static final String AVATAR = "avatar";

	private static final String ENTITY = "entity";

	private static final String ENTITY_ID = "entityId";

	private RefreshableListGrid aclGrid;

	private GUIDocumentNote note;

	public NoteUpdateDialog(GUIDocumentNote note, final NoteChangeListener saveHandler) {
		super();
		this.note = note;
		this.saveHandler = saveHandler;

		HeaderControl closeIcon = new HeaderControl(HeaderControl.CLOSE, event -> NoteUpdateDialog.this.destroy());

		setHeaderControls(HeaderControls.HEADER_LABEL, closeIcon);
		setTitle(I18N.message("note"));
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		setAutoSize(true);
		centerInPage();

		addResizedHandler(resized -> {
			if (getWidth() > 600)
				messageBox.setWidth(getWidth() - 10);
			else
				messageBox.setWidth(590);
		});
	}

	@Override
	protected void onDraw() {
		if (messageForm != null)
			removeItem(messageForm);

		if (toolStrip != null)
			removeItem(toolStrip);

		ToolStripButton save = new ToolStripButton(I18N.message("save"));
		save.addClickHandler(event -> onSave());
		ToolStripButton close = new ToolStripButton(I18N.message("close"));
		close.addClickHandler(event -> destroy());

		toolStrip = new ToolStrip();
		toolStrip.addButton(save);
		toolStrip.addSeparator();
		toolStrip.addButton(close);
		toolStrip.addFill();
		toolStrip.setWidth100();
		toolStrip.setMinWidth(590);

		messageBox = ItemFactory.newRichTextItemForNote(MESSAGE, MESSAGE, note.getMessage());
		messageBox.setBrowserSpellCheck(false);

		messageForm = new DynamicForm();
		messageForm.setWidth100();
		messageForm.setHeight100();
		messageForm.setItems(messageBox);

		Tab messageTab = new Tab(I18N.message("message"));
		messageTab.setPane(messageForm);

		Tab securityTab = new Tab(I18N.message("security"));
		securityTab.setPane(prepareSecurityPanel());

		TabSet tabs = new TabSet();
		tabs.setTabs(messageTab, securityTab);

		addItem(toolStrip);
		addItem(tabs);
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

		aclGrid = new RefreshableListGrid();
		aclGrid.setCanFreezeFields(true);
		aclGrid.setSelectionType(SelectionStyle.MULTIPLE);
		aclGrid.setAutoFetchData(true);
		aclGrid.setRotateHeaderTitles(true);
		aclGrid.setHeaderHeight(100);
		aclGrid.setFields(entityId, entity, read, write);
		aclGrid.setDataSource(new AccessControlListDS(note.getId(), "note"));

		HLayout buttons = new HLayout();
		buttons.setMembersMargin(4);
		buttons.setWidth100();
		buttons.setHeight(20);
		addGroupSelector(buttons);
		addUserSelector(buttons);
		if (note.getId() != 0L)
			addExportAndPrintButtons(buttons);

		panel.setMembers(aclGrid, buttons);
		return panel;
	}

	private void addExportAndPrintButtons(HLayout buttons) {
		Button exportButton = new Button(I18N.message(GUIAccessControlEntry.PERMISSION_EXPORT.toLowerCase()));
		exportButton.setAutoFit(true);
		buttons.addMember(exportButton);
		exportButton.addClickHandler(click -> GridUtil.exportCSV(aclGrid, true));

		Button printButton = new Button(I18N.message(GUIAccessControlEntry.PERMISSION_PRINT.toLowerCase()));
		printButton.setAutoFit(true);
		buttons.addMember(printButton);
		printButton.addClickHandler(click -> GridUtil.print(aclGrid));
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
			rec.setAttribute(GUIAccessControlEntry.PERMISSION_PREVIEW.toLowerCase(), true);

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
			rec.setAttribute(GUIAccessControlEntry.PERMISSION_PREVIEW.toLowerCase(), true);
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

	private void onSave() {
		if (!messageForm.validate())
			return;

		note.setMessage(messageForm.getValueAsString(MESSAGE));
		note.getAccessControlList().clear();
		note.setAccessControlList(getACL());

		DocumentService.Instance.get().saveNote(note, new DefaultAsyncCallback<>() {

			@Override
			public void onFailure(Throwable caught) {
				super.onFailure(caught);
				destroy();
			}

			@Override
			public void handleSuccess(GUIDocumentNote result) {
				if (saveHandler != null)
					saveHandler.onChanged();
				destroy();
				DocumentService.Instance.get().getById(result.getDocId(), new DefaultAsyncCallback<GUIDocument>() {
					@Override
					protected void handleSuccess(GUIDocument result) {
						DocumentController.get().modified(result);
					}
				});
			}
		});
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