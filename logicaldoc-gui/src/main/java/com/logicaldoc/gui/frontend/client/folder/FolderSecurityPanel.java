package com.logicaldoc.gui.frontend.client.folder;

import java.util.ArrayList;
import java.util.List;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.beans.GUIAccessControlEntry;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.data.AccessControlListDS;
import com.logicaldoc.gui.common.client.grid.UserListGridField;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.GridUtil;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.frontend.client.services.FolderService;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.Cursor;
import com.smartgwt.client.types.ListGridFieldType;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.types.VerticalAlignment;
import com.smartgwt.client.widgets.Button;
import com.smartgwt.client.widgets.HTMLPane;
import com.smartgwt.client.widgets.Img;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.events.ClickEvent;
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

/**
 * This panel shows the security policies.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class FolderSecurityPanel extends FolderDetailTab {

	private static final String AVATAR = "avatar";

	private static final String ENTITY = "entity";

	private static final String ENTITY_ID = "entityId";

	private AccessControlListDS aclDS;

	private ListGrid list;

	private VLayout container = new VLayout();

	private HLayout inheritInfoPanel = new HLayout();

	public FolderSecurityPanel(final GUIFolder folder) {
		super(folder, null);
	}

	@Override
	protected void onDraw() {
		container.setMembersMargin(3);
		addMember(container);
		refresh(folder);
	}

	private String prepareHeaderLabel(String labelKey) {
		return I18N.message(labelKey.toLowerCase());
	}

	void refresh(GUIFolder folder) {
		super.folder = folder;

		container.removeMembers(container.getMembers());

		if (folder.getSecurityRef() != null) {
			displayInheritingPanel(folder);
		}

		ListGridField entityId = new ListGridField(ENTITY_ID, ENTITY_ID);
		entityId.setCanEdit(false);
		entityId.setHidden(true);
		entityId.setAutoFitWidth(true);

		ListGridField entity = new UserListGridField(ENTITY, AVATAR, ENTITY);
		entity.setCanEdit(false);
		entity.setAutoFitWidth(true);
		entity.setRotateTitle(false);

		ListGridField read = new ListGridField(GUIAccessControlEntry.PERMISSION_READ.toLowerCase(),
				prepareHeaderLabel(GUIAccessControlEntry.PERMISSION_READ));
		read.setType(ListGridFieldType.BOOLEAN);
		read.setCanEdit(true);

		ListGridField preview = new ListGridField(GUIAccessControlEntry.PERMISSION_PREVIEW.toLowerCase(),
				prepareHeaderLabel(GUIAccessControlEntry.PERMISSION_PREVIEW));
		preview.setType(ListGridFieldType.BOOLEAN);
		preview.setCanEdit(true);

		ListGridField print = new ListGridField(GUIAccessControlEntry.PERMISSION_PRINT.toLowerCase(),
				prepareHeaderLabel(GUIAccessControlEntry.PERMISSION_PRINT));
		print.setType(ListGridFieldType.BOOLEAN);
		print.setCanEdit(true);

		ListGridField download = new ListGridField(GUIAccessControlEntry.PERMISSION_DOWNLOAD.toLowerCase(),
				prepareHeaderLabel(GUIAccessControlEntry.PERMISSION_DOWNLOAD));
		download.setType(ListGridFieldType.BOOLEAN);
		download.setCanEdit(true);

		ListGridField write = new ListGridField(GUIAccessControlEntry.PERMISSION_WRITE.toLowerCase(),
				prepareHeaderLabel(GUIAccessControlEntry.PERMISSION_WRITE));
		write.setType(ListGridFieldType.BOOLEAN);
		write.setCanEdit(true);

		ListGridField customid = new ListGridField(GUIAccessControlEntry.PERMISSION_CUSTOMID.toLowerCase(),
				prepareHeaderLabel(GUIAccessControlEntry.PERMISSION_CUSTOMID));
		customid.setType(ListGridFieldType.BOOLEAN);
		customid.setCanEdit(true);

		ListGridField revision = new ListGridField(GUIAccessControlEntry.PERMISSION_REVISION.toLowerCase(),
				prepareHeaderLabel(GUIAccessControlEntry.PERMISSION_REVISION));
		revision.setType(ListGridFieldType.BOOLEAN);
		revision.setCanEdit(true);

		ListGridField add = new ListGridField("add", prepareHeaderLabel("addfolder"));
		add.setType(ListGridFieldType.BOOLEAN);
		add.setCanEdit(true);

		ListGridField security = new ListGridField(GUIAccessControlEntry.PERMISSION_SECURITY.toLowerCase(),
				prepareHeaderLabel(GUIAccessControlEntry.PERMISSION_SECURITY));
		security.setType(ListGridFieldType.BOOLEAN);
		security.setCanEdit(true);

		ListGridField immutable = new ListGridField(GUIAccessControlEntry.PERMISSION_IMMUTABLE.toLowerCase(),
				prepareHeaderLabel(GUIAccessControlEntry.PERMISSION_IMMUTABLE));
		immutable.setType(ListGridFieldType.BOOLEAN);
		immutable.setCanEdit(true);

		ListGridField delete = new ListGridField(GUIAccessControlEntry.PERMISSION_DELETE.toLowerCase(),
				prepareHeaderLabel("ddelete"));
		delete.setType(ListGridFieldType.BOOLEAN);
		delete.setCanEdit(true);

		ListGridField rename = new ListGridField(GUIAccessControlEntry.PERMISSION_RENAME.toLowerCase(),
				prepareHeaderLabel(GUIAccessControlEntry.PERMISSION_RENAME));
		rename.setType(ListGridFieldType.BOOLEAN);
		rename.setCanEdit(true);

		ListGridField iimport = new ListGridField(GUIAccessControlEntry.PERMISSION_IMPORT.toLowerCase(),
				prepareHeaderLabel("iimport"));
		iimport.setType(ListGridFieldType.BOOLEAN);
		iimport.setCanEdit(true);

		ListGridField export = new ListGridField(GUIAccessControlEntry.PERMISSION_EXPORT.toLowerCase(),
				prepareHeaderLabel("eexport"));
		export.setType(ListGridFieldType.BOOLEAN);
		export.setCanEdit(true);

		ListGridField password = new ListGridField(GUIAccessControlEntry.PERMISSION_PASSWORD.toLowerCase(),
				prepareHeaderLabel(GUIAccessControlEntry.PERMISSION_PASSWORD));
		password.setType(ListGridFieldType.BOOLEAN);
		password.setCanEdit(true);

		ListGridField move = new ListGridField(GUIAccessControlEntry.PERMISSION_MOVE.toLowerCase(),
				prepareHeaderLabel(GUIAccessControlEntry.PERMISSION_MOVE));
		move.setType(ListGridFieldType.BOOLEAN);
		move.setCanEdit(true);

		ListGridField email = new ListGridField(GUIAccessControlEntry.PERMISSION_EMAIL.toLowerCase(),
				prepareHeaderLabel(GUIAccessControlEntry.PERMISSION_EMAIL));
		email.setType(ListGridFieldType.BOOLEAN);
		email.setCanEdit(true);

		list = new ListGrid();
		list.setEmptyMessage(I18N.message("notitemstoshow"));
		list.setCanFreezeFields(true);
		list.setSelectionType(SelectionStyle.MULTIPLE);
		list.setAutoFetchData(true);
		list.setRotateHeaderTitles(true);
		list.setHeaderHeight(100);

		aclDS = new AccessControlListDS(folder.getId(), "folder");
		list.setDataSource(aclDS);

		List<ListGridField> fields = new ArrayList<>();
		fields.add(entityId);
		fields.add(entity);
		fields.add(read);
		fields.add(preview);
		fields.add(print);
		fields.add(download);
		fields.add(email);
		fields.add(write);
		fields.add(add);
		fields.add(rename);
		if (Feature.enabled(Feature.CUSTOMID))
			fields.add(customid);
		if (Feature.enabled(Feature.REVISION))
			fields.add(revision);
		fields.add(delete);
		fields.add(move);
		fields.add(security);
		fields.add(immutable);
		fields.add(password);
		fields.add(iimport);
		fields.add(export);
		addSign(fields);
		addArchive(fields);
		addWorkflow(fields);
		addCalendar(fields);
		addSubscription(fields);
		addAutomation(fields);
		addStore(fields);
		addReadingReq(fields);

		list.setFields(fields.toArray(new ListGridField[0]));

		container.addMember(list);

		addCellContextClickHandler(folder);

		addButtons(folder);
	}

	private void displayInheritingPanel(GUIFolder folder) {
		FolderService.Instance.get().getFolder(folder.getSecurityRef().getId(), true, false, false,
				new DefaultAsyncCallback<>() {
					@Override
					public void onSuccess(final GUIFolder refFolder) {
						inheritInfoPanel = new HLayout();
						inheritInfoPanel.setMembersMargin(5);
						inheritInfoPanel.setStyleName("warn");
						inheritInfoPanel.setWidth100();
						inheritInfoPanel.setHeight(20);

						Label label = new Label(I18N.message("rightsinheritedfrom"));
						label.setWrap(false);

						Label path = new Label("<b><span style='text-decoration: underline'>"
								+ refFolder.getPathExtended() + "</span></b>");
						path.setWrap(false);
						path.addClickHandler(click -> FolderNavigator.get().openFolder(refFolder.getId()));

						HTMLPane spacer = new HTMLPane();
						spacer.setContents("<div>&nbsp;</div>");
						spacer.setWidth("*");
						spacer.setOverflow(Overflow.HIDDEN);

						Img closeImage = ItemFactory.newImgIcon("delete.png");
						closeImage.setHeight("16px");
						closeImage.addClickHandler(click -> inheritInfoPanel.setVisible(false));
						closeImage.setCursor(Cursor.HAND);
						closeImage.setTooltip(I18N.message("close"));
						closeImage.setLayoutAlign(Alignment.RIGHT);
						closeImage.setLayoutAlign(VerticalAlignment.CENTER);

						inheritInfoPanel.setMembers(label, path, spacer, closeImage);
						container.addMember(inheritInfoPanel, 0);
					}
				});
	}

	private void addReadingReq(List<ListGridField> fields) {
		if (Feature.enabled(Feature.READING_CONFIRMATION)) {
			ListGridField readingreq = new ListGridField(GUIAccessControlEntry.PERMISSION_READINGREQ.toLowerCase(),
					prepareHeaderLabel(GUIAccessControlEntry.PERMISSION_READINGREQ));
			readingreq.setType(ListGridFieldType.BOOLEAN);
			readingreq.setCanEdit(true);
			readingreq.setAutoFitWidth(true);
			fields.add(readingreq);
		}
	}

	private void addStore(List<ListGridField> fields) {
		if (Feature.enabled(Feature.MULTI_STORE)) {
			ListGridField store = new ListGridField(GUIAccessControlEntry.PERMISSION_STORE.toLowerCase(),
					prepareHeaderLabel(GUIAccessControlEntry.PERMISSION_STORE));
			store.setType(ListGridFieldType.BOOLEAN);
			store.setCanEdit(true);
			store.setAutoFitWidth(true);
			fields.add(store);
		}
	}

	private void addAutomation(List<ListGridField> fields) {
		if (Feature.enabled(Feature.AUTOMATION)) {
			ListGridField automation = new ListGridField(GUIAccessControlEntry.PERMISSION_AUTOMATION.toLowerCase(),
					prepareHeaderLabel(GUIAccessControlEntry.PERMISSION_AUTOMATION));
			automation.setType(ListGridFieldType.BOOLEAN);
			automation.setCanEdit(true);
			automation.setAutoFitWidth(true);
			fields.add(automation);
		}
	}

	private void addSubscription(List<ListGridField> fields) {
		if (Feature.enabled(Feature.AUDIT)) {
			ListGridField subscription = new ListGridField(GUIAccessControlEntry.PERMISSION_SUBSCRIPTION.toLowerCase(),
					prepareHeaderLabel(GUIAccessControlEntry.PERMISSION_SUBSCRIPTION));
			subscription.setType(ListGridFieldType.BOOLEAN);
			subscription.setCanEdit(true);
			subscription.setAutoFitWidth(true);
			fields.add(subscription);
		}
	}

	private void addCalendar(List<ListGridField> fields) {
		if (Feature.enabled(Feature.CALENDAR)) {
			ListGridField calendar = new ListGridField(GUIAccessControlEntry.PERMISSION_CALENDAR.toLowerCase(),
					prepareHeaderLabel(GUIAccessControlEntry.PERMISSION_CALENDAR));
			calendar.setType(ListGridFieldType.BOOLEAN);
			calendar.setCanEdit(true);
			fields.add(calendar);
		}
	}

	private void addWorkflow(List<ListGridField> fields) {
		if (Feature.enabled(Feature.WORKFLOW)) {
			ListGridField workflow = new ListGridField(GUIAccessControlEntry.PERMISSION_WORKFLOW.toLowerCase(),
					prepareHeaderLabel(GUIAccessControlEntry.PERMISSION_WORKFLOW));
			workflow.setType(ListGridFieldType.BOOLEAN);
			workflow.setCanEdit(true);
			fields.add(workflow);
		}
	}

	private void addArchive(List<ListGridField> fields) {
		if (Feature.enabled(Feature.ARCHIVING) || Feature.enabled(Feature.IMPEX)) {
			ListGridField archive = new ListGridField(GUIAccessControlEntry.PERMISSION_ARCHIVE.toLowerCase(),
					prepareHeaderLabel(GUIAccessControlEntry.PERMISSION_ARCHIVE));
			archive.setType(ListGridFieldType.BOOLEAN);
			archive.setCanEdit(true);
			fields.add(archive);
		}
	}

	private void addSign(List<ListGridField> fields) {
		if (Feature.enabled(Feature.DIGITAL_SIGNATURE)) {
			ListGridField sign = new ListGridField(GUIAccessControlEntry.PERMISSION_SIGN.toLowerCase(),
					prepareHeaderLabel(GUIAccessControlEntry.PERMISSION_SIGN));
			sign.setType(ListGridFieldType.BOOLEAN);
			sign.setCanEdit(true);
			fields.add(sign);
		}
	}

	private void addButtons(GUIFolder folder) {
		HLayout buttons = new HLayout();
		buttons.setMembersMargin(4);
		buttons.setWidth100();
		buttons.setHeight(20);
		container.addMember(buttons);

		Button save = new Button(I18N.message("save"));
		save.setAutoFit(true);
		buttons.addMember(save);

		Button applyRightsSubfolders = new Button(I18N.message("applytosubfolders"));
		applyRightsSubfolders.setAutoFit(true);
		buttons.addMember(applyRightsSubfolders);

		save.addClickHandler(click -> onSave(false));

		applyRightsSubfolders.addClickHandler(click -> onSave(true));

		Button inheritFromParent = new Button(I18N.message("inheritfromparent"));
		inheritFromParent.setAutoFit(true);
		buttons.addMember(inheritFromParent);
		inheritFromParent.addClickHandler((ClickEvent event) -> FolderService.Instance.get()
				.getFolder(folder.getParentId(), false, false, false, new DefaultAsyncCallback<>() {
					@Override
					public void onSuccess(GUIFolder parent) {
						LD.ask(I18N.message("inheritrights"),
								I18N.message("inheritrightsask", folder.getName(), parent.getName()),
								(Boolean interitConfirmed) -> {
									if (Boolean.TRUE.equals(interitConfirmed)) {
										FolderService.Instance.get().inheritACL(folder.getId(), folder.getParentId(),
												new DefaultAsyncCallback<>() {
													@Override
													public void onSuccess(GUIFolder arg) {
														FolderSecurityPanel.this.refresh(arg);
													}
												});
									}
								});
					}

				}));

		Button inheritRights = new Button(I18N.message("inherit"));
		inheritRights.setAutoFit(true);
		buttons.addMember(inheritRights);
		inheritRights.addClickHandler(click -> new InheritRightsDialog(FolderSecurityPanel.this).show());

		addGroupSelector(buttons);

		addUserSelector(buttons);

		addExportAndPrintButtons(buttons);
	}

	private void addUserSelector(HLayout buttons) {
		final DynamicForm userForm = new DynamicForm();
		final SelectItem user = ItemFactory.newUserSelector("user", "adduser", null, true, false);
		userForm.setItems(user);

		user.addChangedHandler((ChangedEvent userChangedEvent) -> {
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
			rec.setAttribute(GUIAccessControlEntry.PERMISSION_READ.toLowerCase(), true);
			rec.setAttribute(GUIAccessControlEntry.PERMISSION_PREVIEW.toLowerCase(), true);

			list.addData(rec);
			user.clearValue();
		});
		buttons.addMember(userForm);
	}

	private void addGroupSelector(HLayout buttons) {
		// Prepare the combo and button for adding a new Group
		final DynamicForm groupForm = new DynamicForm();
		final SelectItem group = ItemFactory.newGroupSelector("group", "addgroup");
		groupForm.setItems(group);
		group.addChangedHandler((ChangedEvent changedEvent) -> {
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
			rec.setAttribute(GUIAccessControlEntry.PERMISSION_READ.toLowerCase(), true);
			rec.setAttribute(GUIAccessControlEntry.PERMISSION_PREVIEW.toLowerCase(), true);
			list.addData(rec);
			group.clearValue();
		});
		buttons.addMember(groupForm);
	}

	private void addExportAndPrintButtons(HLayout buttons) {
		Button exportButton = new Button(I18N.message(GUIAccessControlEntry.PERMISSION_EXPORT.toLowerCase()));
		exportButton.setAutoFit(true);
		buttons.addMember(exportButton);
		exportButton.addClickHandler((ClickEvent exportClick) -> GridUtil.exportCSV(list, true));

		Button printButton = new Button(I18N.message(GUIAccessControlEntry.PERMISSION_PRINT.toLowerCase()));
		printButton.setAutoFit(true);
		buttons.addMember(printButton);
		printButton.addClickHandler((ClickEvent printClick) -> GridUtil.print(list));
	}

	private void addCellContextClickHandler(GUIFolder folder) {
		if (folder != null && folder.hasPermission(GUIAccessControlEntry.PERMISSION_SECURITY)) {
			list.addCellContextClickHandler((CellContextClickEvent contextClick) -> {
				if (contextClick.getColNum() == 0) {
					Menu contextMenu = setupContextMenu();
					contextMenu.showContextMenu();
				}
				contextClick.cancel();
			});
		}
	}

	/**
	 * Creates the collection of all the ACL
	 * 
	 * @return the list of ACEs
	 */
	public List<GUIAccessControlEntry> getACL() {
		int totalRecords = list.getRecordList().getLength();
		List<GUIAccessControlEntry> acl = new ArrayList<>();

		for (int i = 0; i < totalRecords; i++) {
			Record rec = list.getRecordList().get(i);
			GUIAccessControlEntry ace = new GUIAccessControlEntry();
			ace.setName(rec.getAttributeAsString(ENTITY));
			ace.setEntityId(Long.parseLong(rec.getAttribute(ENTITY_ID)));
			ace.setRead(Boolean.TRUE
					.equals(rec.getAttributeAsBoolean(GUIAccessControlEntry.PERMISSION_READ.toLowerCase())));
			ace.setPreview(Boolean.TRUE
					.equals(rec.getAttributeAsBoolean(GUIAccessControlEntry.PERMISSION_PREVIEW.toLowerCase())));
			ace.setPrint(Boolean.TRUE
					.equals(rec.getAttributeAsBoolean(GUIAccessControlEntry.PERMISSION_PRINT.toLowerCase())));
			ace.setWrite(Boolean.TRUE
					.equals(rec.getAttributeAsBoolean(GUIAccessControlEntry.PERMISSION_WRITE.toLowerCase())));
			ace.setCustomid(Boolean.TRUE
					.equals(rec.getAttributeAsBoolean(GUIAccessControlEntry.PERMISSION_CUSTOMID.toLowerCase())));
			ace.setRevision(Boolean.TRUE
					.equals(rec.getAttributeAsBoolean(GUIAccessControlEntry.PERMISSION_REVISION.toLowerCase())));
			ace.setDelete(Boolean.TRUE
					.equals(rec.getAttributeAsBoolean(GUIAccessControlEntry.PERMISSION_DELETE.toLowerCase())));
			ace.setAdd(
					Boolean.TRUE.equals(rec.getAttributeAsBoolean(GUIAccessControlEntry.PERMISSION_ADD.toLowerCase())));
			ace.setWorkflow(Boolean.TRUE
					.equals(rec.getAttributeAsBoolean(GUIAccessControlEntry.PERMISSION_WORKFLOW.toLowerCase())));
			ace.setSign(Boolean.TRUE
					.equals(rec.getAttributeAsBoolean(GUIAccessControlEntry.PERMISSION_SIGN.toLowerCase())));
			ace.setImport(Boolean.TRUE
					.equals(rec.getAttributeAsBoolean(GUIAccessControlEntry.PERMISSION_IMPORT.toLowerCase())));
			ace.setExport(Boolean.TRUE
					.equals(rec.getAttributeAsBoolean(GUIAccessControlEntry.PERMISSION_EXPORT.toLowerCase())));
			ace.setImmutable(Boolean.TRUE
					.equals(rec.getAttributeAsBoolean(GUIAccessControlEntry.PERMISSION_IMMUTABLE.toLowerCase())));
			ace.setRename(Boolean.TRUE
					.equals(rec.getAttributeAsBoolean(GUIAccessControlEntry.PERMISSION_RENAME.toLowerCase())));
			ace.setSecurity(Boolean.TRUE
					.equals(rec.getAttributeAsBoolean(GUIAccessControlEntry.PERMISSION_SECURITY.toLowerCase())));
			ace.setArchive(Boolean.TRUE
					.equals(rec.getAttributeAsBoolean(GUIAccessControlEntry.PERMISSION_ARCHIVE.toLowerCase())));
			ace.setDownload(Boolean.TRUE
					.equals(rec.getAttributeAsBoolean(GUIAccessControlEntry.PERMISSION_DOWNLOAD.toLowerCase())));
			ace.setCalendar(Boolean.TRUE
					.equals(rec.getAttributeAsBoolean(GUIAccessControlEntry.PERMISSION_CALENDAR.toLowerCase())));
			ace.setSubscription(Boolean.TRUE
					.equals(rec.getAttributeAsBoolean(GUIAccessControlEntry.PERMISSION_SUBSCRIPTION.toLowerCase())));
			ace.setPassword(Boolean.TRUE
					.equals(rec.getAttributeAsBoolean(GUIAccessControlEntry.PERMISSION_PASSWORD.toLowerCase())));
			ace.setMove(Boolean.TRUE
					.equals(rec.getAttributeAsBoolean(GUIAccessControlEntry.PERMISSION_MOVE.toLowerCase())));
			ace.setEmail(Boolean.TRUE
					.equals(rec.getAttributeAsBoolean(GUIAccessControlEntry.PERMISSION_EMAIL.toLowerCase())));
			ace.setAutomation(Boolean.TRUE
					.equals(rec.getAttributeAsBoolean(GUIAccessControlEntry.PERMISSION_AUTOMATION.toLowerCase())));
			ace.setStore(Boolean.TRUE
					.equals(rec.getAttributeAsBoolean(GUIAccessControlEntry.PERMISSION_STORE.toLowerCase())));
			ace.setReadingreq(Boolean.TRUE
					.equals(rec.getAttributeAsBoolean(GUIAccessControlEntry.PERMISSION_READINGREQ.toLowerCase())));

			acl.add(ace);
		}
		return acl;
	}

	@Override
	public void destroy() {
		super.destroy();
		if (aclDS != null)
			aclDS.destroy();
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
		deleteItem.addClickHandler(click -> onDeleteItem());

		contextMenu.setItems(deleteItem);
		return contextMenu;
	}

	private void onDeleteItem() {
		ListGridRecord[] selection = list.getSelectedRecords();
		if (selection == null || selection.length == 0)
			return;

		LD.ask(I18N.message("question"), I18N.message("confirmdelete"), (Boolean yes) -> {
			if (Boolean.TRUE.equals(yes))
				list.removeSelectedData();
		});
	}

	public void onSave(final boolean recursive) {
		// Apply all rights
		folder.setAccessControlList(this.getACL());

		FolderService.Instance.get().saveACL(folder, recursive, new DefaultAsyncCallback<>() {
			@Override
			public void onSuccess(Void result) {
				if (!recursive)
					GuiLog.info(I18N.message("appliedrights"), null);
				else
					GuiLog.info(I18N.message("appliedrightsonsubfolders"), null);
				folder.setSecurityRef(null);
				refresh(folder);
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