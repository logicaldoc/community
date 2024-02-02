package com.logicaldoc.gui.frontend.client.document;

import java.util.ArrayList;
import java.util.List;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIRight;
import com.logicaldoc.gui.common.client.data.RightsDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.GridUtil;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.widgets.grid.UserListGridField;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.ListGridFieldType;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.widgets.Button;
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
import com.smartgwt.client.widgets.menu.events.MenuItemClickEvent;

/**
 * This panel shows the security policies of a document.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.9.1
 */
public class DocumentSecurityPanel extends DocumentDetailTab {

	private static final String ARCHIVE = "archive";

	private static final String WORKFLOW = "workflow";

	private static final String CALENDAR = "calendar";

	private static final String SUBSCRIPTION = "subscription";

	private static final String AUTOMATION = "automation";

	private static final String READINGREQ = "readingreq";

	private static final String EMAIL = "email";

	private static final String PASSWORD = "password";

	private static final String EXPORT = "export";

	private static final String RENAME = "rename";

	private static final String DELETE = "delete";

	private static final String IMMUTABLE = "immutable";

	private static final String SECURITY = "security";

	private static final String WRITE = "write";

	private static final String DOWNLOAD = "download";

	private static final String PRINT = "print";

	private static final String AVATAR = "avatar";

	private static final String ENTITY = "entity";

	private static final String ENTITY_ID = "entityId";

	private RightsDS dataSource;

	private ListGrid list;

	private VLayout container = new VLayout();

	public DocumentSecurityPanel(final GUIDocument document) {
		super(document, null);
	}

	@Override
	protected void onDraw() {
		container.setMembersMargin(3);
		addMember(container);
		refresh(document);
	}

	private String prepareHeaderLabel(String labelKey) {
		return I18N.message(labelKey);
	}

	void refresh(GUIDocument document) {
		super.document = document;

		container.removeMembers(container.getMembers());

		ListGridField entityId = new ListGridField(ENTITY_ID, ENTITY_ID);
		entityId.setCanEdit(false);
		entityId.setHidden(true);
		entityId.setAutoFitWidth(true);

		ListGridField entity = new UserListGridField(ENTITY, AVATAR, ENTITY);
		entity.setCanEdit(false);
		entity.setAutoFitWidth(true);
		entity.setRotateTitle(false);

		ListGridField read = new ListGridField("read", prepareHeaderLabel("read"));
		read.setType(ListGridFieldType.BOOLEAN);
		read.setCanEdit(true);

		ListGridField print = new ListGridField(PRINT, prepareHeaderLabel(PRINT));
		print.setType(ListGridFieldType.BOOLEAN);
		print.setCanEdit(true);

		ListGridField download = new ListGridField(DOWNLOAD, prepareHeaderLabel(DOWNLOAD));
		download.setType(ListGridFieldType.BOOLEAN);
		download.setCanEdit(true);

		ListGridField write = new ListGridField(WRITE, prepareHeaderLabel(WRITE));
		write.setType(ListGridFieldType.BOOLEAN);
		write.setCanEdit(true);

		ListGridField security = new ListGridField(SECURITY, prepareHeaderLabel(SECURITY));
		security.setType(ListGridFieldType.BOOLEAN);
		security.setCanEdit(true);

		ListGridField immutable = new ListGridField(IMMUTABLE, prepareHeaderLabel(IMMUTABLE));
		immutable.setType(ListGridFieldType.BOOLEAN);
		immutable.setCanEdit(true);

		ListGridField delete = new ListGridField(DELETE, prepareHeaderLabel("ddelete"));
		delete.setType(ListGridFieldType.BOOLEAN);
		delete.setCanEdit(true);

		ListGridField rename = new ListGridField(RENAME, prepareHeaderLabel(RENAME));
		rename.setType(ListGridFieldType.BOOLEAN);
		rename.setCanEdit(true);

		ListGridField password = new ListGridField(PASSWORD, prepareHeaderLabel(PASSWORD));
		password.setType(ListGridFieldType.BOOLEAN);
		password.setCanEdit(true);

		ListGridField move = new ListGridField("move", prepareHeaderLabel("move"));
		move.setType(ListGridFieldType.BOOLEAN);
		move.setCanEdit(true);

		ListGridField email = new ListGridField(EMAIL, prepareHeaderLabel(EMAIL));
		email.setType(ListGridFieldType.BOOLEAN);
		email.setCanEdit(true);

		list = new ListGrid();
		list.setEmptyMessage(I18N.message("notitemstoshow"));
		list.setCanFreezeFields(true);
		list.setSelectionType(SelectionStyle.MULTIPLE);
		list.setAutoFetchData(true);
		list.setRotateHeaderTitles(true);
		list.setHeaderHeight(100);
		dataSource = new RightsDS(document.getId(), "document");
		list.setDataSource(dataSource);

		List<ListGridField> fields = new ArrayList<>();
		fields.add(entityId);
		fields.add(entity);
		fields.add(read);
		fields.add(print);
		fields.add(download);
		fields.add(email);
		fields.add(write);
		fields.add(rename);
		fields.add(delete);
		fields.add(move);
		fields.add(security);
		fields.add(immutable);
		fields.add(password);
		addSign(fields);
		addArchive(fields);
		addWorkflow(fields);
		addCalendar(fields);
		addSubscription(fields);
		addAutomation(fields);
		addReadingReq(fields);

		list.setFields(fields.toArray(new ListGridField[0]));

		container.addMember(list);

		addCellContextClickHandler(document);

		addButtons(document);
	}

	private void addReadingReq(List<ListGridField> fields) {
		if (Feature.enabled(Feature.READING_CONFIRMATION)) {
			ListGridField readingreq = new ListGridField(READINGREQ, prepareHeaderLabel(READINGREQ));
			readingreq.setType(ListGridFieldType.BOOLEAN);
			readingreq.setCanEdit(true);
			readingreq.setAutoFitWidth(true);
			fields.add(readingreq);
		}
	}

	private void addAutomation(List<ListGridField> fields) {
		if (Feature.enabled(Feature.AUTOMATION)) {
			ListGridField automation = new ListGridField(AUTOMATION, prepareHeaderLabel(AUTOMATION));
			automation.setType(ListGridFieldType.BOOLEAN);
			automation.setCanEdit(true);
			automation.setAutoFitWidth(true);
			fields.add(automation);
		}
	}

	private void addSubscription(List<ListGridField> fields) {
		if (Feature.enabled(Feature.AUDIT)) {
			ListGridField subscription = new ListGridField(SUBSCRIPTION, prepareHeaderLabel(SUBSCRIPTION));
			subscription.setType(ListGridFieldType.BOOLEAN);
			subscription.setCanEdit(true);
			subscription.setAutoFitWidth(true);
			fields.add(subscription);
		}
	}

	private void addCalendar(List<ListGridField> fields) {
		if (Feature.enabled(Feature.CALENDAR)) {
			ListGridField calendar = new ListGridField(CALENDAR, prepareHeaderLabel(CALENDAR));
			calendar.setType(ListGridFieldType.BOOLEAN);
			calendar.setCanEdit(true);
			fields.add(calendar);
		}
	}

	private void addWorkflow(List<ListGridField> fields) {
		if (Feature.enabled(Feature.WORKFLOW)) {
			ListGridField workflow = new ListGridField(WORKFLOW, prepareHeaderLabel(WORKFLOW));
			workflow.setType(ListGridFieldType.BOOLEAN);
			workflow.setCanEdit(true);
			fields.add(workflow);
		}
	}

	private void addArchive(List<ListGridField> fields) {
		if (Feature.enabled(Feature.ARCHIVING) || Feature.enabled(Feature.IMPEX)) {
			ListGridField archive = new ListGridField(ARCHIVE, prepareHeaderLabel(ARCHIVE));
			archive.setType(ListGridFieldType.BOOLEAN);
			archive.setCanEdit(true);
			fields.add(archive);
		}
	}

	private void addSign(List<ListGridField> fields) {
		if (Feature.enabled(Feature.DIGITAL_SIGNATURE)) {
			ListGridField sign = new ListGridField("sign", prepareHeaderLabel("sign"));
			sign.setType(ListGridFieldType.BOOLEAN);
			sign.setCanEdit(true);
			fields.add(sign);
		}
	}

	private void addButtons(GUIDocument document) {
		HLayout buttons = new HLayout();
		buttons.setMembersMargin(4);
		buttons.setWidth100();
		buttons.setHeight(20);
		container.addMember(buttons);

		Button save = new Button(I18N.message("save"));
		save.setAutoFit(true);
		save.addClickHandler(click -> onSave());
		buttons.addMember(save);
		
		Button copyParentFolderSecurity = new Button(I18N.message("copyfoldersecurity"));
		copyParentFolderSecurity.setAutoFit(true);
		copyParentFolderSecurity.addClickHandler(click -> onCopyParentFolderSecurity());
		buttons.addMember(copyParentFolderSecurity);
		
		addGroupSelector(buttons);

		addUserSelector(buttons);

		addExportAndPrintButtons(buttons);
	}

	private void addUserSelector(HLayout buttons) {
		final DynamicForm userForm = new DynamicForm();
		final SelectItem user = ItemFactory.newUserSelector("user", "adduser", null, true, false);
		userForm.setItems(user);

		user.addChangedHandler(userChangedEvent -> {
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
			rec.setAttribute("read", true);

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
			rec.setAttribute("read", true);
			list.addData(rec);
			group.clearValue();
		});
		buttons.addMember(groupForm);
	}

	private void addExportAndPrintButtons(HLayout buttons) {
		Button exportButton = new Button(I18N.message(EXPORT));
		exportButton.setAutoFit(true);
		buttons.addMember(exportButton);
		exportButton.addClickHandler((ClickEvent exportClick) -> GridUtil.exportCSV(list, true));

		Button printButton = new Button(I18N.message(PRINT));
		printButton.setAutoFit(true);
		buttons.addMember(printButton);
		printButton.addClickHandler((ClickEvent printClick) -> GridUtil.print(list));
	}

	private void addCellContextClickHandler(GUIDocument document) {
		if (document != null && document.hasPermission(Constants.PERMISSION_SECURITY)) {
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
	 * Creates an array of all the rights
	 * 
	 * @return the array of rights
	 */
	public GUIRight[] getRights() {
		int totalRecords = list.getRecordList().getLength();
		List<GUIRight> tmp = new ArrayList<>();

		for (int i = 0; i < totalRecords; i++) {
			Record rec = list.getRecordList().get(i);
			GUIRight right = new GUIRight();
			right.setName(rec.getAttributeAsString(ENTITY));
			right.setEntityId(Long.parseLong(rec.getAttribute(ENTITY_ID)));
			right.setRead(rec.getAttributeAsBoolean("read"));
			right.setPrint(rec.getAttributeAsBoolean(PRINT));
			right.setWrite(rec.getAttributeAsBoolean(WRITE));
			right.setDelete(rec.getAttributeAsBoolean(DELETE));
			right.setWorkflow(rec.getAttributeAsBoolean(WORKFLOW));
			right.setSign(rec.getAttributeAsBoolean("sign"));
			right.setImmutable(rec.getAttributeAsBoolean(IMMUTABLE));
			right.setRename(rec.getAttributeAsBoolean(RENAME));
			right.setSecurity(rec.getAttributeAsBoolean(SECURITY));
			right.setArchive(rec.getAttributeAsBoolean(ARCHIVE));
			right.setDownload(rec.getAttributeAsBoolean(DOWNLOAD));
			right.setCalendar(rec.getAttributeAsBoolean(CALENDAR));
			right.setSubscription(rec.getAttributeAsBoolean(SUBSCRIPTION));
			right.setPassword(rec.getAttributeAsBoolean(PASSWORD));
			right.setMove(rec.getAttributeAsBoolean("move"));
			right.setEmail(rec.getAttributeAsBoolean(EMAIL));
			right.setAutomation(rec.getAttributeAsBoolean(AUTOMATION));
			right.setReadingreq(rec.getAttributeAsBoolean(READINGREQ));

			tmp.add(right);
		}

		return tmp.toArray(new GUIRight[0]);
	}

	@Override
	public void destroy() {
		super.destroy();
		if (dataSource != null)
			dataSource.destroy();
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
		deleteItem.addClickHandler((MenuItemClickEvent event) -> onDeleteItem());

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

	public void onSave() {
		// Apply all rights
		document.setRights(this.getRights());

		DocumentService.Instance.get().applySecurity(document, new AsyncCallback<Void>() {

			@Override
			public void onFailure(Throwable caught) {
				GuiLog.serverError(caught);
			}

			@Override
			public void onSuccess(Void result) {
				GuiLog.info(I18N.message("appliedrightsondoc"), null);
				refresh(document);
			}
		});
	}

	public void onCopyParentFolderSecurity() {
		LD.ask(I18N.message("question"), I18N.message("confirmdelete"), choice -> {
			if (Boolean.TRUE.equals(choice)) {
				DocumentService.Instance.get().applyParentFolderSecurity(document.getId(), new AsyncCallback<Void>() {

					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(Void result) {
						GuiLog.info(I18N.message("appliedrightsondoc"), null);
						refresh(document);
					}
				});
			}
		});

	}
}