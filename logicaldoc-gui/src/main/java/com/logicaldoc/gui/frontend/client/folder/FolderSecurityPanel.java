package com.logicaldoc.gui.frontend.client.folder;

import java.util.ArrayList;
import java.util.List;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.beans.GUIRight;
import com.logicaldoc.gui.common.client.data.RightsDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.GridUtil;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.widgets.grid.UserListGridField;
import com.logicaldoc.gui.frontend.client.services.FolderService;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.Cursor;
import com.smartgwt.client.types.ListGridFieldType;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.types.VerticalAlignment;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.widgets.Button;
import com.smartgwt.client.widgets.HTMLPane;
import com.smartgwt.client.widgets.Img;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
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
 * This panel shows the security policies.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class FolderSecurityPanel extends FolderDetailTab {

	private RightsDS dataSource;

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

	void refresh(GUIFolder folder) {
		super.folder = folder;

		container.removeMembers(container.getMembers());

		if (folder.getSecurityRef() != null) {
			displayInheritingPanel(folder);
		}

		ListGridField entityId = new ListGridField("entityId", "entityId");
		entityId.setCanEdit(false);
		entityId.setHidden(true);
		entityId.setAutoFitWidth(true);

		ListGridField entity = new UserListGridField("entity", "avatar", "entity");
		entity.setCanEdit(false);
		entity.setAutoFitWidth(true);
		entity.setRotateTitle(false);

		ListGridField read = new ListGridField("read", I18N.message("read"));
		read.setType(ListGridFieldType.BOOLEAN);
		read.setCanEdit(true);
		read.setAutoFitWidth(true);

		ListGridField print = new ListGridField("print", I18N.message("print"));
		print.setType(ListGridFieldType.BOOLEAN);
		print.setCanEdit(true);
		print.setAutoFitWidth(true);

		ListGridField download = new ListGridField("download", I18N.message("download"));
		download.setType(ListGridFieldType.BOOLEAN);
		download.setCanEdit(true);
		download.setAutoFitWidth(true);

		ListGridField write = new ListGridField("write", I18N.message("write"));
		write.setType(ListGridFieldType.BOOLEAN);
		write.setCanEdit(true);
		write.setAutoFitWidth(true);

		ListGridField add = new ListGridField("add", I18N.message("addfolder"));
		add.setType(ListGridFieldType.BOOLEAN);
		add.setCanEdit(true);
		add.setAutoFitWidth(true);

		ListGridField security = new ListGridField("security", I18N.message("security"));
		security.setType(ListGridFieldType.BOOLEAN);
		security.setCanEdit(true);
		security.setAutoFitWidth(true);

		ListGridField immutable = new ListGridField("immutable", I18N.message("immutable"));
		immutable.setType(ListGridFieldType.BOOLEAN);
		immutable.setCanEdit(true);
		immutable.setAutoFitWidth(true);

		ListGridField delete = new ListGridField("delete", I18N.message("ddelete"));
		delete.setType(ListGridFieldType.BOOLEAN);
		delete.setCanEdit(true);
		delete.setAutoFitWidth(true);

		ListGridField rename = new ListGridField("rename", I18N.message("rename"));
		rename.setType(ListGridFieldType.BOOLEAN);
		rename.setCanEdit(true);
		rename.setAutoFitWidth(true);

		ListGridField _import = new ListGridField("import", I18N.message("iimport"));
		_import.setType(ListGridFieldType.BOOLEAN);
		_import.setCanEdit(true);
		_import.setAutoFitWidth(true);

		ListGridField export = new ListGridField("export", I18N.message("eexport"));
		export.setType(ListGridFieldType.BOOLEAN);
		export.setCanEdit(true);
		export.setAutoFitWidth(true);

		ListGridField password = new ListGridField("password", I18N.message("password"));
		password.setType(ListGridFieldType.BOOLEAN);
		password.setCanEdit(true);
		password.setAutoFitWidth(true);

		ListGridField move = new ListGridField("move", I18N.message("move"));
		move.setType(ListGridFieldType.BOOLEAN);
		move.setCanEdit(true);
		move.setAutoFitWidth(true);

		ListGridField email = new ListGridField("email", I18N.message("email"));
		email.setType(ListGridFieldType.BOOLEAN);
		email.setCanEdit(true);
		email.setAutoFitWidth(true);

		list = new ListGrid();
		list.setEmptyMessage(I18N.message("notitemstoshow"));
		list.setCanFreezeFields(true);
		list.setSelectionType(SelectionStyle.MULTIPLE);
		list.setAutoFetchData(true);
		list.setRotateHeaderTitles(true);
		list.setHeaderHeight(100);
		dataSource = new RightsDS(folder.getId(), true);
		list.setDataSource(dataSource);

		List<ListGridField> fields = new ArrayList<ListGridField>();
		fields.add(entityId);
		fields.add(entity);
		fields.add(read);
		fields.add(print);
		fields.add(download);
		fields.add(email);
		fields.add(write);
		fields.add(add);
		fields.add(rename);
		fields.add(delete);
		fields.add(move);
		fields.add(security);
		fields.add(immutable);
		fields.add(password);
		fields.add(_import);
		fields.add(export);
		addSign(fields);
		addArchive(fields);
		addWorkflow(fields);
		addCalendar(fields);
		addSubscription(fields);
		addAutomation(fields);
		addStorage(fields);

		list.setFields(fields.toArray(new ListGridField[0]));

		container.addMember(list);

		addCellContextClickHandler(folder);

		addButtons(folder);
	}

	private void displayInheritingPanel(GUIFolder folder) {
		FolderService.Instance.get().getFolder(folder.getSecurityRef().getId(), true, false, false,
				new AsyncCallback<GUIFolder>() {

					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

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
						path.addClickHandler(new ClickHandler() {
							@Override
							public void onClick(ClickEvent event) {
								FolderNavigator.get().openFolder(refFolder.getId());
							}
						});

						HTMLPane spacer = new HTMLPane();
						spacer.setContents("<div>&nbsp;</div>");
						spacer.setWidth("*");
						spacer.setOverflow(Overflow.HIDDEN);

						Img closeImage = ItemFactory.newImgIcon("delete.png");
						closeImage.setHeight("16px");
						closeImage.addClickHandler(new ClickHandler() {
							@Override
							public void onClick(ClickEvent event) {
								inheritInfoPanel.setVisible(false);
							}
						});
						closeImage.setCursor(Cursor.HAND);
						closeImage.setTooltip(I18N.message("close"));
						closeImage.setLayoutAlign(Alignment.RIGHT);
						closeImage.setLayoutAlign(VerticalAlignment.CENTER);

						inheritInfoPanel.setMembers(label, path, spacer, closeImage);
						container.addMember(inheritInfoPanel, 0);
					}
				});
	}

	private void addStorage(List<ListGridField> fields) {
		if (Feature.enabled(Feature.MULTI_STORAGE)) {
			ListGridField storage = new ListGridField("storage", I18N.message("storage"));
			storage.setType(ListGridFieldType.BOOLEAN);
			storage.setCanEdit(true);
			storage.setAutoFitWidth(true);
			fields.add(storage);
		}
	}

	private void addAutomation(List<ListGridField> fields) {
		if (Feature.enabled(Feature.AUTOMATION)) {
			ListGridField automation = new ListGridField("automation", I18N.message("automation"));
			automation.setType(ListGridFieldType.BOOLEAN);
			automation.setCanEdit(true);
			automation.setAutoFitWidth(true);
			fields.add(automation);
		}
	}

	private void addSubscription(List<ListGridField> fields) {
		if (Feature.enabled(Feature.AUDIT)) {
			ListGridField subscription = new ListGridField("subscription", I18N.message("subscription"));
			subscription.setType(ListGridFieldType.BOOLEAN);
			subscription.setCanEdit(true);
			subscription.setAutoFitWidth(true);
			fields.add(subscription);
		}
	}

	private void addButtons(GUIFolder folder) {
		HLayout buttons = new HLayout();
		buttons.setMembersMargin(4);
		buttons.setWidth100();
		buttons.setHeight(20);
		container.addMember(buttons);

		Button applyRights = new Button(I18N.message("applyrights"));
		applyRights.setAutoFit(true);
		buttons.addMember(applyRights);

		Button applyRightsSubfolders = new Button(I18N.message("applytosubfolders"));
		applyRightsSubfolders.setAutoFit(true);
		buttons.addMember(applyRightsSubfolders);

		applyRights.addClickHandler((ClickEvent applyClick) -> {
			onSave(false);
		});

		applyRightsSubfolders.addClickHandler((ClickEvent rightsClick) -> {
			onSave(true);
		});

		Button inheritFromParent = new Button(I18N.message("inheritfromparent"));
		inheritFromParent.setAutoFit(true);
		buttons.addMember(inheritFromParent);
		inheritFromParent.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				FolderService.Instance.get().getFolder(folder.getParentId(), false, false, false,
						new AsyncCallback<GUIFolder>() {

							@Override
							public void onFailure(Throwable caught) {
								GuiLog.serverError(caught);
							}

							@Override
							public void onSuccess(GUIFolder parent) {
								LD.ask(I18N.message("inheritrights"),
										I18N.message("inheritrightsask",
												new String[] { folder.getName(), parent.getName() }),
										(Boolean interitConfirmed) -> {
											if (interitConfirmed) {
												FolderService.Instance.get().inheritRights(folder.getId(),
														folder.getParentId(), new AsyncCallback<GUIFolder>() {

															@Override
															public void onFailure(Throwable caught) {
																GuiLog.serverError(caught);
															}

															@Override
															public void onSuccess(GUIFolder arg) {
																FolderSecurityPanel.this.refresh(arg);
															}
														});
											}
										});
							}

						});
			}
		});

		Button inheritRights = new Button(I18N.message("inheritrights"));
		inheritRights.setAutoFit(true);
		buttons.addMember(inheritRights);
		inheritRights.addClickHandler((ClickEvent InheritClick) -> {
			InheritRightsDialog dialog = new InheritRightsDialog(FolderSecurityPanel.this);
			dialog.show();
		});

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
				if (test.getAttribute("entityId").equals(selectedRecord.getAttribute("usergroup"))) {
					user.clearValue();
					return;
				}
			}

			// Update the rights table
			ListGridRecord record = new ListGridRecord();
			record.setAttribute("entityId", selectedRecord.getAttribute("usergroup"));
			record.setAttribute("avatar", selectedRecord.getAttribute("id"));
			record.setAttribute("entity",
					selectedRecord.getAttribute("label") + " (" + selectedRecord.getAttribute("username") + ")");
			record.setAttribute("read", true);

			list.addData(record);
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
				if (test.getAttribute("entityId").equals(selectedRecord.getAttribute("id"))) {
					group.clearValue();
					return;
				}
			}

			// Update the rights table
			ListGridRecord record = new ListGridRecord();
			record.setAttribute("entityId", selectedRecord.getAttribute("id"));
			record.setAttribute("avatar", "group");
			record.setAttribute("entity", selectedRecord.getAttribute("name"));
			record.setAttribute("read", true);
			list.addData(record);
			group.clearValue();
		});
		buttons.addMember(groupForm);
	}

	private void addExportAndPrintButtons(HLayout buttons) {
		Button exportButton = new Button(I18N.message("export"));
		exportButton.setAutoFit(true);
		buttons.addMember(exportButton);
		exportButton.addClickHandler((ClickEvent exportClick) -> {
			GridUtil.exportCSV(list, true);
		});

		Button printButton = new Button(I18N.message("print"));
		printButton.setAutoFit(true);
		buttons.addMember(printButton);
		printButton.addClickHandler((ClickEvent printClick) -> {
			GridUtil.print(list);
		});
	}

	private void addCellContextClickHandler(GUIFolder folder) {
		if (folder != null && folder.hasPermission(Constants.PERMISSION_SECURITY)) {
			list.addCellContextClickHandler((CellContextClickEvent contextClick) -> {
				if (contextClick.getColNum() == 0) {
					Menu contextMenu = setupContextMenu();
					contextMenu.showContextMenu();
				}
				contextClick.cancel();
			});
		}
	}

	private void addCalendar(List<ListGridField> fields) {
		if (Feature.enabled(Feature.CALENDAR)) {
			ListGridField calendar = new ListGridField("calendar", I18N.message("calendar"));
			calendar.setType(ListGridFieldType.BOOLEAN);
			calendar.setCanEdit(true);
			calendar.setAutoFitWidth(true);
			fields.add(calendar);
		}
	}

	private void addWorkflow(List<ListGridField> fields) {
		if (Feature.enabled(Feature.WORKFLOW)) {
			ListGridField workflow = new ListGridField("workflow", I18N.message("workflow"));
			workflow.setType(ListGridFieldType.BOOLEAN);
			workflow.setCanEdit(true);
			workflow.setAutoFitWidth(true);
			fields.add(workflow);
		}
	}

	private void addArchive(List<ListGridField> fields) {
		if (Feature.enabled(Feature.ARCHIVING) || Feature.enabled(Feature.IMPEX)) {
			ListGridField archive = new ListGridField("archive", I18N.message("archive"));
			archive.setType(ListGridFieldType.BOOLEAN);
			archive.setCanEdit(true);
			archive.setAutoFitWidth(true);
			fields.add(archive);
		}
	}

	private void addSign(List<ListGridField> fields) {
		if (Feature.enabled(Feature.DIGITAL_SIGNATURE)) {
			ListGridField sign = new ListGridField("sign", I18N.message("sign"));
			sign.setType(ListGridFieldType.BOOLEAN);
			sign.setCanEdit(true);
			sign.setAutoFitWidth(true);
			fields.add(sign);
		}
	}

	/**
	 * Creates an array of all the right
	 * 
	 * @return the array of rights
	 */
	public GUIRight[] getRights() {
		int totalRecords = list.getRecordList().getLength();
		List<GUIRight> tmp = new ArrayList<GUIRight>();

		for (int i = 0; i < totalRecords; i++) {
			Record record = list.getRecordList().get(i);
			if (!record.getAttributeAsBoolean("read"))
				continue;

			GUIRight right = new GUIRight();

			right.setName(record.getAttributeAsString("entity"));
			right.setEntityId(Long.parseLong(record.getAttribute("entityId")));
			right.setPrint("true".equals(record.getAttributeAsString("print")));
			right.setWrite("true".equals(record.getAttributeAsString("write")));
			right.setDelete("true".equals(record.getAttributeAsString("delete")));
			right.setAdd("true".equals(record.getAttributeAsString("add")));
			right.setWorkflow("true".equals(record.getAttributeAsString("workflow")));
			right.setSign("true".equals(record.getAttributeAsString("sign")));
			right.setImport("true".equals(record.getAttributeAsString("import")));
			right.setExport("true".equals(record.getAttributeAsString("export")));
			right.setImmutable("true".equals(record.getAttributeAsString("immutable")));
			right.setRename("true".equals(record.getAttributeAsString("rename")));
			right.setSecurity("true".equals(record.getAttributeAsString("security")));
			right.setArchive("true".equals(record.getAttributeAsString("archive")));
			right.setDownload("true".equals(record.getAttributeAsString("download")));
			right.setCalendar("true".equals(record.getAttributeAsString("calendar")));
			right.setSubscription("true".equals(record.getAttributeAsString("subscription")));
			right.setPassword("true".equals(record.getAttributeAsString("password")));
			right.setMove("true".equals(record.getAttributeAsString("move")));
			right.setEmail("true".equals(record.getAttributeAsString("email")));
			right.setAutomation("true".equals(record.getAttributeAsString("automation")));
			right.setStorage("true".equals(record.getAttributeAsString("storage")));

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
		deleteItem.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				onDeleteItem();
			}
		});

		contextMenu.setItems(deleteItem);
		return contextMenu;
	}

	private void onDeleteItem() {
		ListGridRecord[] selection = list.getSelectedRecords();
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

	public void onSave(final boolean recursive) {
		// Apply all rights
		folder.setRights(this.getRights());

		FolderService.Instance.get().applyRights(folder, recursive, new AsyncCallback<Void>() {

			@Override
			public void onFailure(Throwable caught) {
				GuiLog.serverError(caught);
			}

			@Override
			public void onSuccess(Void result) {
				if (!recursive)
					GuiLog.info(I18N.message("appliedrights"), null);
				else
					GuiLog.info(I18N.message("appliedrightsonsubfolders"), null);

				int totalRecords = list.getRecordList().getLength();
				for (int i = 0; i < totalRecords; i++) {
					Record rec = list.getRecordList().get(i);
					if (!rec.getAttributeAsBoolean("read"))
						list.removeData(rec);
				}
				folder.setSecurityRef(null);
				refresh(folder);
			}
		});
	}
}