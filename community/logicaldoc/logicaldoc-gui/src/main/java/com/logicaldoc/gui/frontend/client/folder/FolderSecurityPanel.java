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
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.util.Util;
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
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.HTMLPane;
import com.smartgwt.client.widgets.Img;
import com.smartgwt.client.widgets.Label;
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
			FolderService.Instance.get().getFolder(folder.getSecurityRef().getId(), true, false, false,
					new AsyncCallback<GUIFolder>() {

						@Override
						public void onFailure(Throwable caught) {
							Log.serverError(caught);
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

		ListGridField entityId = new ListGridField("entityId", "entityId", 50);
		entityId.setCanEdit(false);
		entityId.setHidden(true);
		entityId.setAutoFitWidth(true);

		ListGridField entity = new ListGridField("entity", I18N.message("entity"), 180);
		entity.setCanEdit(false);
		entity.setAutoFitWidth(true);

		ListGridField read = new ListGridField("read", I18N.message("read"), 60);
		read.setType(ListGridFieldType.BOOLEAN);
		read.setCanEdit(true);
		read.setAutoFitWidth(true);

		ListGridField print = new ListGridField("print", I18N.message("print"), 60);
		print.setType(ListGridFieldType.BOOLEAN);
		print.setCanEdit(true);
		print.setAutoFitWidth(true);

		ListGridField download = new ListGridField("download", I18N.message("download"), 60);
		download.setType(ListGridFieldType.BOOLEAN);
		download.setCanEdit(true);
		download.setAutoFitWidth(true);

		ListGridField write = new ListGridField("write", I18N.message("write"), 60);
		write.setType(ListGridFieldType.BOOLEAN);
		write.setCanEdit(true);
		write.setAutoFitWidth(true);

		ListGridField add = new ListGridField("add", I18N.message("addfolder"), 60);
		add.setType(ListGridFieldType.BOOLEAN);
		add.setCanEdit(true);
		add.setAutoFitWidth(true);

		ListGridField security = new ListGridField("security", I18N.message("security"), 60);
		security.setType(ListGridFieldType.BOOLEAN);
		security.setCanEdit(true);
		security.setAutoFitWidth(true);

		ListGridField immutable = new ListGridField("immutable", I18N.message("immutable"), 60);
		immutable.setType(ListGridFieldType.BOOLEAN);
		immutable.setCanEdit(true);
		immutable.setAutoFitWidth(true);

		ListGridField delete = new ListGridField("delete", I18N.message("ddelete"), 60);
		delete.setType(ListGridFieldType.BOOLEAN);
		delete.setCanEdit(true);
		delete.setAutoFitWidth(true);

		ListGridField rename = new ListGridField("rename", I18N.message("rename"), 60);
		rename.setType(ListGridFieldType.BOOLEAN);
		rename.setCanEdit(true);
		rename.setAutoFitWidth(true);

		ListGridField _import = new ListGridField("import", I18N.message("iimport"), 60);
		_import.setType(ListGridFieldType.BOOLEAN);
		_import.setCanEdit(true);
		_import.setAutoFitWidth(true);

		ListGridField export = new ListGridField("export", I18N.message("eexport"), 60);
		export.setType(ListGridFieldType.BOOLEAN);
		export.setCanEdit(true);
		export.setAutoFitWidth(true);

		ListGridField sign = new ListGridField("sign", I18N.message("sign"), 60);
		sign.setType(ListGridFieldType.BOOLEAN);
		sign.setCanEdit(true);
		sign.setAutoFitWidth(true);

		ListGridField archive = new ListGridField("archive", I18N.message("archive"), 60);
		archive.setType(ListGridFieldType.BOOLEAN);
		archive.setCanEdit(true);
		archive.setAutoFitWidth(true);

		ListGridField workflow = new ListGridField("workflow", I18N.message("workflow"), 60);
		workflow.setType(ListGridFieldType.BOOLEAN);
		workflow.setCanEdit(true);
		workflow.setAutoFitWidth(true);

		ListGridField calendar = new ListGridField("calendar", I18N.message("calendar"), 60);
		calendar.setType(ListGridFieldType.BOOLEAN);
		calendar.setCanEdit(true);
		calendar.setAutoFitWidth(true);

		ListGridField subscription = new ListGridField("subscription", I18N.message("subscription"), 60);
		subscription.setType(ListGridFieldType.BOOLEAN);
		subscription.setCanEdit(true);
		subscription.setAutoFitWidth(true);

		ListGridField password = new ListGridField("password", I18N.message("password"), 60);
		password.setType(ListGridFieldType.BOOLEAN);
		password.setCanEdit(true);
		password.setAutoFitWidth(true);

		ListGridField move = new ListGridField("move", I18N.message("move"), 60);
		move.setType(ListGridFieldType.BOOLEAN);
		move.setCanEdit(true);
		move.setAutoFitWidth(true);

		ListGridField email = new ListGridField("email", I18N.message("email"), 60);
		email.setType(ListGridFieldType.BOOLEAN);
		email.setCanEdit(true);
		email.setAutoFitWidth(true);

		ListGridField automation = new ListGridField("automation", I18N.message("automation"), 60);
		automation.setType(ListGridFieldType.BOOLEAN);
		automation.setCanEdit(true);
		automation.setAutoFitWidth(true);

		list = new ListGrid();
		list.setEmptyMessage(I18N.message("notitemstoshow"));
		list.setCanFreezeFields(true);
		list.setSelectionType(SelectionStyle.MULTIPLE);
		list.setAutoFetchData(true);
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
		if (Feature.enabled(Feature.DIGITAL_SIGNATURE))
			fields.add(sign);
		if (Feature.enabled(Feature.IMPEX))
			fields.add(archive);
		if (Feature.enabled(Feature.WORKFLOW))
			fields.add(workflow);
		if (Feature.enabled(Feature.CALENDAR))
			fields.add(calendar);
		if (Feature.enabled(Feature.AUDIT))
			fields.add(subscription);
		if (Feature.enabled(Feature.AUTOMATION))
			fields.add(automation);

		list.setFields(fields.toArray(new ListGridField[0]));

		container.addMember(list);

		if (folder != null && folder.hasPermission(Constants.PERMISSION_SECURITY)) {
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
		}

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

		applyRights.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				onSave(false);
			}
		});

		applyRightsSubfolders.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				onSave(true);
			}
		});

		Button inheritRights = new Button(I18N.message("inheritrights"));
		inheritRights.setAutoFit(true);
		buttons.addMember(inheritRights);
		inheritRights.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				InheritRightsDialog dialog = new InheritRightsDialog(FolderSecurityPanel.this);
				dialog.show();
			}
		});

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
		final SelectItem user = ItemFactory.newUserSelector("user", "adduser", null, true);
		userForm.setItems(user);

		user.addChangedHandler(new ChangedHandler() {
			@Override
			public void onChanged(ChangedEvent event) {
				ListGridRecord selectedRecord = user.getSelectedRecord();
				if (selectedRecord == null)
					return;

				/*
				 * Check if the selected user is already present in the rights
				 * table
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
				record.setAttribute("entity", I18N.message("user") + ": " + selectedRecord.getAttribute("label") + " ("
						+ selectedRecord.getAttribute("username") + ")");
				record.setAttribute("read", true);
				list.addData(record);
				user.clearValue();
			}
		});
		buttons.addMember(userForm);

		Button exportButton = new Button(I18N.message("export"));
		exportButton.setAutoFit(true);
		buttons.addMember(exportButton);
		exportButton.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				Util.exportCSV(list, true);
			}
		});

		Button printButton = new Button(I18N.message("print"));
		printButton.setAutoFit(true);
		buttons.addMember(printButton);
		printButton.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				Canvas.printComponents(new Object[] { list });
			}
		});
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
				Log.serverError(caught);
			}

			@Override
			public void onSuccess(Void result) {
				if (!recursive)
					Log.info(I18N.message("appliedrights"), null);
				else
					Log.info(I18N.message("appliedrightsonsubfolders"), null);

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