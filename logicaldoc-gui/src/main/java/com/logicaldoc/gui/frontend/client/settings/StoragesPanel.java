package com.logicaldoc.gui.frontend.client.settings;

import java.util.ArrayList;
import java.util.List;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIParameter;
import com.logicaldoc.gui.common.client.data.StoragesDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.widgets.grid.RefreshableListGrid;
import com.logicaldoc.gui.frontend.client.services.SettingService;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.ListGridFieldType;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridEditorContext;
import com.smartgwt.client.widgets.grid.ListGridEditorCustomizer;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.CellContextClickEvent;
import com.smartgwt.client.widgets.grid.events.CellContextClickHandler;
import com.smartgwt.client.widgets.grid.events.CellSavedEvent;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.events.MenuItemClickEvent;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * This class shows the storagesGrid list and informations.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.1
 */
public class StoragesPanel extends VLayout {

	public static final int OPERATION_NONE = 0;

	public static final int OPERATION_ADD = 1;

	public static final int OPERATION_CUMPUTESIZE = 2;

	private RefreshableListGrid storagesGrid;

	public StoragesPanel() {
		setWidth100();
		setHeight100();
		setMembersMargin(5);
		setMargin(5);
	}

	@Override
	public void onDraw() {
		ToolStrip toolStrip = new ToolStrip();
		toolStrip.setHeight(20);
		toolStrip.setWidth100();
		toolStrip.addSpacer(2);

		addAddButton(toolStrip);

		addSaveButton(toolStrip);

		toolStrip.addSeparator();

		addRefreshButton(toolStrip);

		toolStrip.addFill();

		prepareStoragesGrid();

		setMembers(toolStrip, storagesGrid);

		refresh();
	}

	private void addRefreshButton(ToolStrip toolStrip) {
		ToolStripButton refresh = new ToolStripButton();
		refresh.setTitle(I18N.message("refresh"));
		refresh.addClickHandler((ClickEvent event) -> {
			refresh();
		});
		toolStrip.addButton(refresh);
	}

	private void addSaveButton(ToolStrip toolStrip) {
		ToolStripButton save = new ToolStripButton();
		save.setTitle(I18N.message("save"));
		save.addClickHandler((ClickEvent event) -> {
			onSave(true);
		});
		save.setDisabled(Session.get().isDemo() || Session.get().getUser().getId() != 1);
		toolStrip.addButton(save);
	}

	private void addAddButton(ToolStrip toolStrip) {
		ToolStripButton add = new ToolStripButton();
		add.setTitle(I18N.message("addstore"));
		add.addClickHandler((ClickEvent event) -> {
			onAddStorage();
		});
		add.setDisabled(Session.get().isDemo());
		if (Feature.visible(Feature.MULTI_STORAGE)) {
			toolStrip.addButton(add);
			toolStrip.addSeparator();
			if (!Feature.enabled(Feature.MULTI_STORAGE)) {
				add.setDisabled(true);
				add.setTooltip(I18N.message("featuredisabled"));
			}
		}
	}

	private void prepareStoragesGrid() {
		storagesGrid = new RefreshableListGrid() {

			@Override
			protected Canvas getExpansionComponent(final ListGridRecord rec) {
				return buildExpansionComponent(rec);
			}
		};
		storagesGrid.setEmptyMessage(I18N.message("notitemstoshow"));
		storagesGrid.setSelectionType(SelectionStyle.SINGLE);
		storagesGrid.setCanExpandRecords(true);

		ListGridField id = new ListGridField("id", " ", 20);
		ListGridField name = new ListGridField("name", I18N.message("name"), 150);
		ListGridField path = new ListGridField("path", I18N.message("path"));
		path.setWidth("100%");
		path.setCanEdit(true);

		ListGridField type = prepareTypeField();

		ListGridField write = new ListGridField("write", " ", 20);
		write.setType(ListGridFieldType.IMAGE);
		write.setCanSort(false);
		write.setAlign(Alignment.CENTER);
		write.setShowDefaultContextMenu(false);
		write.setImageURLPrefix(Util.imagePrefix());
		write.setImageURLSuffix(".png");
		write.setCanFilter(false);

		storagesGrid.setFields(id, write, name, type, path);
		storagesGrid.setAutoFetchData(true);
		storagesGrid.addCellContextClickHandler(new CellContextClickHandler() {
			@Override
			public void onCellContextClick(CellContextClickEvent event) {
				showContextMenu();
				event.cancel();
			}
		});
		storagesGrid.setEditorCustomizer(new ListGridEditorCustomizer() {
			public FormItem getEditor(ListGridEditorContext context) {
				ListGridField field = context.getEditField();
				if (field.getName().equals("type")) {
					SelectItem item = ItemFactory.newStorageTypeSelector();
					item.addChangedHandler((ChangedEvent event) -> {
						storagesGrid.getSelectedRecord().setAttribute("type", event.getValue().toString());
						storagesGrid.collapseRecord(storagesGrid.getSelectedRecord());
//							onSave(false);
					});
					return item;
				} else
					return context.getDefaultProperties();
			}
		});
	}

	private Canvas buildExpansionComponent(final ListGridRecord rec) {
		VLayout layout = new VLayout(5);
		layout.setPadding(5);

		final ListGrid parametersGrid = new ListGrid();
		parametersGrid.setHeight(150);
		parametersGrid.setCanEdit(true);
		parametersGrid.setModalEditing(true);
		parametersGrid.setAutoSaveEdits(true);
		parametersGrid.setAutoFetchData(true);

		ListGridField name = new ListGridField("name", I18N.message("parameter"), 150);
		name.setCanEdit(false);
		ListGridField value = new ListGridField("value", I18N.message("value"));
		value.setWidth("*");
		value.setCanEdit(true);
		parametersGrid.setFields(name, value);

		parametersGrid.addCellSavedHandler((CellSavedEvent event) -> {
			ListGridRecord paramRecord = event.getRecord();
			rec.setAttribute(paramRecord.getAttributeAsString("name"),
					event.getNewValue() != null ? event.getNewValue().toString() : "");
		});

		String[] attrs = rec.getAttributes();
		if (attrs != null && attrs.length > 0) {
			List<ListGridRecord> records = new ArrayList<ListGridRecord>();
			for (String attr : attrs) {
				if (!StoragesPanel.isParameterAttribute(attr))
					continue;
				ListGridRecord recd = new ListGridRecord();
				recd.setAttribute("name", attr);
				recd.setAttribute("value", rec.getAttributeAsString(attr));
				records.add(recd);
			}
			parametersGrid.setRecords(records.toArray(new ListGridRecord[0]));
		}

		layout.addMember(parametersGrid);
		return layout;
	}

	private ListGridField prepareTypeField() {
		ListGridField type = new ListGridField("type", I18N.message("type"), 150);
		type.setCanEdit(true);
		type.setCellFormatter((Object value, ListGridRecord rec, int rowNum, int colNum) -> {
			if (value == null)
				return "";
			String label = I18N.message("storer." + value);
			if (label.equals("storer." + value))
				return value.toString();
			else
				return label;
		});
		return type;
	}

	private static boolean isParameterAttribute(String name) {
		if ("type".equals(name) || "id".equals(name) || "name".equals(name) || "path".equals(name)
				|| "write".equals(name) || name.startsWith("_"))
			return false;
		else
			return true;
	}

	private void refresh() {
		storagesGrid.refresh(new StoragesDS(false, true));
	}

	/**
	 * Prepares the context menu
	 */
	private void showContextMenu() {
		MenuItem makeWrite = prepareMakeWriteMenuItem();

		MenuItem test = prepareTestMenuItem();

		MenuItem delete = prepareDeleteMenuItem();

		Menu contextMenu = new Menu();
		contextMenu.setItems(makeWrite, test, delete);
		contextMenu.showContextMenu();
	}

	private MenuItem prepareDeleteMenuItem() {
		ListGridRecord selectedRecord = storagesGrid.getSelectedRecord();
		int selectedStorageId = selectedRecord.getAttributeAsInt("id");

		MenuItem delete = new MenuItem();
		delete.setTitle(I18N.message("ddelete"));
		delete.addClickHandler((MenuItemClickEvent event) -> {
			LD.ask(I18N.message("question"), I18N.message("confirmdelete"), (Boolean yes) -> {
				if (yes) {
					doRemoveStorage(selectedStorageId);
				}
			});
		});
		delete.setEnabled(
				!Session.get().isDemo() && !"database_edit".equals(selectedRecord.getAttributeAsString("write")));
		return delete;
	}

	private void doRemoveStorage(int selectedStorageId) {
		SettingService.Instance.get().removeStorage(selectedStorageId, new AsyncCallback<String[]>() {

			@Override
			public void onFailure(Throwable caught) {
				GuiLog.serverError(caught);
			}

			@Override
			public void onSuccess(String[] paths) {
				if (paths != null && paths.length > 0) {
					StringBuilder report = new StringBuilder("<ul>");
					for (String path : paths) {
						report.append("<li>");
						report.append(path);
						report.append("</li>");
					}
					report.append("</ul>");

					SC.warn(I18N.message("foldersusingstorage", "" + selectedStorageId)
							+ Util.padLeft(report.toString(), 1000));
				} else {
					refresh();
				}
			}
		});
	}

	private MenuItem prepareTestMenuItem() {
		ListGridRecord selectedRecord = storagesGrid.getSelectedRecord();

		MenuItem test = new MenuItem();
		test.setTitle(I18N.message("testconnection"));
		test.addClickHandler((MenuItemClickEvent event) -> {
			SettingService.Instance.get().testStorage(selectedRecord.getAttributeAsInt("id"),
					new AsyncCallback<Boolean>() {

						@Override
						public void onFailure(Throwable caught) {
							GuiLog.serverError(caught);
						}

						@Override
						public void onSuccess(Boolean result) {
							if (result.booleanValue())
								SC.say(I18N.message("connectionestablished"));
							else
								SC.warn(I18N.message("connectionfailed"));
						}
					});
		});
		test.setEnabled(!Session.get().isDemo());
		return test;
	}

	private MenuItem prepareMakeWriteMenuItem() {
		MenuItem makeWrite = new MenuItem();
		makeWrite.setTitle(I18N.message("makedefwritestore"));
		makeWrite.addClickHandler((MenuItemClickEvent event) -> {
			ListGridRecord[] recs = storagesGrid.getRecords();
			for (ListGridRecord rec : recs) {
				rec.setAttribute("write", "blank");
				storagesGrid.refreshRow(storagesGrid.getRowNum(rec));
			}
			ListGridRecord selectedRecord = storagesGrid.getSelectedRecord();
			selectedRecord.setAttribute("write", "database_edit");
			storagesGrid.refreshRow(storagesGrid.getRowNum(storagesGrid.getSelectedRecord()));
		});
		makeWrite.setEnabled(!Session.get().isDemo());
		return makeWrite;
	}

	private void onSave(boolean alertInclusion) {
		List<GUIParameter> settings = collectSettings();

		SettingService.Instance.get().saveStorageSettings(settings.toArray(new GUIParameter[0]),
				new AsyncCallback<Void>() {

					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(Void arg) {
						GuiLog.info(I18N.message("settingssaved"), null);

						// Replicate the settings in the current session
						for (GUIParameter setting : settings)
							Session.get().setConfig(setting.getName(), setting.getValue());

						refresh();

						if (alertInclusion)
							SC.warn(I18N.message("importantnotice"), I18N.message("makesurenotnestedstorage"));
					}
				});
	}

	private List<GUIParameter> collectSettings() {
		List<GUIParameter> settings = new ArrayList<GUIParameter>();
		ListGridRecord[] records = storagesGrid.getRecords();
		for (ListGridRecord storageRecord : records) {
			try {
				String storageId = storageRecord.getAttributeAsString("id").trim();
				settings.add(new GUIParameter("store." + storageId + ".dir",
						storageRecord.getAttributeAsString("path").trim()));
				settings.add(new GUIParameter("store." + storageId + ".type",
						storageRecord.getAttributeAsString("type").trim()));
				if ("database_edit".equals(storageRecord.getAttributeAsString("write"))) {
					settings.add(new GUIParameter("store.write", storageId));
				}

				collectAttribute(settings, storageRecord);
			} catch (Throwable t) {
				/*
				 * the extensions table is lazy loaded so we may have null
				 * pointers here, in this case just skip
				 */
			}
		}
		return settings;
	}

	private void collectAttribute(List<GUIParameter> settings, ListGridRecord storageRecord) {
		String[] attrs = storageRecord.getAttributes();
		if (attrs != null && attrs.length > 0) {
			try {
				for (String attr : attrs) {
					if (!StoragesPanel.isParameterAttribute(attr))
						continue;
					String storageId = storageRecord.getAttributeAsString("id").trim();
					settings.add(new GUIParameter("store." + storageId + "." + attr,
							storageRecord.getAttributeAsString(attr).trim()));
				}
			} catch (Throwable t) {
				/*
				 * the extensions table is lazy loaded so we may have null
				 * pointers here, in this case just skip
				 */
			}
		}
	}

	private void onAddStorage() {
		for (int i = 1; i < 99; i++) {
			Record rec = storagesGrid.getRecordList().find("id", Integer.toString(i));
			if (rec == null) {
				ListGridRecord newStore = new ListGridRecord();
				newStore.setAttribute("id", Integer.toString(i));
				newStore.setAttribute("name", "Storage " + i);
				newStore.setAttribute("type", "fs");
				newStore.setAttribute("encryption", "false");
				newStore.setAttribute("compression", "5");
				newStore.setAttribute("write", "blank");

				storagesGrid.getDataSource().addData(newStore);
				storagesGrid.redraw();
				break;
			}
		}
	}
}
