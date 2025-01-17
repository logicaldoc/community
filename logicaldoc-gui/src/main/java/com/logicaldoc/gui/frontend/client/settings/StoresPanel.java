package com.logicaldoc.gui.frontend.client.settings;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIParameter;
import com.logicaldoc.gui.common.client.data.StoresDS;
import com.logicaldoc.gui.common.client.grid.RefreshableListGrid;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.frontend.client.services.SettingService;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.ListGridFieldType;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * This class shows the storesGrid list and informations.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.1
 */
public class StoresPanel extends VLayout {

	private static final String STORE = "store.";

	private static final String DATABASE_EDIT = "database_edit";

	private static final String VALUE = "value";

	private static final String WRITE = "write";

	public static final int OPERATION_NONE = 0;

	public static final int OPERATION_ADD = 1;

	public static final int OPERATION_CUMPUTESIZE = 2;

	private RefreshableListGrid storesGrid;

	public StoresPanel() {
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

		prepareStoresGrid();

		setMembers(toolStrip, storesGrid);

		refresh();
	}

	private void addRefreshButton(ToolStrip toolStrip) {
		ToolStripButton refresh = new ToolStripButton();
		refresh.setTitle(I18N.message("refresh"));
		refresh.addClickHandler(event -> refresh());
		toolStrip.addButton(refresh);
	}

	private void addSaveButton(ToolStrip toolStrip) {
		ToolStripButton save = new ToolStripButton();
		save.setTitle(I18N.message("save"));
		save.addClickHandler(event -> onSave(true));
		save.setDisabled(Session.get().isDemo() || Session.get().getUser().getId() != 1);
		toolStrip.addButton(save);
	}

	private void addAddButton(ToolStrip toolStrip) {
		ToolStripButton add = new ToolStripButton();
		add.setTitle(I18N.message("addstore"));
		add.addClickHandler(event -> onAddStore());
		add.setDisabled(Session.get().isDemo());
		if (Feature.visible(Feature.MULTI_STORE)) {
			toolStrip.addButton(add);
			toolStrip.addSeparator();
			if (!Feature.enabled(Feature.MULTI_STORE)) {
				add.setDisabled(true);
				add.setTooltip(I18N.message("featuredisabled"));
			}
		}
	}

	private void prepareStoresGrid() {
		storesGrid = new RefreshableListGrid() {

			@Override
			protected Canvas getExpansionComponent(final ListGridRecord rec) {
				return buildExpansionComponent(rec);
			}
		};
		storesGrid.setEmptyMessage(I18N.message("notitemstoshow"));
		storesGrid.setSelectionType(SelectionStyle.SINGLE);
		storesGrid.setCanExpandRecords(true);

		ListGridField id = new ListGridField("id", " ", 20);
		ListGridField name = new ListGridField("name", I18N.message("store"), 150);
		ListGridField path = new ListGridField("path", I18N.message("path"));
		path.setWidth("100%");
		path.setCanEdit(true);

		ListGridField type = prepareTypeField();

		ListGridField write = new ListGridField(WRITE, " ", 20);
		write.setType(ListGridFieldType.IMAGE);
		write.setCanSort(false);
		write.setAlign(Alignment.CENTER);
		write.setShowDefaultContextMenu(false);
		write.setImageURLPrefix(Util.imagePrefix());
		write.setImageURLSuffix(".png");
		write.setCanFilter(false);

		storesGrid.setFields(id, write, name, type, path);
		storesGrid.setAutoFetchData(true);
		storesGrid.addCellContextClickHandler(event -> {
			showContextMenu();
			event.cancel();
		});
		storesGrid.setEditorCustomizer(context -> {
			ListGridField field = context.getEditField();
			if (field.getName().equals("type")) {
				SelectItem item = ItemFactory.newStoreTypeSelector();
				item.addChangedHandler(event -> {
					storesGrid.getSelectedRecord().setAttribute("type", event.getValue().toString());
					storesGrid.collapseRecord(storesGrid.getSelectedRecord());
				});
				return item;
			} else
				return context.getDefaultProperties();
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
		ListGridField value = new ListGridField(VALUE, I18N.message(VALUE));
		value.setWidth("*");
		value.setCanEdit(true);
		parametersGrid.setFields(name, value);

		parametersGrid.addCellSavedHandler(event -> {
			ListGridRecord paramRecord = event.getRecord();
			rec.setAttribute(paramRecord.getAttributeAsString("name"),
					event.getNewValue() != null ? event.getNewValue().toString() : "");
		});

		String[] attrs = rec.getAttributes();
		if (attrs != null && attrs.length > 0) {
			List<ListGridRecord> records = new ArrayList<>();
			for (String attr : attrs) {
				if (!StoresPanel.isParameterAttribute(attr))
					continue;
				ListGridRecord recd = new ListGridRecord();
				recd.setAttribute("name", attr);
				recd.setAttribute(VALUE, rec.getAttributeAsString(attr));
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
			String label = I18N.message(STORE + value);
			if (label.equals(STORE + value))
				return value.toString();
			else
				return label;
		});
		return type;
	}

	private static boolean isParameterAttribute(String name) {
		return !("type".equals(name) || "id".equals(name) || "name".equals(name) || "path".equals(name)
				|| WRITE.equals(name) || name.startsWith("_"));
	}

	private void refresh() {
		storesGrid.refresh(new StoresDS(false, true));
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
		ListGridRecord selectedRecord = storesGrid.getSelectedRecord();
		int selectedStoreId = selectedRecord.getAttributeAsInt("id");

		MenuItem delete = new MenuItem();
		delete.setTitle(I18N.message("ddelete"));
		delete.addClickHandler(event -> LD.ask(I18N.message("question"), I18N.message("confirmdelete"), yes -> {
			if (Boolean.TRUE.equals(yes)) {
				doRemoveStore(selectedStoreId);
			}
		}));
		delete.setEnabled(!Session.get().isDemo() && !DATABASE_EDIT.equals(selectedRecord.getAttributeAsString(WRITE)));
		return delete;
	}

	private void doRemoveStore(int selectedStoreId) {
		SettingService.Instance.get().removeStore(selectedStoreId, new DefaultAsyncCallback<>() {
			@Override
			public void onSuccess(List<String> paths) {
				if (!paths.isEmpty()) {
					SC.warn(I18N.message("foldersusingstorage", "" + selectedStoreId) + Util.padLeft("<ul>"
							+ paths.stream().map(p -> "<li>" + p + "</li>").collect(Collectors.joining()) + "</ul>",
							1000));
				} else {
					refresh();
				}
			}
		});
	}

	private MenuItem prepareTestMenuItem() {
		ListGridRecord selectedRecord = storesGrid.getSelectedRecord();

		MenuItem test = new MenuItem();
		test.setTitle(I18N.message("testconnection"));
		test.addClickHandler(event -> SettingService.Instance.get().testStore(selectedRecord.getAttributeAsInt("id"),
				new DefaultAsyncCallback<>() {
					@Override
					public void onSuccess(Boolean result) {
						if (result.booleanValue())
							SC.say(I18N.message("connectionestablished"));
						else
							SC.warn(I18N.message("connectionfailed"));
					}
				}));
		test.setEnabled(!Session.get().isDemo());
		return test;
	}

	private MenuItem prepareMakeWriteMenuItem() {
		MenuItem makeWrite = new MenuItem();
		makeWrite.setTitle(I18N.message("makedefwritestore"));
		makeWrite.addClickHandler(event -> {
			ListGridRecord[] recs = storesGrid.getRecords();
			for (ListGridRecord rec : recs) {
				rec.setAttribute(WRITE, "blank");
				storesGrid.refreshRow(storesGrid.getRowNum(rec));
			}
			ListGridRecord selectedRecord = storesGrid.getSelectedRecord();
			selectedRecord.setAttribute(WRITE, DATABASE_EDIT);
			storesGrid.refreshRow(storesGrid.getRowNum(storesGrid.getSelectedRecord()));
		});
		makeWrite.setEnabled(!Session.get().isDemo());
		return makeWrite;
	}

	private void onSave(boolean alertInclusion) {
		List<GUIParameter> settings = collectSettings();

		SettingService.Instance.get().saveStoreSettings(settings, new DefaultAsyncCallback<>() {
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
		List<GUIParameter> settings = new ArrayList<>();
		ListGridRecord[] records = storesGrid.getRecords();
		for (ListGridRecord storeRecord : records) {
			try {
				String storeId = storeRecord.getAttributeAsString("id").trim();
				settings.add(
						new GUIParameter(STORE + storeId + ".dir", storeRecord.getAttributeAsString("path").trim()));
				settings.add(
						new GUIParameter(STORE + storeId + ".type", storeRecord.getAttributeAsString("type").trim()));
				if (DATABASE_EDIT.equals(storeRecord.getAttributeAsString(WRITE))) {
					settings.add(new GUIParameter("store.write", storeId));
				}

				collectAttribute(settings, storeRecord);
			} catch (Exception t) {
				/*
				 * the extensions table is lazy loaded so we may have null
				 * pointers here, in this case just skip
				 */
			}
		}
		return settings;
	}

	private void collectAttribute(List<GUIParameter> settings, ListGridRecord storeRecord) {
		String[] attrs = storeRecord.getAttributes();
		if (attrs != null && attrs.length > 0) {
			try {
				for (String attr : attrs) {
					if (!StoresPanel.isParameterAttribute(attr))
						continue;
					String storeId = storeRecord.getAttributeAsString("id").trim();
					settings.add(new GUIParameter(STORE + storeId + "." + attr,
							storeRecord.getAttributeAsString(attr).trim()));
				}
			} catch (Exception t) {
				/*
				 * the extensions table is lazy loaded so we may have null
				 * pointers here, in this case just skip
				 */
			}
		}
	}

	private void onAddStore() {
		for (int i = 1; i < 99; i++) {
			Record rec = storesGrid.getRecordList().find("id", Integer.toString(i));
			if (rec == null) {
				ListGridRecord newStore = new ListGridRecord();
				newStore.setAttribute("id", Integer.toString(i));
				newStore.setAttribute("name", "Store " + i);
				newStore.setAttribute("type", "fs");
				newStore.setAttribute("encryption", "false");
				newStore.setAttribute("compression", "5");
				newStore.setAttribute(WRITE, "blank");

				storesGrid.getDataSource().addData(newStore);
				storesGrid.redraw();
				break;
			}
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