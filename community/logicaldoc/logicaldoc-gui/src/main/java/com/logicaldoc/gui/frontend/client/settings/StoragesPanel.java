package com.logicaldoc.gui.frontend.client.settings;

import java.util.ArrayList;
import java.util.List;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIParameter;
import com.logicaldoc.gui.common.client.data.StoragesDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.frontend.client.services.SettingService;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.ListGridFieldType;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.grid.CellFormatter;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridEditorContext;
import com.smartgwt.client.widgets.grid.ListGridEditorCustomizer;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.CellContextClickEvent;
import com.smartgwt.client.widgets.grid.events.CellContextClickHandler;
import com.smartgwt.client.widgets.grid.events.CellSavedEvent;
import com.smartgwt.client.widgets.grid.events.CellSavedHandler;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.events.MenuItemClickEvent;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * This class shows the storagesGrid list and informations.
 * 
 * @author Matteo Caruso - Logical Objects
 * @since 6.1
 */
public class StoragesPanel extends VLayout {

	public static final int OPERATION_NONE = 0;

	public static final int OPERATION_ADD = 1;

	public static final int OPERATION_CUMPUTESIZE = 2;

	private ListGrid storagesGrid;

	private ToolStrip toolStrip;

	public StoragesPanel() {
		setWidth100();
		setHeight100();
		setMembersMargin(5);
		setMargin(5);

		toolStrip = new ToolStrip();
		toolStrip.setHeight(20);
		toolStrip.setWidth100();
		toolStrip.addSpacer(2);

		ToolStripButton add = new ToolStripButton();
		add.setTitle(I18N.message("addstore"));
		add.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				onAddStorage();
			}
		});

		ToolStripButton refresh = new ToolStripButton();
		refresh.setTitle(I18N.message("refresh"));
		refresh.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				refresh();
			}
		});

		ToolStripButton save = new ToolStripButton();
		save.setTitle(I18N.message("save"));
		save.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				onSave();
			}
		});
		save.setDisabled(Session.get().isDemo() && Session.get().getUser().getId() == 1);

		if (Feature.visible(Feature.MULTI_STORAGE)) {
			toolStrip.addButton(add);
			toolStrip.addSeparator();
			if (!Feature.enabled(Feature.MULTI_STORAGE)) {
				add.setDisabled(true);
				add.setTooltip(I18N.message("featuredisabled"));
			}
		}

		if (Session.get().isDemo()) {
			save.setDisabled(true);
			add.setDisabled(true);
		}

		toolStrip.addButton(save);
		toolStrip.addSeparator();
		toolStrip.addButton(refresh);
		toolStrip.addFill();

		addMember(toolStrip);
		refresh();
	}

	private static boolean isParameterAttribute(String name) {
		if ("type".equals(name) || "id".equals(name) || "name".equals(name) || "path".equals(name)
				|| "write".equals(name) || name.startsWith("_"))
			return false;
		else
			return true;
	}

	private void refresh() {
		if (storagesGrid != null)
			removeMember(storagesGrid);

		storagesGrid = new ListGrid() {

			@Override
			protected Canvas getExpansionComponent(final ListGridRecord record) {
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

				parametersGrid.addCellSavedHandler(new CellSavedHandler() {
					@Override
					public void onCellSaved(CellSavedEvent event) {
						ListGridRecord paramRecord = event.getRecord();
						record.setAttribute(paramRecord.getAttributeAsString("name"),
								event.getNewValue() != null ? event.getNewValue().toString() : "");
					}
				});

				String[] attrs = record.getAttributes();
				if (attrs != null && attrs.length > 0) {
					List<ListGridRecord> records = new ArrayList<ListGridRecord>();
					for (String attr : attrs) {
						if (!StoragesPanel.isParameterAttribute(attr))
							continue;
						ListGridRecord rec = new ListGridRecord();
						rec.setAttribute("name", attr);
						rec.setAttribute("value", record.getAttributeAsString(attr));
						records.add(rec);
					}
					parametersGrid.setRecords(records.toArray(new ListGridRecord[0]));
				}

				layout.addMember(parametersGrid);
				return layout;
			}
		};
		storagesGrid.setEmptyMessage(I18N.message("notitemstoshow"));
		storagesGrid.setSelectionType(SelectionStyle.SINGLE);
		storagesGrid.setCanExpandRecords(true);

		ListGridField id = new ListGridField("id", " ", 20);
		ListGridField name = new ListGridField("name", I18N.message("name"), 100);
		ListGridField path = new ListGridField("path", I18N.message("path"));
		path.setWidth("100%");
		path.setCanEdit(true);
		ListGridField type = new ListGridField("type", I18N.message("type"), 110);
		type.setCanEdit(true);
		type.setCellFormatter(new CellFormatter() {

			@Override
			public String format(Object value, ListGridRecord record, int rowNum, int colNum) {
				if (value == null)
					return "";
				String label = I18N.message("storer." + value);
				if (label.equals("storer." + value))
					return value.toString();
				else
					return label;
			}
		});

		ListGridField write = new ListGridField("write", " ", 20);
		write.setType(ListGridFieldType.IMAGE);
		write.setCanSort(false);
		write.setAlign(Alignment.CENTER);
		write.setShowDefaultContextMenu(false);
		write.setImageURLPrefix(Util.imagePrefix());
		write.setImageURLSuffix(".png");
		write.setCanFilter(false);

		storagesGrid.setFields(id, write, name, type, path);
		storagesGrid.setDataSource(new StoragesDS(false, true));
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
					item.addChangedHandler(new ChangedHandler() {

						@Override
						public void onChanged(ChangedEvent event) {
							storagesGrid.getSelectedRecord().setAttribute("type", event.getValue().toString());
							onSave();
							String state = storagesGrid.getViewState();
							refresh();
							storagesGrid.setViewState(state);
						}
					});
					return item;
				} else
					return context.getDefaultProperties();
			}
		});

		addMember(storagesGrid);
	}

	/**
	 * Prepares the context menu
	 */
	private void showContextMenu() {
		MenuItem makeWrite = new MenuItem();
		makeWrite.setTitle(I18N.message("makedefwritestore"));
		makeWrite.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				ListGridRecord[] recs = storagesGrid.getRecords();
				for (ListGridRecord rec : recs) {
					rec.setAttribute("write", "blank");
					storagesGrid.refreshRow(storagesGrid.getRowNum(rec));
				}
				storagesGrid.getSelectedRecord().setAttribute("write", "database_edit");
				storagesGrid.refreshRow(storagesGrid.getRowNum(storagesGrid.getSelectedRecord()));
			}
		});

		MenuItem test = new MenuItem();
		test.setTitle(I18N.message("testconnection"));
		test.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				ListGridRecord selection = storagesGrid.getSelectedRecord();
				SettingService.Instance.get().testStorage(selection.getAttributeAsInt("id"),
						new AsyncCallback<Boolean>() {

							@Override
							public void onFailure(Throwable caught) {
								Log.serverError(caught);
							}

							@Override
							public void onSuccess(Boolean result) {
								if (result.booleanValue())
									SC.say(I18N.message("connectionestablished"));
								else
									SC.warn(I18N.message("connectionfailed"));
							}
						});
			}
		});

		if (Session.get().isDemo()) {
			makeWrite.setEnabled(false);
			test.setEnabled(false);
		}

		Menu contextMenu = new Menu();
		contextMenu.setItems(makeWrite, test);
		contextMenu.showContextMenu();
	}

	private void onSave() {
		final List<GUIParameter> settings = new ArrayList<GUIParameter>();
		ListGridRecord[] records = storagesGrid.getRecords();
		for (ListGridRecord rec : records) {
			String id = rec.getAttributeAsString("id").trim();
			settings.add(new GUIParameter("store." + id + ".dir", rec.getAttributeAsString("path").trim()));
			settings.add(new GUIParameter("store." + id + ".type", rec.getAttributeAsString("type").trim()));
			if ("database_edit".equals(rec.getAttributeAsString("write"))) {
				settings.add(new GUIParameter("store.write", id));
			}

			String[] attrs = rec.getAttributes();
			if (attrs != null && attrs.length > 0) {
				for (String attr : attrs) {
					if (!StoragesPanel.isParameterAttribute(attr))
						continue;
					settings.add(new GUIParameter("store." + id + "." + attr, rec.getAttributeAsString(attr).trim()));
				}
			}
		}

		SettingService.Instance.get().saveSettings(settings.toArray(new GUIParameter[0]), new AsyncCallback<Void>() {

			@Override
			public void onFailure(Throwable caught) {
				Log.serverError(caught);
			}

			@Override
			public void onSuccess(Void arg) {
				Log.info(I18N.message("settingssaved"), null);

				// Replicate the settings in the current session
				for (GUIParameter setting : settings) {
					Session.get().setConfig(setting.getName(), setting.getValue());
				}
			}
		});
	}

	private void onAddStorage() {
		for (int i = 1; i < 99; i++) {
			Record record = storagesGrid.getRecordList().find("id", Integer.toString(i));
			if (record == null) {
				ListGridRecord newStore = new ListGridRecord();
				newStore.setAttribute("id", Integer.toString(i));
				newStore.setAttribute("name", "Storage " + i);
				newStore.setAttribute("type", "fs");
				newStore.setAttribute("encrypt", "false");
				newStore.setAttribute("write", "blank");

				storagesGrid.getDataSource().addData(newStore);
				storagesGrid.redraw();
				break;
			}
		}
	}
}
