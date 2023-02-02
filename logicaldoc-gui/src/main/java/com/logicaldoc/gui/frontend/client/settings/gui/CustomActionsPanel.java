package com.logicaldoc.gui.frontend.client.settings.gui;

import java.util.ArrayList;
import java.util.List;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIMenu;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.services.SecurityService;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.util.Util;
import com.smartgwt.client.data.AdvancedCriteria;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.ListGridFieldType;
import com.smartgwt.client.types.OperatorId;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.util.ValueCallback;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.HTMLFlow;
import com.smartgwt.client.widgets.ImgButton;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.events.DoubleClickEvent;
import com.smartgwt.client.widgets.events.DoubleClickHandler;
import com.smartgwt.client.widgets.form.fields.TextItem;
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
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * Shows the custom actions
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.5.3
 */
public class CustomActionsPanel extends VLayout {

	private ListGrid grid;

	private ListGridRecord rollOverRecord;

	private HLayout rollOverCanvas;

	private List<GUIMenu> actions = new ArrayList<GUIMenu>();

	public CustomActionsPanel() {
		setWidth100();
		setHeight100();
		setMembersMargin(5);
		setMargin(5);
	}

	@Override
	public void onDraw() {
		HTMLFlow hint = new HTMLFlow(I18N.message("customactionsshint"));
		hint.setMargin(3);

		ToolStripButton add = new ToolStripButton();
		add.setAutoFit(true);
		add.setTitle(I18N.message("addcustomaction"));
		add.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				TextItem item = ItemFactory.newTextItem("name", "", null);
				item.setRequired(true);
				LD.askForValue(I18N.message("newcustomaction"), I18N.message("name"), null, item, new ValueCallback() {
					@Override
					public void execute(String value) {
						GUIMenu action = new GUIMenu();
						action.setName(value.replace("/", ""));
						action.setType(2);
						action.setEnabled(true);
						action.setParentId(com.logicaldoc.gui.common.client.Menu.CUSTOM_ACTIONS);

						SecurityService.Instance.get().saveMenu(action, I18N.getLocale(), new AsyncCallback<GUIMenu>() {

							@Override
							public void onFailure(Throwable caught) {
								GuiLog.serverError(caught);
							}

							@Override
							public void onSuccess(GUIMenu newMenu) {
								actions.add(newMenu);
								fillGrid();

								CustomActionEditor editor = new CustomActionEditor(newMenu, CustomActionsPanel.this);
								editor.show();
							}
						});
					}
				});
			}
		});

		ToolStripButton save = new ToolStripButton();
		save.setAutoFit(true);
		save.setTitle(I18N.message("save"));
		save.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				onSave();
			}
		});

		ToolStrip toolStrip = new ToolStrip();
		toolStrip.addButton(add);
		toolStrip.addSeparator();
		toolStrip.addButton(save);
		toolStrip.addFill();
		toolStrip.setWidth100();

		ListGridField enabled = new ListGridField("eenabled", " ", 24);
		enabled.setType(ListGridFieldType.IMAGE);
		enabled.setCanSort(false);
		enabled.setAlign(Alignment.CENTER);
		enabled.setShowDefaultContextMenu(false);
		enabled.setImageURLPrefix(Util.imagePrefix());
		enabled.setImageURLSuffix(".gif");
		enabled.setCanFilter(false);

		ListGridField id = new ListGridField("id", I18N.message("id"));
		id.setWidth(80);
		id.setRequired(true);
		id.setCanEdit(false);
		id.setHidden(true);

		ListGridField name = new ListGridField("name", I18N.message("name"));
		name.setWidth(100);
		name.setRequired(true);

		ListGridField description = new ListGridField("description", I18N.message("description"));
		description.setWidth(300);

		ListGridField allowed = new ListGridField("allowed", I18N.message("allowedentities"));
		allowed.setWidth("*");

		grid = new ListGrid() {
			@Override
			protected Canvas getRollOverCanvas(Integer rowNum, Integer colNum) {
				rollOverRecord = this.getRecord(rowNum);

				if (rollOverCanvas == null) {
					rollOverCanvas = new HLayout(3);
					rollOverCanvas.setSnapTo("R");
					rollOverCanvas.setWidth(50);
					rollOverCanvas.setHeight(22);

					ImgButton editImg = new ImgButton();
					editImg.setShowDown(false);
					editImg.setShowRollOver(false);
					editImg.setLayoutAlign(Alignment.CENTER);
					editImg.setSrc("[SKIN]/actions/edit.png");
					editImg.setPrompt(I18N.message("edit"));
					editImg.setHeight(16);
					editImg.setWidth(16);
					editImg.addClickHandler(new ClickHandler() {
						public void onClick(ClickEvent event) {
							onEdit();
						}
					});

					rollOverCanvas.addMember(editImg);
				}
				return rollOverCanvas;
			}
		};
		grid.setEmptyMessage(I18N.message("notitemstoshow"));
		grid.setShowAllRecords(true);
		grid.setCanEdit(false);
		grid.setWidth100();
		grid.setHeight100();
		grid.setSelectionType(SelectionStyle.SINGLE);
		grid.setModalEditing(true);
		grid.setShowRollOverCanvas(true);
		grid.setShowRollUnderCanvas(false);
		grid.setCanReorderRecords(true);
		grid.setFields(enabled, id, name, description, allowed);
		setMembers(hint, toolStrip, grid);

		grid.addCellContextClickHandler(new CellContextClickHandler() {
			@Override
			public void onCellContextClick(CellContextClickEvent event) {
				showContextMenu();
				event.cancel();
			}
		});

		grid.addDoubleClickHandler(new DoubleClickHandler() {

			@Override
			public void onDoubleClick(DoubleClickEvent event) {
				onEdit();
			}
		});

		reload();
	}

	private void reload() {
		SecurityService.Instance.get().getMenus(com.logicaldoc.gui.common.client.Menu.CUSTOM_ACTIONS, I18N.getLocale(),
				false, new AsyncCallback<GUIMenu[]>() {
					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(GUIMenu[] mns) {
						actions.clear();

						if (mns != null)
							for (GUIMenu menu : mns)
								actions.add(menu);
						fillGrid();
					}
				});
	}

	/**
	 * Save the actions
	 */
	private void onSave() {

		/*
		 * First of all fix the ordering
		 */
		int i = 1;
		ListGridRecord[] records = grid.getRecords();
		if (records != null)
			for (ListGridRecord rec : records) {
				GUIMenu action = getAction(rec.getAttributeAsLong("id"));
				if (action != null)
					action.setPosition(i++);
			}

		SecurityService.Instance.get().saveMenus(actions.toArray(new GUIMenu[0]), I18N.getLocale(),
				new AsyncCallback<Void>() {
					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(Void arg0) {
						GuiLog.info(I18N.message("settingssaved"), null);
					}
				});
	}

	private void onEdit() {
		CustomActionEditor editor = new CustomActionEditor(getAction(rollOverRecord.getAttributeAsLong("id")),
				CustomActionsPanel.this);
		editor.show();
	}

	private void fillGrid() {
		ListGridRecord[] records = new ListGridRecord[actions.size()];
		int i = 0;
		for (GUIMenu menu : actions) {
			ListGridRecord rec = new ListGridRecord();
			rec.setAttribute("id", menu.getId());
			rec.setAttribute("name", menu.getName());
			rec.setAttribute("description", menu.getDescription());
			rec.setAttribute("eenabled", menu.isEnabled() ? "0" : "2");
			rec.setAttribute("allowed", Util.toString(menu.getRights()));
			records[i++] = rec;
		}
		grid.setData(records);
	}

	public void update(GUIMenu action) {
		Record rec = grid.find(new AdvancedCriteria("id", OperatorId.EQUALS, action.getId()));
		if (rec != null) {
			rec.setAttribute("name", action.getName());
			rec.setAttribute("description", action.getDescription());
			rec.setAttribute("eenabled", action.isEnabled() ? "0" : "2");
			rec.setAttribute("allowed", Util.toString(action.getRights()));

			grid.invalidateRecordComponents();
			grid.refreshRecordComponent(grid.getRecordIndex(rec));
			grid.refreshFields();
		}
	}

	private void showContextMenu() {
		Menu contextMenu = new Menu();

		ListGridRecord selectedRecord = grid.getSelectedRecord();

		MenuItem delete = new MenuItem();
		delete.setTitle(I18N.message("ddelete"));
		delete.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				LD.ask(I18N.message("question"), I18N.message("confirmdelete"), new BooleanCallback() {
					@Override
					public void execute(Boolean value) {
						if (value) {
							long menuId = selectedRecord.getAttributeAsLong("id");
							if (menuId == 0L) {
								int index = grid.getRecordIndex(selectedRecord);
								actions.remove(index);
							} else {
								SecurityService.Instance.get().deleteMenu(selectedRecord.getAttributeAsLong("id"),
										new AsyncCallback<Void>() {
											@Override
											public void onFailure(Throwable caught) {
												GuiLog.serverError(caught);
											}

											@Override
											public void onSuccess(Void arg) {
												reload();
											}
										});
							}
						}
					}
				});
			}
		});

		contextMenu.setItems(delete);
		contextMenu.showContextMenu();
	}

	private GUIMenu getAction(long actionId) {
		for (GUIMenu action : actions) {
			if (action.getId() == actionId)
				return action;
		}
		return null;
	}
}