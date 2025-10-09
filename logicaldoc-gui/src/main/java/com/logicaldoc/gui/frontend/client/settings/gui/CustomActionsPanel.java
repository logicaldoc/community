package com.logicaldoc.gui.frontend.client.settings.gui;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIMenu;
import com.logicaldoc.gui.common.client.grid.IdListGridField;
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
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.HTMLFlow;
import com.smartgwt.client.widgets.ImgButton;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * Shows the custom actions
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.5.3
 */
public class CustomActionsPanel extends VLayout {

	private static final String ALLOWED = "allowed";

	private static final String DESCRIPTION = "description";

	private static final String EENABLED = "eenabled";

	private ListGrid grid;

	private ListGridRecord rollOverRecord;

	private HLayout rollOverCanvas;

	private List<GUIMenu> actions = new ArrayList<>();

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
		add.addClickHandler(event -> {
			TextItem item = ItemFactory.newTextItem("name", "", null);
			item.setRequired(true);
			LD.askForValue(I18N.message("newcustomaction"), I18N.message("name"), null, item, value -> {
				GUIMenu action = new GUIMenu();
				action.setName(value.replace("/", ""));
				action.setType(2);
				action.setEnabled(true);
				action.setParentId(com.logicaldoc.gui.common.client.Menu.CUSTOM_ACTIONS);

				SecurityService.Instance.get().saveMenu(action, I18N.getLocale(), new DefaultAsyncCallback<>() {
					@Override
					public void handleSuccess(GUIMenu newMenu) {
						actions.add(newMenu);
						fillGrid();
						new CustomActionEditor(newMenu, CustomActionsPanel.this).show();
					}
				});
			});
		});

		ToolStripButton save = new ToolStripButton();
		save.setAutoFit(true);
		save.setTitle(I18N.message("save"));
		save.addClickHandler(event -> onSave());

		ToolStrip toolStrip = new ToolStrip();
		toolStrip.addButton(add);
		toolStrip.addSeparator();
		toolStrip.addButton(save);
		toolStrip.addFill();
		toolStrip.setWidth100();

		ListGridField enabled = new ListGridField(EENABLED, " ", 24);
		enabled.setType(ListGridFieldType.IMAGE);
		enabled.setCanSort(false);
		enabled.setAlign(Alignment.CENTER);
		enabled.setShowDefaultContextMenu(false);
		enabled.setImageURLPrefix(Util.imagePrefix());
		enabled.setImageURLSuffix(".gif");
		enabled.setCanFilter(false);

		ListGridField id = new IdListGridField();

		ListGridField name = new ListGridField("name", I18N.message("name"));
		name.setWidth(100);
		name.setRequired(true);

		ListGridField description = new ListGridField(DESCRIPTION, I18N.message(DESCRIPTION));
		description.setWidth(300);

		ListGridField allowed = new ListGridField(ALLOWED, I18N.message("allowedentities"));
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
					editImg.addClickHandler(event -> onEdit());

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

		grid.addCellContextClickHandler(event -> {
			showContextMenu();
			event.cancel();
		});

		grid.addDoubleClickHandler(event -> onEdit());

		reload();
	}

	private void reload() {
		SecurityService.Instance.get().getMenus(com.logicaldoc.gui.common.client.Menu.CUSTOM_ACTIONS, I18N.getLocale(),
				false, new DefaultAsyncCallback<>() {
					@Override
					public void handleSuccess(List<GUIMenu> menus) {
						actions.clear();
						actions.addAll(menus);
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

		SecurityService.Instance.get().saveMenus(actions, I18N.getLocale(), new DefaultAsyncCallback<>() {
			@Override
			public void handleSuccess(Void arg0) {
				GuiLog.info(I18N.message("settingssaved"), null);
			}
		});
	}

	private void onEdit() {
		new CustomActionEditor(getAction(rollOverRecord.getAttributeAsLong("id")), CustomActionsPanel.this).show();
	}

	private void fillGrid() {
		ListGridRecord[] records = new ListGridRecord[actions.size()];
		int i = 0;
		for (GUIMenu menu : actions) {
			ListGridRecord rec = new ListGridRecord();
			rec.setAttribute("id", menu.getId());
			rec.setAttribute("name", menu.getName());
			rec.setAttribute(DESCRIPTION, menu.getDescription());
			rec.setAttribute(EENABLED, menu.isEnabled() ? "0" : "2");
			rec.setAttribute(ALLOWED,
					menu.getAccessControlList().stream().map(ace -> ace.getName()).collect(Collectors.joining(",")));
			records[i++] = rec;
		}
		grid.setData(records);
	}

	public void update(GUIMenu action) {
		Record rec = grid.find(new AdvancedCriteria("id", OperatorId.EQUALS, action.getId()));
		if (rec != null) {
			rec.setAttribute("name", action.getName());
			rec.setAttribute(DESCRIPTION, action.getDescription());
			rec.setAttribute(EENABLED, action.isEnabled() ? "0" : "2");
			rec.setAttribute(ALLOWED,
					action.getAccessControlList().stream().map(ace -> ace.getName()).collect(Collectors.joining(",")));

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
		delete.addClickHandler(event -> LD.ask(I18N.message("question"), I18N.message("confirmdelete"), confirm -> {
			if (Boolean.TRUE.equals(confirm)) {
				long menuId = selectedRecord.getAttributeAsLong("id");
				if (menuId == 0L) {
					int index = grid.getRecordIndex(selectedRecord);
					actions.remove(index);
				} else {
					SecurityService.Instance.get().deleteMenu(selectedRecord.getAttributeAsLong("id"),
							new DefaultAsyncCallback<>() {
								@Override
								public void handleSuccess(Void arg) {
									reload();
								}
							});
				}
			}
		}));

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

	@Override
	public boolean equals(Object other) {
		return super.equals(other);
	}

	@Override
	public int hashCode() {
		return super.hashCode();
	}
}