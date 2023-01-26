package com.logicaldoc.gui.frontend.client.settings.gui;

import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIDashlet;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.ListGridComponent;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.RadioGroupItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.CellContextClickEvent;
import com.smartgwt.client.widgets.grid.events.CellContextClickHandler;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.events.MenuItemClickEvent;
import com.smartgwt.client.widgets.tab.Tab;
import com.smartgwt.client.widgets.tab.TabSet;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * This dialog is used to edit the content of a dashlet
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.3.4
 */
public class DashletEditor extends Window {

	private DynamicForm form = new DynamicForm();

	private TextAreaItem content;

	private RadioGroupItem unique;

	private SpinnerItem max;

	private TextAreaItem query;

	private GUIDashlet dashlet;

	private DashletsPanel panel;

	private TabSet tabset;

	private ListGrid columnsGrid;

	public DashletEditor(GUIDashlet dashlet, DashletsPanel panel) {
		this.dashlet = dashlet;
		this.panel = panel;

		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("dashlet") + " - " + dashlet.getName());
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		setMargin(3);
		setWidth(660);
		setHeight(600);

		centerInPage();

		ToolStripButton save = new ToolStripButton();
		save.setTitle(I18N.message("save"));
		save.addClickHandler((com.smartgwt.client.widgets.events.ClickEvent event) -> {
			onSave();
		});

		ToolStripButton close = new ToolStripButton();
		close.setTitle(I18N.message("close"));
		close.addClickHandler((com.smartgwt.client.widgets.events.ClickEvent event) -> {
			destroy();
		});

		ToolStrip toolStrip = new ToolStrip();
		toolStrip.setHeight(20);
		toolStrip.setWidth100();
		toolStrip.addSpacer(2);
		toolStrip.addButton(save);
		toolStrip.addSeparator();
		toolStrip.addButton(close);

		addItem(toolStrip);

		Tab propertiesTab = new Tab(I18N.message("properties"));
		propertiesTab.setPane(preparePropertiesPanel());

		Tab columnsTab = new Tab(I18N.message("columns"));
		columnsTab.setPane(prepareColumnsPanel());

		tabset = new TabSet();
		tabset.setTabs(propertiesTab, columnsTab);

		addItem(tabset);
		onTypeChange(dashlet.getType());
	}

	private VLayout prepareColumnsPanel() {
		ListGridField attribute = new ListGridField("label", I18N.message("attribute"));
		attribute.setCanEdit(false);

		columnsGrid = new ListGrid();
		columnsGrid.setEmptyMessage(I18N.message("notitemstoshow"));
		columnsGrid.setCanEdit(false);
		columnsGrid.setWidth100();
		columnsGrid.setHeight100();
		columnsGrid.setSelectionType(SelectionStyle.MULTIPLE);
		columnsGrid.setCanReorderRecords(true);
		columnsGrid.setShowRowNumbers(true);
		columnsGrid.setFields(attribute);

		columnsGrid.addCellContextClickHandler(new CellContextClickHandler() {
			@Override
			public void onCellContextClick(CellContextClickEvent event) {
				Menu contextMenu = new Menu();
				MenuItem delete = new MenuItem();
				delete.setTitle(I18N.message("ddelete"));
				delete.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
					public void onClick(MenuItemClickEvent event) {
						columnsGrid.removeSelectedData();
					}
				});
				contextMenu.setItems(delete);
				contextMenu.showContextMenu();
				event.cancel();
			}
		});

		ToolStrip controls = new ToolStrip();
		controls.setWidth100();
		controls.setHeight(24);
		final SelectItem selector = ItemFactory
				.newAttributesSelector(dashlet.getType().equals(GUIDashlet.TYPE_DOCEVENT) ? "docevent" : null);
		selector.setWidth(100);
		selector.addChangedHandler(new ChangedHandler() {

			@Override
			public void onChanged(ChangedEvent event) {
				ListGridRecord[] seletion = selector.getSelectedRecords();
				for (ListGridRecord sel : seletion) {
					// Skip search-specific attributes
					if (sel.getAttributeAsString("name").equals("folder")
							|| sel.getAttributeAsString("name").equals("score"))
						continue;

					Record record = columnsGrid.getRecordList().find("name", sel.getAttributeAsString("name"));
					if (record == null) {
						ListGridRecord newRec = new ListGridRecord();
						newRec.setAttribute("name", sel.getAttributeAsString("name"));
						newRec.setAttribute("label", sel.getAttributeAsString("label"));
						columnsGrid.addData(newRec);
					}
				}
				selector.clearValue();
			}

		});
		controls.addFormItem(selector);

		columnsGrid.setGridComponents(new Object[] { ListGridComponent.HEADER, ListGridComponent.BODY, controls });

		for (String column : dashlet.getColumnsList()) {
			ListGridRecord record = new ListGridRecord();
			String n = column.trim();
			record.setAttribute("name", n);
			record.setAttribute("label", Session.get().getInfo().getAttributeLabel(n));
			columnsGrid.addData(record);
		}

		VLayout columnsPanel = new VLayout();
		columnsPanel.setMembers(columnsGrid);
		return columnsPanel;
	}

	private VLayout preparePropertiesPanel() {
		TextItem title = ItemFactory.newTextItem("title", "title", dashlet.getTitle());
		title.setRequired(true);
		title.setDisabled(dashlet.isSystemDashlet());

		max = ItemFactory.newSpinnerItem("max", "max", dashlet.getMax() != null ? dashlet.getMax() : 1);
		max.setMin(1);
		max.setRequired(true);

		unique = ItemFactory.newYesNoRadioItem("unique", "unique");
		unique.setValue(dashlet.isUnique());
		unique.setDisabled(dashlet.isSystemDashlet());
		unique.setRequired(true);

		SelectItem type = ItemFactory.newDashletTypeSelector(dashlet.getType());
		type.setRequired(true);
		type.setDisabled(dashlet.isSystemDashlet());
		type.addChangedHandler(new ChangedHandler() {

			@Override
			public void onChanged(ChangedEvent event) {
				onTypeChange(event.getValue().toString());
			}
		});

		content = ItemFactory.newTextAreaItemForAutomation("content", "content", dashlet.getContent(), null, true);
		content.setWidth("*");

		query = ItemFactory.newTextAreaItemForAutomation("query", "query", dashlet.getQuery(), null, false);
		query.setWidth("*");

		form.setWidth100();
		form.setHeight100();
		form.setTitleOrientation(TitleOrientation.TOP);
		form.setNumCols(1);
		form.setItems(title, type, max, unique, content, query);

		VLayout propertiesPanel = new VLayout();
		propertiesPanel.setMembers(form);
		return propertiesPanel;
	}

	private void onTypeChange(String newValue) {
		if ("content".equals(newValue)) {
			content.show();
			query.hide();
			unique.hide();
			max.hide();
			tabset.disableTab(1);
		} else {
			content.hide();
			query.show();
			max.show();
			if (GUIDashlet.TYPE_DOCUMENT.equals(newValue) || GUIDashlet.TYPE_DOCEVENT.equals(newValue)) {
				tabset.enableTab(1);
				unique.show();
			} else {
				tabset.disableTab(1);
				unique.hide();
			}
		}
	}

	private void onSave() {
		if (form.validate()) {
			dashlet.setContent(form.getValueAsString("content"));
			dashlet.setQuery(form.getValueAsString("query"));
			dashlet.setMax(Integer.parseInt(form.getValueAsString("max")));
			dashlet.setUnique(Boolean.parseBoolean(form.getValueAsString("unique")));

			if (!dashlet.isSystemDashlet()) {
				dashlet.setType(form.getValueAsString("type"));
				dashlet.setTitle(form.getValueAsString("title"));
			}

			dashlet.setColumns("");
			ListGridRecord[] records = columnsGrid.getRecords();
			if (records != null && records.length > 0) {
				for (ListGridRecord record : records) {
					if (!dashlet.getColumns().isEmpty())
						dashlet.setColumns(dashlet.getColumns() + ",");
					dashlet.setColumns(dashlet.getColumns() + record.getAttributeAsString("name"));
				}
			}

			panel.refreshGrid();
			destroy();
		} else {
			tabset.selectTab(0);
		}
	}
}