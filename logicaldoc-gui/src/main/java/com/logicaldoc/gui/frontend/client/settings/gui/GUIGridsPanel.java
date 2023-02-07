package com.logicaldoc.gui.frontend.client.settings.gui;

import java.util.ArrayList;
import java.util.List;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIParameter;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.frontend.client.services.SettingService;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.ListGridComponent;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.CellContextClickEvent;
import com.smartgwt.client.widgets.grid.events.CellContextClickHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.SectionStack;
import com.smartgwt.client.widgets.layout.SectionStackSection;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.events.MenuItemClickEvent;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * Displays the settings for the documents grids.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.5.4
 */
public class GUIGridsPanel extends VLayout {

	private static final String LABEL = "label";

	private ListGrid documentsFieldsGrid;

	private ListGrid searchGrid;

	private SpinnerItem searchHits;
	
	private SpinnerItem pageSize;
	
	public GUIGridsPanel() {
		setMembersMargin(3);	
	}	
		
	@Override
	public void onDraw() {
		ToolStrip toolbar = new ToolStrip();
		toolbar.setWidth100();
		ToolStripButton save = new ToolStripButton(I18N.message("save"));
		save.addClickHandler(new ClickHandler() {

			@Override
			public void onClick(ClickEvent event) {
				onSave();
			}
		});
		toolbar.addButton(save);

		SectionStack documentsStack = prepareDocumentsGrid();
		SectionStack searchStack = prepareSearchGrid();
		
		HLayout body = new HLayout();
		body.setMembersMargin(3);
		body.setWidth100();
		body.setHeight100();
		body.setMembers(documentsStack, searchStack);

		setMembers(toolbar, body);
	}

	private SectionStack prepareDocumentsGrid() {
		ListGridField attribute = new ListGridField(LABEL, I18N.message("attribute"));
		attribute.setCanEdit(false);

		documentsFieldsGrid = new ListGrid();
		documentsFieldsGrid.setEmptyMessage(I18N.message("notitemstoshow"));
		documentsFieldsGrid.setCanEdit(false);
		documentsFieldsGrid.setWidth100();
		documentsFieldsGrid.setHeight100();
		documentsFieldsGrid.setSelectionType(SelectionStyle.MULTIPLE);
		documentsFieldsGrid.setCanReorderRecords(true);
		documentsFieldsGrid.setShowRowNumbers(true);
		documentsFieldsGrid.setFields(attribute);

		documentsFieldsGrid.addCellContextClickHandler(new CellContextClickHandler() {
			@Override
			public void onCellContextClick(CellContextClickEvent event) {
				Menu contextMenu = new Menu();
				MenuItem delete = new MenuItem();
				delete.setTitle(I18N.message("ddelete"));
				delete.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
					public void onClick(MenuItemClickEvent event) {
						documentsFieldsGrid.removeSelectedData();
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
		final SelectItem selector = ItemFactory.newAttributesSelector();
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

					Record rec = documentsFieldsGrid.getRecordList().find("name", sel.getAttributeAsString("name"));
					if (rec == null) {
						ListGridRecord newRec = new ListGridRecord();
						newRec.setAttribute("name", sel.getAttributeAsString("name"));
						newRec.setAttribute(LABEL, sel.getAttributeAsString(LABEL));
						documentsFieldsGrid.addData(newRec);
					}
				}
				selector.clearValue();
			}

		});
		controls.addFormItem(selector);

		documentsFieldsGrid.setGridComponents(new Object[] { ListGridComponent.HEADER, ListGridComponent.BODY, controls });

		String columns = Session.get().getConfig("gui.document.columns");

		if (columns != null) {
			String[] attributes = columns.split("\\,");
			for (String att : attributes) {
				ListGridRecord rec = new ListGridRecord();
				String n = att.trim();
				rec.setAttribute("name", n);
				rec.setAttribute(LABEL, Session.get().getInfo().getAttributeLabel(n));
				documentsFieldsGrid.addData(rec);
			}
		}

		SectionStack stack = new SectionStack();
		stack.setWidth(350);
		stack.setHeight100();

		SectionStackSection section = new SectionStackSection(I18N.message("columnsindocuments"));
		section.setCanCollapse(false);
		section.setExpanded(true);

		pageSize = ItemFactory.newSpinnerItem("pagesize", Session.get().getConfigAsInt("gui.document.pagesize"));
		pageSize.setRequired(true);
		pageSize.setWrapTitle(false);
		pageSize.setMin(5);
		pageSize.setStep(10);
		controls.addFormItem(pageSize);
		
		section.setItems(documentsFieldsGrid);
		stack.setSections(section);

		return stack;
	}

	private SectionStack prepareSearchGrid() {
		ListGridField attribute = new ListGridField(LABEL, I18N.message("attribute"));
		attribute.setCanEdit(false);

		searchGrid = new ListGrid();
		searchGrid.setEmptyMessage(I18N.message("notitemstoshow"));
		searchGrid.setCanEdit(false);
		searchGrid.setWidth100();
		searchGrid.setHeight100();
		searchGrid.setSelectionType(SelectionStyle.MULTIPLE);
		searchGrid.setCanReorderRecords(true);
		searchGrid.setShowRowNumbers(true);
		searchGrid.setFields(attribute);

		searchGrid.addCellContextClickHandler(new CellContextClickHandler() {
			@Override
			public void onCellContextClick(CellContextClickEvent event) {
				Menu contextMenu = new Menu();
				MenuItem delete = new MenuItem();
				delete.setTitle(I18N.message("ddelete"));
				delete.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
					public void onClick(MenuItemClickEvent event) {
						searchGrid.removeSelectedData();
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
		final SelectItem selector = ItemFactory.newAttributesSelector();
		selector.setWidth(100);
		selector.addChangedHandler(new ChangedHandler() {

			@Override
			public void onChanged(ChangedEvent event) {
				ListGridRecord[] seletion = selector.getSelectedRecords();
				for (ListGridRecord sel : seletion) {
					Record rec = searchGrid.getRecordList().find("name", sel.getAttributeAsString("name"));
					if (rec == null) {
						ListGridRecord newRec = new ListGridRecord();
						newRec.setAttribute("name", sel.getAttributeAsString("name"));
						newRec.setAttribute(LABEL, sel.getAttributeAsString(LABEL));
						searchGrid.addData(newRec);
					}
				}
				selector.clearValue();
			}

		});
		controls.addFormItem(selector);
		
		searchHits = ItemFactory.newSpinnerItem("searchhits", I18N.message("searchhits"), Session.get().getConfigAsInt("search.hits"));
		searchHits.setRequired(true);
		searchHits.setWrapTitle(false);
		searchHits.setMin(5);
		searchHits.setStep(5);
		controls.addFormItem(searchHits);

		searchGrid.setGridComponents(new Object[] { ListGridComponent.HEADER, ListGridComponent.BODY, controls });

		String columns = Session.get().getConfig("gui.search.columns");

		if (columns != null) {
			String[] attributes = columns.split("\\,");
			for (String att : attributes) {
				ListGridRecord rec = new ListGridRecord();
				String n = att.trim();
				rec.setAttribute("name", n);
				rec.setAttribute(LABEL, I18N.message(Session.get().getInfo().getAttributeLabel(n)));
				searchGrid.addData(rec);
			}
		}

		SectionStack stack = new SectionStack();
		stack.setWidth(350);
		stack.setHeight(500);

		SectionStackSection section = new SectionStackSection(I18N.message("columnsinsearch"));
		section.setCanCollapse(false);
		section.setExpanded(true);

		section.setItems(searchGrid);
		stack.setSections(section);
		stack.setHeight100();

		return stack;
	}

	private void onSave() {
		List<String> extendedAttributes = new ArrayList<>();
		List<GUIParameter> parameters = new ArrayList<>();
		
		/*
		 * Prepare the list of columns for the documents screen
		 */
		collectDocumentsColumns(extendedAttributes, parameters);
		
		/*
		 * Prepare the list of columns for the search screen
		 */
		collectSearchColumns(extendedAttributes, parameters);

		/*
		 * Now taking care of defining what extended attributes have to be
		 * retrieved when searching for the documents
		 */
		StringBuilder value = new StringBuilder();
		for (String att : extendedAttributes) {
			if (value.length() > 0)
				value.append(",");
			value.append(att);
		}
		GUIParameter param = new GUIParameter(Session.get().getTenantName() + ".search.extattr", value.toString());
		parameters.add(param);
		Session.get().setConfig(param.getName(), param.getValue());

		param = new GUIParameter(Session.get().getTenantName() + ".search.hits", searchHits.getValueAsString());
		parameters.add(param);
		Session.get().setConfig(param.getName(), param.getValue());
		
		param = new GUIParameter(Session.get().getTenantName() + ".gui.document.pagesize", pageSize.getValueAsString());
		parameters.add(param);
		Session.get().setConfig(param.getName(), param.getValue());
		
		// Save all
		SettingService.Instance.get().saveSettings(parameters.toArray(new GUIParameter[0]), new AsyncCallback<Void>() {

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

	private void collectSearchColumns(List<String> extendedAttributes, List<GUIParameter> parameters) {
		StringBuilder value = new StringBuilder();
		ListGridRecord[] attrs = searchGrid.getRecords();
		for (ListGridRecord att : attrs) {
			if (value.length() > 0)
				value.append(",");
			String name = att.getAttributeAsString("name").trim();
			value.append(name);

			// Meanwhile collect the extended attributes
			if (name.startsWith("ext_")) {
				String n = name.substring(4);
				if (!extendedAttributes.contains(n))
					extendedAttributes.add(n);
			}
		}
		GUIParameter param = new GUIParameter(Session.get().getTenantName() + ".gui.search.columns", value.toString());
		parameters.add(param);
		Session.get().setConfig(param.getName(), param.getValue());
	}

	private List<GUIParameter> collectDocumentsColumns(List<String> extendedAttributes, List<GUIParameter> parameters) {
		StringBuilder value = new StringBuilder();
		ListGridRecord[] attrs = documentsFieldsGrid.getRecords();
		for (ListGridRecord att : attrs) {
			if (value.length() > 0)
				value.append(",");
			String name = att.getAttributeAsString("name").trim();
			value.append(name);

			// Meanwhile collect the extended attributes
			if (name.startsWith("ext_")) {
				String n = name.substring(4);
				if (!extendedAttributes.contains(n))
					extendedAttributes.add(n);
			}
		}
		
		GUIParameter param = new GUIParameter(Session.get().getTenantName() + ".gui.document.columns", value.toString());
		parameters.add(param);
		Session.get().setConfig(param.getName(), param.getValue());
		return parameters;
	}
}