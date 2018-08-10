package com.logicaldoc.gui.frontend.client.metadata.barcode;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIBarcodePattern;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.frontend.client.services.BarcodeService;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.DragDataAction;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.widgets.HTMLFlow;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.grid.CellFormatter;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.CellContextClickEvent;
import com.smartgwt.client.widgets.grid.events.CellContextClickHandler;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.events.MenuItemClickEvent;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * Shows the barcodes patterns configuration.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.1
 */
public class BarcodePatternsPanel extends VLayout {

	private ListGrid patternsGrid;

	private SelectItem templateSelector;

	public BarcodePatternsPanel() {
		setWidth100();
		setHeight100();
		setMembersMargin(5);
	}

	@Override
	public void onDraw() {
		HTMLFlow hint = new HTMLFlow(I18N.message("barcodepatternhint"));
		hint.setMargin(3);

		templateSelector = ItemFactory.newTemplateSelector(true, null);
		templateSelector.setWrapTitle(false);
		templateSelector.setMultiple(false);
		templateSelector.setEndRow(false);
		templateSelector.addChangedHandler(new ChangedHandler() {

			@Override
			public void onChanged(ChangedEvent event) {
				refresh();
			}
		});

		ToolStripButton save = new ToolStripButton();
		save.setAutoFit(true);
		save.setTitle(I18N.message("save"));
		save.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				savePatterns();
			}
		});

		ToolStripButton append = new ToolStripButton();
		append.setAutoFit(true);
		append.setTitle(I18N.message("appendpattern"));
		append.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				ListGridRecord rec = new ListGridRecord();
				rec.setAttribute("pattern", "");
				patternsGrid.getRecordList().add(rec);
			}
		});

		ToolStrip toolStrip = new ToolStrip();
		toolStrip.addFormItem(templateSelector);
		toolStrip.addButton(save);
		toolStrip.addButton(append);
		toolStrip.addFill();
		toolStrip.setWidth100();

		ListGridField pattern = new ListGridField("pattern", I18N.message("patternscomma"));
		pattern.setWidth(400);
		pattern.setRequired(true);
		pattern.setCellFormatter(new CellFormatter() {
			@Override
			public String format(Object value, ListGridRecord record, int rowNum, int colNum) {
				return Util.strip(record.getAttributeAsString("pattern"));
			}
		});

		patternsGrid = new ListGrid();
		patternsGrid.setEmptyMessage(I18N.message("notitemstoshow"));
		patternsGrid.setShowAllRecords(true);
		patternsGrid.setCanEdit(true);
		patternsGrid.setWidth100();
		patternsGrid.setHeight100();
		patternsGrid.setFields(pattern);
		patternsGrid.setSelectionType(SelectionStyle.SINGLE);
		patternsGrid.setModalEditing(true);
		patternsGrid.setFields(pattern);
		patternsGrid.setShowRowNumbers(true);
		patternsGrid.setCanReorderRecords(true);
		patternsGrid.setCanDragRecordsOut(true);
		patternsGrid.setCanAcceptDroppedRecords(true);
		patternsGrid.setDragDataAction(DragDataAction.MOVE);

		patternsGrid.addCellContextClickHandler(new CellContextClickHandler() {
			@Override
			public void onCellContextClick(CellContextClickEvent event) {
				showContextMenu();
				event.cancel();
			}
		});

		setMembers(hint, toolStrip, patternsGrid);
		
		refresh();
	}

	private void showContextMenu() {
		Menu contextMenu = new Menu();

		MenuItem clean = new MenuItem();
		clean.setTitle(I18N.message("delete"));
		clean.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				LD.ask(I18N.message("question"), I18N.message("confirmdelete"), new BooleanCallback() {
					@Override
					public void execute(Boolean value) {
						if (value) {
							patternsGrid.removeData(patternsGrid.getSelectedRecord());
						}
					}
				});
			}
		});

		contextMenu.setItems(clean);
		contextMenu.showContextMenu();
	}

	/**
	 * Sends the patterns in the grid to the server to save them.
	 */
	private void savePatterns() {
		Record[] records = patternsGrid.getRecords();
		String[] patterns = new String[records.length];
		int i = 0;
		for (Record record : records) {
			patterns[i++] = record.getAttributeAsString("pattern");
		}

		BarcodeService.Instance.get().savePatterns(patterns, getSelectedTemplate(), new AsyncCallback<Void>() {
			@Override
			public void onFailure(Throwable caught) {
				Log.serverError(caught);
			}

			@Override
			public void onSuccess(Void arg0) {
				Log.info(I18N.message("settingssaved"), null);
			}
		});
	}

	private Long getSelectedTemplate() {
		try {
			ListGridRecord selectedTemplate = templateSelector.getSelectedRecord();
			Long id = null;
			if (selectedTemplate != null && selectedTemplate.getAttribute("id") != null
					&& !"".equals(selectedTemplate.getAttributeAsString("id").trim()))
				id = Long.parseLong(selectedTemplate.getAttributeAsString("id"));
			return id;
		} catch (Throwable t) {
			Log.error(t.getMessage(), t.getMessage(), t);
			return null;
		}
	}

	private void refresh() {
		Long id = getSelectedTemplate();

		BarcodeService.Instance.get().loadPatterns(id, new AsyncCallback<GUIBarcodePattern[]>() {
			@Override
			public void onFailure(Throwable caught) {
				Log.serverError(caught);
			}

			@Override
			public void onSuccess(GUIBarcodePattern[] patterns) {
				ListGridRecord[] records = new ListGridRecord[patterns.length];
				int i = 0;
				for (GUIBarcodePattern pat : patterns) {
					ListGridRecord record = new ListGridRecord();
					record.setAttribute("position", pat.getPosition());
					record.setAttribute("pattern", pat.getPattern());
					record.setAttribute("template", pat.getTemplateId());
					records[i++] = record;
				}
				patternsGrid.setData(records);
			}
		});
	}
}