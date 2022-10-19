package com.logicaldoc.gui.frontend.client.metadata.barcode;

import java.util.ArrayList;

import com.logicaldoc.gui.common.client.beans.GUIBarcodeSpec;
import com.logicaldoc.gui.common.client.beans.GUIBarcodeTemplate;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.smartgwt.client.types.DragDataAction;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.CellContextClickEvent;
import com.smartgwt.client.widgets.grid.events.CellContextClickHandler;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.events.MenuItemClickEvent;

/**
 * This grids displays the positional barcode definitions of a positional
 * barcode template
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.5.3
 */
public class PositionalBarcodesGrid extends ListGrid {

	public PositionalBarcodesGrid(GUIBarcodeTemplate barcodeTemplate) {
		ListGridField pattern = new ListGridField("pattern", I18N.message("patternscomma"));
		pattern.setWidth(300);
		pattern.setRequired(true);
		pattern.setEscapeHTML(true);

		ListGridField include = new ListGridField("include", I18N.message("include"));
		include.setWidth(200);
		include.setRequired(false);
		include.setEscapeHTML(true);

		ListGridField exclude = new ListGridField("exclude", I18N.message("exclude"));
		exclude.setWidth(200);
		exclude.setRequired(false);
		exclude.setEscapeHTML(true);

		ListGridField formats = new ListGridField("formats", I18N.message("formats"));
		formats.setWidth(200);
		formats.setRequired(false);
		formats.setCanEdit(true);
		formats.setEditorProperties(ItemFactory.newBarcodeFormatsComboBoxItem("formats", "formats", (String) null));

		setEmptyMessage(I18N.message("notitemstoshow"));
		setShowAllRecords(true);
		setCanEdit(true);
		setWidth100();
		setHeight100();
		setFields(pattern, include, exclude, formats);
		setSelectionType(SelectionStyle.SINGLE);
		setModalEditing(true);
		setShowRowNumbers(true);
		setCanReorderRecords(true);
		setCanDragRecordsOut(true);
		setCanAcceptDroppedRecords(true);
		setDragDataAction(DragDataAction.MOVE);
		addCellContextClickHandler(new CellContextClickHandler() {
			@Override
			public void onCellContextClick(CellContextClickEvent event) {
				showContextMenu();
				event.cancel();
			}
		});

		if (barcodeTemplate != null) {
			ArrayList<ListGridRecord> records = new ArrayList<ListGridRecord>();
			if (barcodeTemplate.getBarcodeSpecs() != null)
				for (GUIBarcodeSpec pat : barcodeTemplate.getBarcodeSpecs()) {
					ListGridRecord record = new ListGridRecord();
					record.setAttribute("pattern", pat.getPatterns());
					record.setAttribute("include", pat.getInclude());
					record.setAttribute("exclude", pat.getExclude());

					String frmts = pat.getFormats();
					if (frmts == null || frmts.trim().isEmpty())
						record.setAttribute("formats", (String) null);
					else if (!frmts.trim().contains(","))
						record.setAttribute("formats", frmts.trim());
					else
						record.setAttribute("formats", pat.getFormats().replace(" ", "").split(","));
					records.add(record);
				}
			setRecords(records.toArray(new ListGridRecord[0]));
		}
	}

	private void showContextMenu() {
		Menu contextMenu = new Menu();

		MenuItem clean = new MenuItem();
		clean.setTitle(I18N.message("ddelete"));
		clean.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				LD.ask(I18N.message("question"), I18N.message("confirmdelete"), new BooleanCallback() {
					@Override
					public void execute(Boolean value) {
						if (value) {
							removeData(getSelectedRecord());
						}
					}
				});
			}
		});

		contextMenu.setItems(clean);
		contextMenu.showContextMenu();
	}
}