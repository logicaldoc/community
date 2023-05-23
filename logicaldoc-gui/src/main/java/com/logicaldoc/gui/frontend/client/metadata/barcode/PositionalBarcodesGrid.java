package com.logicaldoc.gui.frontend.client.metadata.barcode;

import java.util.ArrayList;

import com.logicaldoc.gui.common.client.beans.GUIBarcodeTemplate;
import com.logicaldoc.gui.common.client.beans.GUIBarcodeZone;
import com.logicaldoc.gui.common.client.beans.GUIZone;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.smartgwt.client.types.DragDataAction;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;

/**
 * This grids displays the positional barcode definitions of a positional
 * barcode template
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.5.3
 */
public class PositionalBarcodesGrid extends ListGrid {

	private static final String FORMATS = "formats";

	private static final String EXCLUDE = "exclude";

	private static final String INCLUDE = "include";

	public PositionalBarcodesGrid(GUIBarcodeTemplate barcodeTemplate) {
		ListGridField pattern = new ListGridField("pattern", I18N.message("patternscomma"));
		pattern.setWidth(300);
		pattern.setRequired(true);
		pattern.setEscapeHTML(true);

		ListGridField include = new ListGridField(INCLUDE, I18N.message(INCLUDE));
		include.setWidth(200);
		include.setRequired(false);
		include.setEscapeHTML(true);

		ListGridField exclude = new ListGridField(EXCLUDE, I18N.message(EXCLUDE));
		exclude.setWidth(200);
		exclude.setRequired(false);
		exclude.setEscapeHTML(true);

		ListGridField formats = new ListGridField(FORMATS, I18N.message(FORMATS));
		formats.setWidth(200);
		formats.setRequired(false);
		formats.setCanEdit(true);
		formats.setEditorProperties(ItemFactory.newBarcodeFormatsComboBoxItem((String) null));

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
		addCellContextClickHandler(event -> {
			showContextMenu();
			event.cancel();
		});

		if (barcodeTemplate != null) {
			ArrayList<ListGridRecord> records = new ArrayList<>();
			if (barcodeTemplate.getZones() != null)
				for (GUIZone pat : barcodeTemplate.getZones()) {
					GUIBarcodeZone brcPat = (GUIBarcodeZone) pat;
					ListGridRecord rec = new ListGridRecord();
					rec.setAttribute("pattern", brcPat.getPatterns());
					rec.setAttribute(INCLUDE, brcPat.getInclude());
					rec.setAttribute(EXCLUDE, brcPat.getExclude());

					String frmts = brcPat.getFormats();
					if (frmts == null || frmts.trim().isEmpty())
						rec.setAttribute(FORMATS, (String) null);
					else if (!frmts.trim().contains(","))
						rec.setAttribute(FORMATS, frmts.trim());
					else
						rec.setAttribute(FORMATS, brcPat.getFormats().replace(" ", "").split(","));
					records.add(rec);
				}
			setRecords(records.toArray(new ListGridRecord[0]));
		}
	}

	private void showContextMenu() {
		Menu contextMenu = new Menu();

		MenuItem clean = new MenuItem();
		clean.setTitle(I18N.message("ddelete"));
		clean.addClickHandler(event -> LD.ask(I18N.message("question"), I18N.message("confirmdelete"), confirm -> {
			if (Boolean.TRUE.equals(confirm))
				removeData(getSelectedRecord());
		}));

		contextMenu.setItems(clean);
		contextMenu.showContextMenu();
	}
}