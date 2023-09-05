package com.logicaldoc.gui.frontend.client.system.usage;

import com.logicaldoc.gui.common.client.data.SystemUsageDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.types.AutoFitWidthApproach;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.Progressbar;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;

/**
 * A grid to display current usage of the system usage
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.8.6
 */
public class UsageGrid extends ListGrid {

	
	public UsageGrid() {
		this(true);
	}
	
	public UsageGrid(boolean withContextMenu) {
		super();

		setEmptyMessage(I18N.message("notitemstoshow"));
		setLoadingDataMessage(I18N.message("calculatingstatspleasewait"));
		setAutoFetchData(true);
		setShowRecordComponents(true);
		setShowRecordComponentsByCell(true);

		ListGridField label = new ListGridField("label", I18N.message("measure"));
		label.setCellFormatter((value, rec, rowNum, colNum) -> stylize(I18N.message(value.toString()), rec));
		label.setAutoFitWidth(true);
		label.setMinWidth(150);

		ListGridField max = new ListGridField("max", I18N.message("max"));
		max.setAutoFitWidth(true);
		max.setAutoFitWidthApproach(AutoFitWidthApproach.BOTH);
		max.setCellFormatter((value, rec, rowNum, colNum) -> stylize((String) value, rec));

		ListGridField used = new ListGridField("used", I18N.message("used"));
		used.setAutoFitWidth(true);
		used.setAutoFitWidthApproach(AutoFitWidthApproach.BOTH);
		used.setCellFormatter((value, rec, rowNum, colNum) -> stylize((String) value, rec));

		ListGridField available = new ListGridField("available", I18N.message("available"));
		available.setAutoFitWidth(true);
		available.setAutoFitWidthApproach(AutoFitWidthApproach.BOTH);
		available.setCellFormatter((value, rec, rowNum, colNum) -> stylize((String) value, rec));

		ListGridField use = new ListGridField("use", " ");
		use.setAutoFitWidth(true);
		use.setAutoFitWidthApproach(AutoFitWidthApproach.BOTH);
		use.setCellFormatter((value, rec, rowNum, colNum) -> stylize(value + "%", rec));

		ListGridField progress = new ListGridField("progress", I18N.message("progress"));
		progress.setWidth(200);

		setFields(label, max, used, available, use, progress);

		setDataSource(new SystemUsageDS());

		if (withContextMenu)
			addCellContextClickHandler(contextClickEvent -> {
				prepateContextMenu().showContextMenu();
				contextClickEvent.cancel();
			});
	}

	/**
	 * Prepares the context menu for multiple selection
	 * 
	 * @return the prepared context menu
	 */
	private Menu prepateContextMenu() {
		MenuItem history = new MenuItem();
		history.setTitle(I18N.message("usagehistory"));
		history.addClickHandler(event -> new UsageHistoryChart(getSelectedRecord().getAttributeAsString("measure"),
				getSelectedRecord().getAttributeAsString("label")).show());

		Menu contextMenu = new Menu();
		contextMenu.setItems(history);

		return contextMenu;
	}

	private String stylize(String value, ListGridRecord rec) {
		if (rec.getAttributeAsInt("use") > 90) {
			return "<span class='systemusage-low'>" + value + "</span>";
		} else {
			return value;
		}
	}

	@Override
	protected Canvas createRecordComponent(final ListGridRecord rec, Integer colNum) {
		String fieldName = this.getFieldName(colNum);
		if (fieldName.equals("progress")) {
			Progressbar prgBar = new Progressbar();
			prgBar.setLength(200);
			prgBar.setBreadth(15);
			Integer percentDone = rec.getAttributeAsInt("use");
			prgBar.setPercentDone(percentDone);
			if (percentDone > 90)
				prgBar.setProgressStyle("systemusage-progress-low");
			return prgBar;
		} else {
			return null;
		}
	}
}