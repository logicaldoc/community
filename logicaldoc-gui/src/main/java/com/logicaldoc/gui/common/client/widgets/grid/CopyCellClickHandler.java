package com.logicaldoc.gui.common.client.widgets.grid;

import com.logicaldoc.gui.common.client.util.LD;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.events.CellDoubleClickEvent;
import com.smartgwt.client.widgets.grid.events.CellDoubleClickHandler;

/**
 * A click handler useful to give users the option to look and copy into the
 * clipboard the content of a grid cell
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.0.1
 */
public class CopyCellClickHandler implements CellDoubleClickHandler {
	@Override
	public void onCellDoubleClick(CellDoubleClickEvent click) {
		ListGrid grid = (ListGrid) click.getSource();
		ListGridField field = grid.getField(click.getColNum());
		String title = field.getTitle();
		String value = grid.getDefaultFormattedFieldValue(click.getRecord(), field);

		// In case the value contains HTML formatting, fallback to the original
		// content
		if (value != null
				&& (value.contains("<div") || value.contains("<p") || value.contains("<img") || value.contains("<b>")))
			value = click.getRecord().getAttribute(grid.getFieldName(click.getColNum()));

		LD.askForValue(title, title, value, v -> {
			// Nothing to do
		});
		click.cancel();
	}
}