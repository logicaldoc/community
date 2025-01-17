package com.logicaldoc.gui.common.client.grid;

import com.logicaldoc.gui.common.client.grid.formatters.EnabledCellFormatter;
import com.smartgwt.client.widgets.grid.ListGridRecord;

/**
 * A field to display dates but also sensible to the enabled status of the
 * record
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.1.1
 */
public class EnabledDateListGridField extends DateListGridField {

	public EnabledDateListGridField(String name, String title, int format) {
		super(name, title, format);
	}

	public EnabledDateListGridField(String name, String title) {
		this(name, title, DateCellFormatter.FORMAT_DEFAULT);

		setCellFormatter(new DateCellFormatter() {

			@Override
			public String format(Object value, ListGridRecord rec, int rowNum, int colNum) {
				String content = super.format(value, rec, rowNum, colNum);
				return new EnabledCellFormatter().format(content, rec, rowNum, colNum);
			}
		});
	}
}