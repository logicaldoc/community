package com.logicaldoc.gui.common.client.widgets.grid;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.widgets.grid.CellFormatter;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;

/**
 * A field to display a text colored where the color is specified in another
 * field(by default called 'display')
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.7
 */
public class ColoredListGridField extends ListGridField {

	protected String colorFieldName = "color";

	public ColoredListGridField(String name, String colorFieldName, String title, int width) {
		this(name, I18N.message(title));
		setWidth(width);
		this.colorFieldName = colorFieldName;
	}

	public ColoredListGridField(String name, String title, int width) {
		this(name, "color", I18N.message(title), width);
	}

	public ColoredListGridField(String name, String title) {
		super(name, I18N.message(title));
		setCellFormatter(new ColoredCellFormatter());
		setCanFilter(true);
		setCanSort(true);
	}

	public ColoredListGridField(String name) {
		this(name, null);
	}

	public class ColoredCellFormatter implements CellFormatter {

		public ColoredCellFormatter() {
			super();
		}

		@Override
		public String format(Object value, ListGridRecord record, int rowNum, int colNum) {
			if (value == null)
				return "";

			String colorSpec = record.getAttributeAsString(colorFieldName);
			if (colorSpec != null && !colorSpec.isEmpty())
				return "<span style='color: " + colorSpec + ";'>" + value + "</span>";
			else
				return value != null ? value.toString() : "";
		}
	}

	public String getColorFieldName() {
		return colorFieldName;
	}

	public void setColorFieldName(String colorFieldName) {
		this.colorFieldName = colorFieldName;
	}
}