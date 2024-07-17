package com.logicaldoc.gui.common.client.widgets.grid;

import com.logicaldoc.gui.common.client.util.Util;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.ListGridFieldType;
import com.smartgwt.client.widgets.grid.CellFormatter;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;

/**
 * A field to display a file type icon
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.8.6
 */
public class IconGridField extends ListGridField {

	public IconGridField() {
		super("icon", " ", 30);
		setType(ListGridFieldType.TEXT);
		setCanSort(false);
		setCanFilter(false);
		setHidden(true);
		setAlign(Alignment.CENTER);
		setShowDefaultContextMenu(false);
		setCellFormatter(new FileIconCellFormatter(16));
	}

	public class FileIconCellFormatter implements CellFormatter {
		int size = 16;

		public FileIconCellFormatter() {
			super();
		}

		public FileIconCellFormatter(int size) {
			super();
			this.size = size;
		}

		@Override
		public String format(Object value, ListGridRecord rec, int rowNum, int colNum) {
			if (value == null)
				return "";

			String iconName = (String) value;
			if (iconName == null || iconName.isEmpty())
				return "";

			return Util.fileNameIcon(iconName, size);
		}
	}
}