package com.logicaldoc.gui.common.client.widgets.grid;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.DocUtil;
import com.smartgwt.client.widgets.grid.ListGridRecord;

/**
 * A field to display a folder name
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.7.1
 */
public class FolderListGridField extends ColoredListGridField {

	public FolderListGridField() {
		this("name", "folder");
	}

	public FolderListGridField(String name) {
		this(name, "folder");
	}

	public FolderListGridField(String name, String title) {
		super(name, I18N.message(title));
		setCellFormatter(new FolderNameCellFormatter());
	}

	public class FolderNameCellFormatter extends ColoredCellFormatter {
		public FolderNameCellFormatter() {
			super();
		}

		@Override
		public String format(Object value, ListGridRecord rec, int rowNum, int colNum) {
			if (value == null)
				return "";

			String val = value + "";
			String color = rec.getAttributeAsString(colorFieldName);
			val = DocUtil.getFolderIcon(
					rec.getAttributeAsBoolean("opened") != null && rec.getAttributeAsBoolean("opened"),
					rec.getAttributeAsInt("type"), val, color);

// Uncomment to have the name itself colored
//			if (color != null) {
//				val = DocUtil.getFolderIcon(
//						rec.getAttributeAsBoolean("opened") != null && rec.getAttributeAsBoolean("opened"),
//						rec.getAttributeAsInt("type"), val);
//				val = "<span style='color: " + color + "'>" + val + "</span>";
//			}
			return val;
		}
	}
}