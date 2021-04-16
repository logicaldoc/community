package com.logicaldoc.gui.common.client.widgets;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.AwesomeFactory;
import com.logicaldoc.gui.common.client.util.Util;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.widgets.grid.CellFormatter;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;

/**
 * A field to display a file name with icon
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.6.1
 */
public class FileNameListGridField extends ListGridField {

	private String iconFieldName = "icon";

	public FileNameListGridField() {
		this("filename", "icon", I18N.message("filename"), 210);
	}

	public FileNameListGridField(String name) {
		this(name, null);
	}

	public FileNameListGridField(String name, String iconFieldName, String title, int width) {
		super(name, title != null ? title : I18N.message("filename"), width);
		this.iconFieldName = iconFieldName;
		setCellFormatter(new FileNameCellFormatter(16));
		setCellAlign(Alignment.LEFT);
	}

	public FileNameListGridField(String name, String iconFieldName, String title) {
		this(name, iconFieldName, title, 24);
	}

	public FileNameListGridField(String name, String iconFieldName) {
		this(name, iconFieldName, null);
	}

	public class FileNameCellFormatter implements CellFormatter {
		int size = 16;

		public FileNameCellFormatter() {
			super();
		}

		public FileNameCellFormatter(int size) {
			super();
			this.size = size;
		}

		@Override
		public String format(Object value, ListGridRecord record, int rowNum, int colNum) {
			String iconName = record.getAttributeAsString(iconFieldName);
			if (iconName == null || iconName.isEmpty())
				return value != null ? value.toString() : "";

			if ("folder".equals(iconName) || "folder_closed.png".equals(iconName) || "folder_closed".equals(iconName))
				return AwesomeFactory.getIconHtml("folder") +"&nbsp;"+ (value != null ? value.toString() : "");
			else
				return Util.iconWithFilename(iconName, value != null ? value.toString() : "");
		}
	}
}