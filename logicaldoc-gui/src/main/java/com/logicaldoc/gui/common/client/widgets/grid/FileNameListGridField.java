package com.logicaldoc.gui.common.client.widgets.grid;

import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.DocUtil;
import com.logicaldoc.gui.common.client.util.Util;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.widgets.grid.ListGridRecord;

/**
 * A field to display a file name with icon
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.6.1
 */
public class FileNameListGridField extends ColoredListGridField {

	private static final String FILENAME = "filename";

	private String iconFieldName = "icon";

	public FileNameListGridField() {
		this(FILENAME, "icon", I18N.message(FILENAME), 210);
	}

	public FileNameListGridField(String name) {
		this(name, null);
	}	

	public FileNameListGridField(String name, String iconFieldName, String title, int width) {
		super(name, "color", title != null ? title : I18N.message(FILENAME), width);
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

	public class FileNameCellFormatter extends ColoredCellFormatter {
		int size = 16;

		public FileNameCellFormatter() {
			super();
		}

		public FileNameCellFormatter(int size) {
			super();
			this.size = size;
		}

		@Override
		public String format(Object value, ListGridRecord rec, int rowNum, int colNum) {
			if (value == null)
				return "";

			String val = super.format(value, rec, rowNum, colNum);

			String iconName = rec.getAttributeAsString(iconFieldName);
			if (iconName == null || iconName.isEmpty())
				return val != null ? val : "";

			if (iconName.contains("folder") || iconName.contains("workspace")) {
				if (iconName.contains("alias"))
					return super.format(DocUtil.getFolderIcon(false, GUIFolder.TYPE_ALIAS, value.toString(), null), rec,
							rowNum, colNum);
				else if (iconName.contains("workspace"))
					return super.format(DocUtil.getFolderIcon(false, GUIFolder.TYPE_WORKSPACE, value.toString(), null),
							rec, rowNum, colNum);
				else
					return super.format(DocUtil.getFolderIcon(false, GUIFolder.TYPE_DEFAULT, value.toString(), null),
							rec, rowNum, colNum);
			} else {
				return Util.iconWithFilename(iconName, val != null ? val : "");
			}
		}
	}
	
	@Override
	public boolean equals(Object other) {
		return super.equals(other);
	}
	
	@Override
	public int hashCode() {
		return super.hashCode();
	}
}