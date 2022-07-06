package com.logicaldoc.gui.common.client.widgets.grid;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.Util;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.ListGridFieldType;
import com.smartgwt.client.widgets.grid.ListGridRecord;

/**
 * A field to display a file size
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.6.1
 */
public class FileSizeListGridField extends ColoredListGridField {

	public FileSizeListGridField(String name, String title, int width, boolean win7Format) {
		super(name, "color", title != null ? title : I18N.message("size"), width);
		setCellFormatter(new FileSizeCellFormatter(win7Format));
		setAlign(Alignment.RIGHT);
		setType(ListGridFieldType.FLOAT);
	}

	public FileSizeListGridField(String title, int width) {
		this("size", title != null ? title : I18N.message("size"), width, false);
	}
	
	public FileSizeListGridField(String name, String title, int width) {
		this(name, title != null ? title : I18N.message("size"), width, false);
	}

	public FileSizeListGridField(String name, String iconFieldName, String title) {
		this(name, title, 70);
	}

	public FileSizeListGridField(String name, String title) {
		this(name, title, null);
	}

	public class FileSizeCellFormatter extends ColoredCellFormatter {
		private boolean win7format = false;

		public FileSizeCellFormatter() {
			super();
		}

		public FileSizeCellFormatter(boolean win7format) {
			super();
			this.win7format = win7format;
		}

		@Override
		public String format(Object value, ListGridRecord record, int rowNum, int colNum) {
			if (value == null)
				return "";

			String val = null;
			if (win7format)
				val = Util.formatSizeW7(value);
			else
				val = Util.formatSizeKB(value);
			return super.format(val, record, rowNum, colNum);
		}
	}
}