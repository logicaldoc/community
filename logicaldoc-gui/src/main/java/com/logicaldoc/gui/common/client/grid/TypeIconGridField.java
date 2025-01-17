package com.logicaldoc.gui.common.client.grid;

import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.util.DocUtil;
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
public class TypeIconGridField extends ListGridField {

	public TypeIconGridField() {
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
			if (iconName.isEmpty())
				return "";

			if (iconName.contains("folder") || iconName.contains("workspace")) {
				if (iconName.contains("alias"))
					return DocUtil.getFolderIcon(false, GUIFolder.TYPE_ALIAS, "", null);
				else if (iconName.contains("workspace"))
					return DocUtil.getFolderIcon(false, GUIFolder.TYPE_WORKSPACE, "", null);
				else
					return DocUtil.getFolderIcon(false, GUIFolder.TYPE_DEFAULT, "", null);
			} else {
				return Util.fileNameIcon(iconName, size);
			}

		}
	}
}