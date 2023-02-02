package com.logicaldoc.gui.frontend.client.folder;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.widgets.grid.ColoredListGridField;
import com.smartgwt.client.widgets.grid.CellFormatter;
import com.smartgwt.client.widgets.grid.ListGridRecord;

/**
 * List grid field to display the folder option (tree or folder) in case of folder subscriptions
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.8.3
 */
public class FolderSubscriptionOptionListGridField extends ColoredListGridField {

	public FolderSubscriptionOptionListGridField() {
		super("folderOption", "option", 60);
		
		setCanEdit(false);
		setCellFormatter(new CellFormatter() {

			@Override
			public String format(Object value, ListGridRecord rec, int rowNum, int colNum) {
				try {
					String decoded = I18N.message("document");
					if ("folder".equals(rec.getAttributeAsString("type")))
						if ("1".equals(value.toString()))
							decoded = I18N.message("tree");
						else
							decoded = I18N.message("folder");

					String colorSpec = rec.getAttributeAsString("color");
					if (colorSpec != null && !colorSpec.isEmpty())
						return "<span style='color: " + colorSpec + ";'>" + decoded + "</span>";
					else
						return decoded != null ? decoded : "";
				} catch (Throwable e) {
					return "";
				}
			}
		});
	}

	
	
}