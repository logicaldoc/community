package com.logicaldoc.gui.common.client.widgets.grid;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.ListGridFieldType;

/**
 * A filed to display the indexed field do the Document
 * 
 * @author Marco Meschieri - LogicalDOC
 * 
 * @since 8.8.5
 */
public class IndexedListGridField extends ColoredListGridField {

	public IndexedListGridField() {
		this("indexed");
	}

	public IndexedListGridField(String name) {
		super(name, I18N.message("indexable"), 80);
		setAlign(Alignment.CENTER);
		setType(ListGridFieldType.TEXT);
		setAutoFitWidth(true);
		setCanFilter(false);
		setCellFormatter((value, listGridRecord, rowNum, colNum) -> {
			if (value == null)
				return "";

			if ("0".equals(value.toString()))
				return I18N.message("all");
			else if ("3".equals(value.toString()))
				return I18N.message("metadata");
			return "";
		});
	}
}