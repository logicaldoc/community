package com.logicaldoc.gui.common.client.widgets.grid;

import com.logicaldoc.gui.common.client.util.Util;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.ListGridFieldType;
import com.smartgwt.client.widgets.grid.ListGridField;

/**
 * A field to display a file type icon
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.8.6
 */
public class IconGridField extends ListGridField {

	public IconGridField() {
		super("icon", " ", 21);
		setType(ListGridFieldType.IMAGE);
		setCanSort(false);
		setCanFilter(false);
		setHidden(true);
		setAlign(Alignment.CENTER);
		setShowDefaultContextMenu(false);
		setImageURLPrefix(Util.imagePrefix());
		setImageURLSuffix(".png");
	}
}