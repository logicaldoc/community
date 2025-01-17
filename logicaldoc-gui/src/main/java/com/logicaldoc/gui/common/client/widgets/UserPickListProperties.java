package com.logicaldoc.gui.common.client.widgets;

import com.logicaldoc.gui.common.client.grid.formatters.UserCellFormatter;
import com.smartgwt.client.widgets.grid.ListGrid;

/**
 * Template for creating users pick lists.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.1.2
 */
public class UserPickListProperties extends ListGrid {

	public UserPickListProperties() {
		setCellFormatter(new UserCellFormatter());
	}
}