package com.logicaldoc.gui.common.client.grid;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.DocUtil;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.ListGridFieldType;
import com.smartgwt.client.widgets.grid.ListGridRecord;

/**
 * A filed to display long integers in cells that contains a user reference
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.7
 */
public class RatingListGridField extends ColoredListGridField {

	public RatingListGridField() {
		super("rating", I18N.message("rating"), 95);
		setType(ListGridFieldType.IMAGE);
		setCanSort(true);
		setCanFilter(true);
		setAlign(Alignment.CENTER);
		setCellFormatter(new RatingCellFormatter());
	}

	/**
	 * Utility formatter for those cells that contains longs
	 * 
	 * @author Marco Meschieri - LogicalDOC
	 * @since 8.7
	 */
	public class RatingCellFormatter extends ColoredCellFormatter {

		@Override
		public String format(Object value, ListGridRecord rec, int rowNum, int colNum) {
			if (value == null)
				return "";

			String val = DocUtil.getRatingIcon((Integer) value);
			return super.format(val, rec, rowNum, colNum);
		}

	}
}