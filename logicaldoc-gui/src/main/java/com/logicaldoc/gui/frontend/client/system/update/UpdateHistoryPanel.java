package com.logicaldoc.gui.frontend.client.system.update;

import com.logicaldoc.gui.common.client.beans.GUIPatch;
import com.logicaldoc.gui.common.client.grid.DateListGridField;
import com.logicaldoc.gui.common.client.grid.DateListGridField.DateCellFormatter;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.types.ExpansionMode;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.types.SortDirection;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * List all updates and patches installed in the past
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2.2
 */
public class UpdateHistoryPanel extends VLayout {

	public UpdateHistoryPanel() {
		setMembersMargin(3);
	}

	@Override
	public void onDraw() {
		ListGridField name = new ListGridField("name", I18N.message("name"), 200);

		ListGridField rating = new ListGridField("rating", I18N.message("severityrating"), 110);
		rating.setCellFormatter((value, rec, rowNum, colNum) -> {
			int ratingVal = 0;
			if (value != null)
				ratingVal = Integer.parseInt(value.toString());

			String formatted = "<span style='color: " + GUIPatch.getColor(ratingVal) + "'>"
					+ I18N.message("severityrating." + ratingVal) + "</span>";
			if ("update".equals(rec.getAttributeAsString("type")))
				return "<b>" + formatted + "</b>";
			else
				return formatted;
		});

		ListGridField date = new DateListGridField("date", I18N.message("installed"), DateCellFormatter.FORMAT_DEFAULT);

		ListGridField type = new ListGridField("type", I18N.message("type"), 80);

		final ListGrid list = new ListGrid();
		list.setEmptyMessage(I18N.message("notitemstoshow"));
		list.setShowRecordComponents(true);
		list.setShowRecordComponentsByCell(true);
		list.setCanFreezeFields(true);
		list.setAutoFetchData(true);
		list.setSelectionType(SelectionStyle.NONE);
		list.setExpansionMode(ExpansionMode.DETAIL_FIELD);
		list.setDataSource(new UpdateHistoryDS());
		list.setFields(name, date, type, rating);
		list.sort("date", SortDirection.DESCENDING);
		list.setCellFormatter((value, rec, rowNum, colNum) -> {
			if ("update".equals(rec.getAttributeAsString("type")))
				return "<b>" + value + "</b>";
			else
				return value.toString();
		});

		addMember(list);
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