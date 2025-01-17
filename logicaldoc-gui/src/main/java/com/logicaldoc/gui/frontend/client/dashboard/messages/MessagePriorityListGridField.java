package com.logicaldoc.gui.frontend.client.dashboard.messages;

import com.logicaldoc.gui.common.client.beans.GUIMessage;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.AwesomeFactory;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.AutoFitWidthApproach;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;

/**
 * A field to display the priority of a message, based on field named pririty
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.1.1
 */
public class MessagePriorityListGridField extends ListGridField {

	public MessagePriorityListGridField() {
		super("priority", I18N.message("priority"));
		setCanFilter(true);
		setCanSort(true);
		setAlign(Alignment.CENTER);
		setAutoFitWidth(true);
		setAutoFitWidthApproach(AutoFitWidthApproach.BOTH);
		setCellFormatter((value, rec, rowNum, colNum) -> formatPriorityIconCell(rec));
	}

	private String formatPriorityIconCell(ListGridRecord rec) {
		String content = "<div style='display: flex; text-align: center; justify-content: center;'>";
		int priority = rec.getAttributeAsInt("priority");

		switch (priority) {
		case GUIMessage.PRIO_INFO:
			content += AwesomeFactory.getIconButtonHTML("square-exclamation", null, "medium", "orange", null);
			break;
		case GUIMessage.PRIO_WARN:
			content += AwesomeFactory.getIconButtonHTML("triangle-exclamation", null, "high", "red", null);
			break;
		default:
			content += AwesomeFactory.getIconButtonHTML("circle-exclamation", null, "low", "green", null);
			break;
		}
		content += "</div>";
		return content;
	}
}