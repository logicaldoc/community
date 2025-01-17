package com.logicaldoc.gui.frontend.client.impex.archives;

import com.logicaldoc.gui.common.client.beans.GUIArchive;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.AwesomeFactory;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.AutoFitWidthApproach;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;

/**
 * A field to display the archive status, it must be bound to a boolean column
 * named status
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.1.1
 */
public class ArchiveStatusListGridField extends ListGridField {

	private static final String STATUS = "status";

	public ArchiveStatusListGridField() {
		super(STATUS, I18N.message(STATUS));
		setAutoFitWidth(true);
		setAutoFitWidthApproach(AutoFitWidthApproach.BOTH);
		setAlign(Alignment.CENTER);
		setCanFilter(true);
		setCanSort(true);
		setCellFormatter((value, rec, rowNum, colNum) -> formatStatusIconCell(rec));
	}

	private String formatStatusIconCell(ListGridRecord rec) {
		String content = "<div style='display: flex; text-align: center; justify-content: center;'>";
		Integer status = rec.getAttributeAsInt(STATUS);
		Integer mode = rec.getAttributeAsInt("mode");
		switch (status) {
		case GUIArchive.STATUS_OPEN:
			content += AwesomeFactory.getIconButtonHTML("lock-open", null,
					mode == GUIArchive.MODE_EXPORT ? "open" : "archivereadytoimport", null, null);
			break;
		case GUIArchive.STATUS_CLOSED:
			content += AwesomeFactory.getIconButtonHTML("lock", null, "archiveclosed", null, null);
			break;
		case GUIArchive.STATUS_FINALIZED:
			content += AwesomeFactory.getIconButtonHTML("clipboard-list", null, "archivefinalized", null, null);
			break;
		case GUIArchive.STATUS_READYTOSIGN:
			content += AwesomeFactory.getIconButtonHTML("signature", null, "archivereadytosign", null, null);
			break;
		default:
			content += AwesomeFactory.getIconButtonHTML("triangle-exclamation", null, "error", null, null);
			break;
		}

		content += "</div>";
		return content;
	}
}