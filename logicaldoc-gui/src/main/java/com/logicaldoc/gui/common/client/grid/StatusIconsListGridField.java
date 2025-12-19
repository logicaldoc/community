package com.logicaldoc.gui.common.client.grid;

import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.util.AwesomeFactory;
import com.smartgwt.client.widgets.grid.ListGridRecord;

/**
 * A field to display the status icons of a document
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.6.6
 */
public class StatusIconsListGridField extends ColoredListGridField {

	private static final String STATUS_ICONS = "statusIcons";

	private static final String SIGNED = "signed";

	private static final String BOOKMARKED = "bookmarked";

	private static final String STAMPED = "stamped";

	private static final String IMMUTABLE = "immutable";

	private static final String INDEXED = "indexed";
	
	private static final String EMBEDDED = "embedded";

	private static final String FILE_VERSION = "fileVersion";

	private static final String FILENAME = "filename";

	public StatusIconsListGridField() {
		super(STATUS_ICONS, " ");

		setWidth(110);
		setCanFilter(false);
		setCanSort(false);
		setCellFormatter((value, rec, rowNum, colNum) -> formatStatusIconCell(rec));
	}

	private String formatStatusIconCell(ListGridRecord rec) {
		String color = rec.getAttributeAsString(colorFieldName);

		String content = "<div style='display: flex; text-align: center; justify-content: center;'>";

		// Put the bookmark icon
		content = putBookmarkStatusIcon(rec, color, content);

		// Put the indexing icon
		content = putIndexedStatusIcon(rec, color, content);

		// Put the embedding icon
		content = putEmbeddedStatusIcon(rec, color, content);
		
		// Put the status icon
		if (rec.getAttribute("status") != null) {
			Integer status = rec.getAttributeAsInt("status");
			if (status != null && status.intValue() > 0)
				content += AwesomeFactory.getLockedButtonHTML(status, rec.getAttributeAsString("lockUser"), color);
		}

		// Put the immutable icon
		content = putImmutableStatusIcon(rec, color, content);

		// Put the password protection icon
		if (Boolean.TRUE.equals(rec.getAttributeAsBoolean("password")))
			content += AwesomeFactory.getIconButtonHTML("key", null, "passwordprotected", color, null);

		// Put the signed icon
		content = putSignedStatusIcon(rec, color, content);

		// Put the stamped icon
		content = putStampedStatusIcon(rec, color, content);

		// Put the links icon
		content = putLinksStatusIcon(rec, color, content);

		content += "</div>";
		return content;
	}

	private String putLinksStatusIcon(ListGridRecord rec, String color, String content) {
		if (rec.getAttribute("links") != null) {
			Integer links = rec.getAttributeAsInt("links");
			if (links != null && links.intValue() > 0)
				content += AwesomeFactory.getIconButtonHTML("link", null, "withlinks", color, null);
		}
		return content;
	}

	private String putImmutableStatusIcon(ListGridRecord rec, String color, String content) {
		if (rec.getAttribute(IMMUTABLE) != null) {
			Integer immutable = rec.getAttributeAsInt(IMMUTABLE);
			if (immutable != null && immutable.intValue() == 1)
				content += AwesomeFactory.getIconButtonHTML("hand-paper", null, IMMUTABLE, color, null);
		}
		return content;
	}

	private String putBookmarkStatusIcon(ListGridRecord rec, String color, String content) {
		if (Boolean.TRUE.equals(rec.getAttributeAsBoolean(BOOKMARKED)))
			content += AwesomeFactory.getIconButtonHTML("bookmark", null, BOOKMARKED, color, null);
		return content;
	}

	private String putIndexedStatusIcon(ListGridRecord rec, String color, String content) {
		if (rec.getAttribute(INDEXED) != null) {
			Integer indexed = rec.getAttributeAsInt(INDEXED);
			if (indexed != null && indexed.intValue() != Constants.INDEX_TO_INDEX
					&& indexed.intValue() != Constants.INDEX_TO_INDEX_METADATA) {
				content += AwesomeFactory.getIndexedIconButtonHTML(rec.getAttributeAsLong("id"), indexed, color);
			}
		}
		return content;
	}
	
	private String putEmbeddedStatusIcon(ListGridRecord rec, String color, String content) {
		if (rec.getAttribute(EMBEDDED) != null) {
			Integer embedded = rec.getAttributeAsInt(EMBEDDED);
			if (embedded != null) {
				content += AwesomeFactory.getEmbeddedIconButtonHTML(embedded, color);
			}
		}
		return content;
	}

	private String putStampedStatusIcon(ListGridRecord rec, String color, String content) {
		if (rec.getAttribute(STAMPED) != null) {
			Integer stamped = rec.getAttributeAsInt(STAMPED);
			if (stamped != null && stamped.intValue() == 1)
				content += AwesomeFactory.getStampedIconButtonHTML(rec.getAttributeAsLong("id"),
						rec.getAttribute(FILE_VERSION), STAMPED, color);
		}
		return content;
	}

	private String putSignedStatusIcon(ListGridRecord rec, String color, String content) {
		if (rec.getAttribute(SIGNED) != null) {
			Integer signed = rec.getAttributeAsInt(SIGNED);
			if (signed != null && signed.intValue() == 1)
				content += AwesomeFactory.getSignedIconButtonHTML(rec.getAttributeAsLong("id"),
						rec.getAttributeAsString(FILENAME), STAMPED, color);
		}
		return content;
	}
}