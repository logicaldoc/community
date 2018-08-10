package com.logicaldoc.gui.frontend.client.document.grid;

import java.util.ArrayList;
import java.util.Date;

import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.widgets.grid.ListGridRecord;

/**
 * Utility methods for documents grids
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.0
 */
public class GridUtil {

	static public long[] getIds(Record[] records) {
		long[] ids = new long[records.length];
		for (int i = 0; i < records.length; i++)
			ids[i] = Long.parseLong(records[i].getAttributeAsString("id"));
		return ids;
	}

	static public GUIDocument[] toDocuments(Record[] records) {
		ArrayList<GUIDocument> docs = new ArrayList<GUIDocument>();
		if (records != null)
			for (Record record : records)
				docs.add(GridUtil.toDocument(record));
		return docs.toArray(new GUIDocument[0]);
	}

	static public GUIDocument toDocument(Record record) {
		GUIDocument document = null;
		if (record != null) {
			document = new GUIDocument();
			document.setId(record.getAttributeAsLong("id"));
			if (record.getAttribute("docref") != null) {
				document.setDocRef(Long.parseLong(record.getAttribute("docref")));
				document.setDocRefType(record.getAttribute("docrefType"));
			}
			document.setExtResId(record.getAttributeAsString("extResId"));
			document.setCustomId(record.getAttributeAsString("customId"));
			document.setType(record.getAttribute("type"));
			document.setFileName(record.getAttribute("filename"));
			document.setTemplate(record.getAttribute("template"));
			document.setVersion(record.getAttribute("version"));
			document.setFileVersion(record.getAttribute("fileVersion"));

			document.setPublisher(record.getAttributeAsString("publisher"));

			if (record.getAttributeAsFloat("size") != null)
				document.setFileSize(record.getAttributeAsFloat("size"));

			if (record.getAttributeAsInt("indexed") != null)
				document.setIndexed(record.getAttributeAsInt("indexed"));

			if (record.getAttributeAsInt("status") != null)
				document.setStatus(record.getAttributeAsInt("status"));

			if (record.getAttributeAsInt("immutable") != null)
				document.setImmutable(record.getAttributeAsInt("immutable"));

			if (record.getAttributeAsInt("password") != null)
				document.setPasswordProtected(record.getAttributeAsBoolean("password"));

			if (record.getAttributeAsInt("signed") != null)
				document.setSigned(record.getAttributeAsInt("signed"));

			if (record.getAttributeAsInt("stamped") != null)
				document.setStamped(record.getAttributeAsInt("stamped"));

			if (record.getAttributeAsInt("bookmarked") != null)
				document.setBookmarked(record.getAttributeAsBoolean("bookmarked"));

			if (record.getAttribute("lockUserId") != null)
				document.setLockUserId(record.getAttributeAsLong("lockUserId"));

			if (record.getAttribute("lockUser") != null)
				document.setLockUser(record.getAttribute("lockUser"));

			if (record.getAttribute("docref") != null) {
				document.setDocRef(Long.parseLong(record.getAttribute("docref")));
				document.setDocRefType(record.getAttribute("docrefType"));
			}

			if (record.getAttributeAsInt("workflowStatus") != null)
				document.setWorkflowStatus(record.getAttributeAsString("workflowStatus"));

			if (record.getAttributeAsString("workflowStatusDisplay") != null)
				document.setWorkflowStatusDisplay(record.getAttributeAsString("workflowStatusDisplay"));
			
			document.setIcon(record.getAttribute("icon"));
			if (record.getAttributeAsDate("lastModified") != null)
				document.setLastModified(record.getAttributeAsDate("lastModified"));
			if (record.getAttributeAsDate("published") != null)
				document.setDate(record.getAttributeAsDate("published"));
			if (record.getAttributeAsDate("created") != null)
				document.setCreation(record.getAttributeAsDate("created"));

			GUIFolder folder = new GUIFolder();
			if ("folder".equals(document.getType())) {
				folder.setId(Long.parseLong(record.getAttributeAsString("id")));
			} else if (record.getAttributeAsLong("folderId") != null)
				folder.setId(record.getAttributeAsLong("folderId"));
			else
				folder.setId(Session.get().getCurrentFolder().getId());
			folder.setName(record.getAttribute("filename"));
			folder.setDescription(record.getAttribute("comment"));

			document.setFolder(folder);
		}
		return document;
	}

	static public ListGridRecord fromDocument(GUIDocument doc) {
		ListGridRecord record = new ListGridRecord();
		updateRecord(doc, record);
		return record;
	}

	static public void updateRecord(GUIDocument doc, Record record) {
		if (record == null || doc == null)
			return;

		if ("folder".equals(record.getAttribute("type"))) {
			record.setAttribute("filename", doc.getFolder().getName());
			record.setAttribute("comment", doc.getFolder().getDescription());
		} else {
			record.setAttribute("summary", record.getAttribute("summary"));
			record.setAttribute("id", doc.getId());
			record.setAttribute("docref", doc.getDocRef());
			record.setAttribute("docrefType", doc.getDocRefType());
			record.setAttribute("filename", doc.getFileName());
			record.setAttribute("size", doc.getFileSize());
			record.setAttribute("icon", doc.getIcon());
			record.setAttribute("version", doc.getVersion());
			record.setAttribute("lastModified", doc.getLastModified());
			record.setAttribute("published", doc.getDate());
			record.setAttribute("publisher", doc.getPublisher());
			record.setAttribute("creator", doc.getCreator());
			record.setAttribute("created", doc.getCreation());
			record.setAttribute("customId", doc.getCustomId());
			record.setAttribute("type", doc.getType());
			record.setAttribute("immutable", doc.getImmutable());
			record.setAttribute("password", doc.isPasswordProtected());
			record.setAttribute("signed", doc.getSigned());
			record.setAttribute("stamped", doc.getStamped());
			record.setAttribute("fileVersion", doc.getFileVersion());
			record.setAttribute("version", doc.getVersion());
			record.setAttribute("comment", doc.getComment());
			record.setAttribute("workflowStatus", doc.getWorkflowStatus());
			record.setAttribute("workflowStatusDisplay", doc.getWorkflowStatusDisplay());
			record.setAttribute("startPublishing", doc.getStartPublishing());
			record.setAttribute("stopPublishing", doc.getStopPublishing());
			record.setAttribute("publishedStatus", doc.getPublished() == 1 ? "yes" : "no");
			record.setAttribute("score", doc.getScore());
			record.setAttribute("summary", doc.getSummary());
			record.setAttribute("folderId", doc.getFolder().getId());
			record.setAttribute("folder", doc.getFolder().getName());
			record.setAttribute("rating", doc.getRating());
			record.setAttribute("template", doc.getTemplate());
			record.setAttribute("lockUserId", doc.getLockUserId());
			record.setAttribute("lockUser", doc.getLockUser());
			record.setAttribute("indexed", doc.getIndexed());
			record.setAttribute("status", doc.getStatus());
			record.setAttribute("bookmarked", doc.isBookmarked());
			record.setAttribute("extResId", doc.getExtResId());

			String[] extNames = Session.get().getInfo().getConfig("search.extattr").split(",");
			for (String name : extNames) {
				Object value = doc.getValue(name);
				if (value instanceof Date)
					value = I18N.formatDateShort((Date) value);
				record.setAttribute("ext_" + name, value);
			}
		}
	}
}
