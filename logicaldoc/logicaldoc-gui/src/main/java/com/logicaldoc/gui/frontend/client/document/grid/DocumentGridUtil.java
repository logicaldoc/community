package com.logicaldoc.gui.frontend.client.document.grid;

import java.util.ArrayList;

import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIAttribute;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.observer.FolderController;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.data.SortSpecifier;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridRecord;

/**
 * Utility methods for documents grids
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.0
 */
public class DocumentGridUtil {

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
				docs.add(DocumentGridUtil.toDocument(record));
		return docs.toArray(new GUIDocument[0]);
	}

	static public GUIDocument toDocument(Record record) {
		try {
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
				document.setLanguage(record.getAttribute("language"));

				document.setPublisher(record.getAttributeAsString("publisher"));

				if (record.getAttributeAsFloat("size") != null)
					document.setFileSize(record.getAttributeAsFloat("size"));

				if (record.getAttributeAsFloat("pages") != null)
					document.setPages(record.getAttributeAsInt("pages"));

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

				if (record.getAttribute("publisherId") != null)
					document.setPublisherId(record.getAttributeAsLong("publisherId"));

				if (record.getAttribute("creatorId") != null)
					document.setCreatorId(record.getAttributeAsLong("creatorId"));

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
					folder.setId(FolderController.get().getCurrentFolder().getFoldRef() != null
							? FolderController.get().getCurrentFolder().getFoldRef()
							: FolderController.get().getCurrentFolder().getId());
				folder.setName(record.getAttribute("filename"));
				folder.setDescription(record.getAttribute("comment"));

				document.setFolder(folder);
			}
			return document;
		} catch (Throwable t) {
			GuiLog.warn(t.getMessage(), null);
			return null;
		}
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
			if (record.getAttribute("docref") == null || record.getAttribute("docref").isEmpty()) {
				record.setAttribute("docref", doc.getDocRef());
				record.setAttribute("id", doc.getId());
			} else {
				record.setAttribute("id", doc.getDocRef());
				record.setAttribute("docref", doc.getId());
			}
			record.setAttribute("docrefType", doc.getDocRefType());
			record.setAttribute("filename", doc.getFileName());
			record.setAttribute("size", doc.getFileSize());
			record.setAttribute("pages", doc.getPages());

			record.setAttribute("icon", doc.getIcon());
			if (record.getAttribute("docref") != null && !record.getAttribute("docref").isEmpty()
					&& doc.getIcon() != null && !doc.getIcon().endsWith("-sc"))
				record.setAttribute("icon", doc.getIcon() + "-sc");

			record.setAttribute("lastModified", doc.getLastModified());
			record.setAttribute("published", doc.getDate());
			record.setAttribute("publisher", doc.getPublisher());
			record.setAttribute("publisherId", doc.getPublisherId());
			record.setAttribute("creator", doc.getCreator());
			record.setAttribute("creatorId", doc.getCreatorId());
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
			record.setAttribute("color", doc.getColor());
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
			record.setAttribute("language", doc.getLanguage());
			record.setAttribute("links", doc.getLinks());
			record.setAttribute("tags", doc.getTgs());

			String[] extNames = Session.get().getInfo().getConfig("search.extattr") != null
					? Session.get().getInfo().getConfig("search.extattr").split(",")
					: null;
			if (extNames != null)
				for (String name : extNames) {
					GUIAttribute att = doc.getAttribute(name);
					if (att == null)
						continue;
					Object value = (att != null && att.getStringValues() != null) ? att.getStringValues()
							: doc.getValue(name);
					if (att.getType() == GUIAttribute.TYPE_USER && att.getStringValues() == null)
						value = att.getUsername();
					else if(att.getType() == GUIAttribute.TYPE_FOLDER && att.getStringValues() == null)
						value = att.getStringValue();
					record.setAttribute("ext_" + name, value);
				}
		}
	}

	/**
	 * Takes the sort specification of a grid. It is a string of comma separated
	 * values each one defining the sorting of a specific attribute of the
	 * document, eg: fileName asc, date desc
	 * 
	 * @param grid the grid to inspect
	 * 
	 * @return the full sort specification
	 */
	public static String getSortSpec(ListGrid grid) {
		return getSortSpec(grid.getSort());
	}

	public static String getSortSpec(SortSpecifier[] sortSpecifiers) {
		String sortSpec = "";
		if (sortSpecifiers != null) {
			for (SortSpecifier sortSpecifier : sortSpecifiers) {
				String attribute = getDocAttribute(sortSpecifier.getAttributeAsString("property"));
				if (attribute == null)
					continue;
				if (!sortSpec.isEmpty())
					sortSpec += ",";
				sortSpec += attribute + " "
						+ (sortSpecifier.getAttributeAsString("direction").equalsIgnoreCase("ascending") ? "asc"
								: "desc");
			}
		}
		return sortSpec;
	}

	/**
	 * Translates the name of a property in the grid into a document's attribute
	 * 
	 * @param property name of the property
	 * 
	 * @return name of the attribute
	 */
	private static String getDocAttribute(String property) {
		String attribute;
		if (property.equalsIgnoreCase("filename"))
			attribute = "fileName";
		else if (property.equalsIgnoreCase("size"))
			attribute = "fileSize";
		else if (property.equalsIgnoreCase("docref"))
			attribute = "docRef";
		else if (property.equalsIgnoreCase("docreftype"))
			attribute = "docRefType";
		else if (property.equalsIgnoreCase("published"))
			attribute = "date";
		else if (property.equalsIgnoreCase("created"))
			attribute = "creation";
		else if (property.equalsIgnoreCase("order"))
			attribute = null;
		else
			attribute = property;
		return attribute;
	}

	/**
	 * Gets the page size from the layout specification, the format is:
	 * |<b>folderPageSize</b>|<b>pageSize</b>|<b>gridLayout</b>
	 * 
	 * @param spec layout specification
	 * 
	 * @return the page size
	 */
	public static Integer getPageSizeFromSpec(String spec) {
		if (spec != null && spec.startsWith("|")) {
			try {
				String txt = spec.substring(1, spec.indexOf('('));
				String[] tokens = txt.split("\\|");
				if (tokens.length == 1)
					return Integer.parseInt(tokens[0]);
				else if (tokens.length == 2)
					return Integer.parseInt(tokens[1]);
			} catch (Throwable t) {
				return null;
			}
		}
		return null;
	}

	/**
	 * Gets the page size from the layout specification, the format is:
	 * |<b>folderPageSize</b>|<b>pageSize</b>|<b>gridLayout</b>
	 * 
	 * @param spec layout specification
	 * 
	 * @return the page size
	 */
	public static Integer getFolderPageSizeFromSpec(String spec) {
		if (spec != null && spec.startsWith("|")) {
			try {
				String txt = spec.substring(1, spec.indexOf('('));
				String[] tokens = txt.split("\\|");
				if (tokens.length == 2)
					return Integer.parseInt(tokens[0]);
			} catch (Throwable t) {
				return null;
			}
		}
		return null;
	}

	/**
	 * Gets the grid layout specification, the format is:
	 * |<b>folderPageSize</b>|<b>pageSize</b>|<b>gridLayout</b> or simply
	 * <b>gridLayout</b>
	 * 
	 * @param spec layout specification
	 * 
	 * @return grid definition
	 */
	public static String getGridLayoutFromSpec(String spec) {
		if (spec == null)
			return null;

		if (spec.startsWith("|")) {
			try {
				return spec.substring(spec.indexOf('('));
			} catch (Throwable t) {
				return null;
			}
		} else
			return spec;
	}
}