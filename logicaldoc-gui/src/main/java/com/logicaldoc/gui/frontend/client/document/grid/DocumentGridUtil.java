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

	static public Long[] getIdsAsLong(Record[] records) {
		Long[] ids = new Long[records.length];
		for (int i = 0; i < records.length; i++)
			ids[i] = records[i].getAttributeAsLong("id");
		return ids;
	}

	static public GUIDocument[] toDocuments(Record[] records) {
		ArrayList<GUIDocument> docs = new ArrayList<GUIDocument>();
		if (records != null)
			for (Record rec : records)
				docs.add(DocumentGridUtil.toDocument(rec));
		return docs.toArray(new GUIDocument[0]);
	}

	static public GUIDocument toDocument(Record rec) {
		try {
			return prepareDocument(rec);
		} catch (Throwable t) {
			GuiLog.warn(t.getMessage(), null);
			return null;
		}
	}

	private static GUIDocument prepareDocument(Record rec) {
		GUIDocument document = null;
		if (rec != null) {
			document = new GUIDocument();
			setId(rec, document);

			document.setIcon(rec.getAttribute("icon"));
			document.setExtResId(rec.getAttributeAsString("extResId"));
			document.setCustomId(rec.getAttributeAsString("customId"));
			document.setType(rec.getAttribute("type"));
			document.setFileName(rec.getAttribute("filename"));
			document.setTemplate(rec.getAttribute("template"));
			document.setVersion(rec.getAttribute("version"));
			document.setFileVersion(rec.getAttribute("fileVersion"));
			document.setLanguage(rec.getAttribute("language"));
			document.setPublisher(rec.getAttributeAsString("publisher"));

			if (rec.getAttribute("tenantId") != null)
				document.setTenantId(rec.getAttributeAsLong("tenantId"));

			if (rec.getAttributeAsFloat("size") != null)
				document.setFileSize(rec.getAttributeAsLong("size"));

			if (rec.getAttributeAsFloat("pages") != null)
				document.setPages(rec.getAttributeAsInt("pages"));

			if (rec.getAttributeAsInt("indexed") != null)
				document.setIndexed(rec.getAttributeAsInt("indexed"));

			if (rec.getAttributeAsInt("status") != null)
				document.setStatus(rec.getAttributeAsInt("status"));

			setFlags(rec, document);

			setUsers(rec, document);

			setWorkflow(rec, document);

			setDates(rec, document);

			setFolder(rec, document);
		}
		return document;
	}

	private static void setUsers(Record rec, GUIDocument document) {
		if (rec.getAttribute("lockUserId") != null)
			document.setLockUserId(rec.getAttributeAsLong("lockUserId"));

		if (rec.getAttribute("lockUser") != null)
			document.setLockUser(rec.getAttribute("lockUser"));

		if (rec.getAttribute("publisherId") != null)
			document.setPublisherId(rec.getAttributeAsLong("publisherId"));

		if (rec.getAttribute("creatorId") != null)
			document.setCreatorId(rec.getAttributeAsLong("creatorId"));
	}

	private static void setFlags(Record rec, GUIDocument document) {
		if (rec.getAttributeAsInt("immutable") != null)
			document.setImmutable(rec.getAttributeAsInt("immutable"));

		if (rec.getAttributeAsInt("password") != null)
			document.setPasswordProtected(rec.getAttributeAsBoolean("password"));

		if (rec.getAttributeAsInt("signed") != null)
			document.setSigned(rec.getAttributeAsInt("signed"));

		if (rec.getAttributeAsInt("stamped") != null)
			document.setStamped(rec.getAttributeAsInt("stamped"));

		if (rec.getAttributeAsInt("bookmarked") != null)
			document.setBookmarked(rec.getAttributeAsBoolean("bookmarked"));
	}

	private static void setDates(Record rec, GUIDocument document) {
		if (rec.getAttributeAsDate("lastModified") != null)
			document.setLastModified(rec.getAttributeAsDate("lastModified"));
		if (rec.getAttributeAsDate("published") != null)
			document.setDate(rec.getAttributeAsDate("published"));
		if (rec.getAttributeAsDate("created") != null)
			document.setCreation(rec.getAttributeAsDate("created"));
	}

	private static void setWorkflow(Record rec, GUIDocument document) {
		if (rec.getAttributeAsInt("workflowStatus") != null)
			document.setWorkflowStatus(rec.getAttributeAsString("workflowStatus"));

		if (rec.getAttributeAsString("workflowStatusDisplay") != null)
			document.setWorkflowStatusDisplay(rec.getAttributeAsString("workflowStatusDisplay"));
	}

	private static void setId(Record rec, GUIDocument document) {
		document.setId(rec.getAttributeAsLong("id"));
		if (rec.getAttribute("docref") != null) {
			document.setDocRef(Long.parseLong(rec.getAttribute("docref")));
			document.setDocRefType(rec.getAttribute("docrefType"));
		}
	}

	private static void setFolder(Record rec, GUIDocument document) {
		GUIFolder folder = new GUIFolder();
		if ("folder".equals(document.getType())) {
			folder.setId(Long.parseLong(rec.getAttributeAsString("id")));
		} else if (rec.getAttributeAsLong("folderId") != null)
			folder.setId(rec.getAttributeAsLong("folderId"));
		else
			folder.setId(FolderController.get().getCurrentFolder().getFoldRef() != null
					? FolderController.get().getCurrentFolder().getFoldRef()
					: FolderController.get().getCurrentFolder().getId());
		folder.setName(rec.getAttribute("filename"));
		folder.setDescription(rec.getAttribute("comment"));

		document.setFolder(folder);
	}

	static public ListGridRecord fromDocument(GUIDocument doc) {
		ListGridRecord rec = new ListGridRecord();
		updateRecord(doc, rec);
		return rec;
	}

	static public void updateRecord(GUIDocument doc, Record rec) {
		if (rec == null || doc == null)
			return;

		if ("folder".equals(rec.getAttribute("type"))) {
			rec.setAttribute("filename", doc.getFolder().getName());
			rec.setAttribute("comment", doc.getFolder().getDescription());
			rec.setAttribute("folderId", doc.getId());
			rec.setAttribute("tenantId", doc.getTenantId());
		} else {
			rec.setAttribute("summary", rec.getAttribute("summary"));

			updateDocRef(doc, rec);

			rec.setAttribute("filename", doc.getFileName());
			rec.setAttribute("size", doc.getFileSize());
			rec.setAttribute("pages", doc.getPages());
			rec.setAttribute("tenantId", doc.getTenantId());

			updateIcon(doc, rec);

			rec.setAttribute("lastModified", doc.getLastModified());
			rec.setAttribute("published", doc.getDate());
			rec.setAttribute("publisher", doc.getPublisher());
			rec.setAttribute("publisherId", doc.getPublisherId());
			rec.setAttribute("creator", doc.getCreator());
			rec.setAttribute("creatorId", doc.getCreatorId());
			rec.setAttribute("created", doc.getCreation());
			rec.setAttribute("customId", doc.getCustomId());
			rec.setAttribute("type", doc.getType());
			rec.setAttribute("immutable", doc.getImmutable());
			rec.setAttribute("password", doc.isPasswordProtected());
			rec.setAttribute("signed", doc.getSigned());
			rec.setAttribute("stamped", doc.getStamped());
			rec.setAttribute("fileVersion", doc.getFileVersion());
			rec.setAttribute("version", doc.getVersion());
			rec.setAttribute("comment", doc.getComment());
			rec.setAttribute("workflowStatus", doc.getWorkflowStatus());
			rec.setAttribute("workflowStatusDisplay", doc.getWorkflowStatusDisplay());
			rec.setAttribute("color", doc.getColor());
			rec.setAttribute("startPublishing", doc.getStartPublishing());
			rec.setAttribute("stopPublishing", doc.getStopPublishing());
			rec.setAttribute("publishedStatus", doc.getPublished() == 1 ? "yes" : "no");
			rec.setAttribute("score", doc.getScore());
			rec.setAttribute("summary", doc.getSummary());
			rec.setAttribute("rating", doc.getRating());
			rec.setAttribute("template", doc.getTemplate());
			rec.setAttribute("lockUserId", doc.getLockUserId());
			rec.setAttribute("lockUser", doc.getLockUser());
			rec.setAttribute("indexed", doc.getIndexed());
			rec.setAttribute("status", doc.getStatus());
			rec.setAttribute("bookmarked", doc.isBookmarked());
			rec.setAttribute("extResId", doc.getExtResId());
			rec.setAttribute("language", doc.getLanguage());
			rec.setAttribute("links", doc.getLinks());
			rec.setAttribute("tags", doc.getTgs());

			updateFolder(doc, rec);

			updateExtendedAttributes(doc, rec);
		}
	}

	private static void updateExtendedAttributes(GUIDocument doc, Record rec) {
		String[] extNames = Session.get().getInfo().getConfig("search.extattr") != null
				? Session.get().getInfo().getConfig("search.extattr").split(",")
				: null;
		if (extNames == null)
			return;

		for (String name : extNames) {
			GUIAttribute att = doc.getAttribute(name);
			if (att == null)
				continue;
			Object value = (att != null && att.getStringValues() != null) ? att.getStringValues() : doc.getValue(name);
			if (att.getType() == GUIAttribute.TYPE_USER && att.getStringValues() == null)
				value = att.getUsername();
			else if (att.getType() == GUIAttribute.TYPE_FOLDER && att.getStringValues() == null)
				value = att.getStringValue();
			rec.setAttribute("ext_" + name, value);
		}
	}

	private static void updateFolder(GUIDocument doc, Record rec) {
		if (doc.getFolder() != null) {
			rec.setAttribute("folderId", doc.getFolder().getId());
			rec.setAttribute("folder", doc.getFolder().getName());
		}
	}

	private static void updateIcon(GUIDocument doc, Record rec) {
		rec.setAttribute("icon", doc.getIcon());
		if (rec.getAttribute("docref") != null && !rec.getAttribute("docref").isEmpty() && doc.getIcon() != null
				&& !doc.getIcon().endsWith("-sc"))
			rec.setAttribute("icon", doc.getIcon() + "-sc");
	}

	private static void updateDocRef(GUIDocument doc, Record rec) {
		if (rec.getAttribute("docref") == null || rec.getAttribute("docref").isEmpty()) {
			rec.setAttribute("docref", doc.getDocRef());
			rec.setAttribute("id", doc.getId());
		} else {
			rec.setAttribute("id", doc.getDocRef());
			rec.setAttribute("docref", doc.getId());
		}
		rec.setAttribute("docrefType", doc.getDocRefType());
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