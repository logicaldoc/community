package com.logicaldoc.gui.frontend.client.document.grid;

import java.util.ArrayList;
import java.util.List;

import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIAttribute;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.controllers.FolderController;
import com.logicaldoc.gui.common.client.log.GuiLog;
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

	private static final String SUMMARY = "summary";

	private static final String WORKFLOW_STATUS_DISPLAY = "workflowStatusDisplay";

	private static final String WORKFLOW_STATUS = "workflowStatus";

	private static final String PUBLISHED = "published";

	private static final String LAST_MODIFIED = "lastModified";

	private static final String DOCREF = "docref";

	private static final String COMMENT = "comment";

	private static final String FOLDER_ID = "folderId";

	private static final String FOLDER = "folder";

	private static final String CREATED = "created";

	private static final String BOOKMARKED = "bookmarked";

	private static final String STAMPED = "stamped";

	private static final String SIGNED = "signed";

	private static final String PASSWORD = "password";

	private static final String IMMUTABLE = "immutable";

	private static final String CREATOR_ID = "creatorId";

	private static final String PUBLISHER_ID = "publisherId";

	private static final String LOCK_USER = "lockUser";

	private static final String LOCK_USER_ID = "lockUserId";

	private static final String STATUS = "status";

	private static final String INDEXED = "indexed";

	private static final String PAGES = "pages";

	private static final String TENANT_ID = "tenantId";

	private static final String FILENAME = "filename";

	private DocumentGridUtil() {
	}

	public static List<Long> getIds(Record[] records) {
		List<Long> ids = new ArrayList<>();
		for (int i = 0; i < records.length; i++)
			ids.add(Long.parseLong(records[i].getAttributeAsString("id")));
		return ids;
	}

	public static List<GUIDocument> toDocuments(Record[] records) {
		List<GUIDocument> docs = new ArrayList<>();
		if (records != null)
			for (Record rec : records)
				docs.add(DocumentGridUtil.toDocument(rec));
		return docs;
	}

	public static GUIDocument toDocument(Record rec) {
		try {
			return prepareDocument(rec);
		} catch (Exception t) {
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
			document.setFileName(rec.getAttribute(FILENAME));
			document.setTemplate(rec.getAttribute("template"));
			document.setVersion(rec.getAttribute("version"));
			document.setFileVersion(rec.getAttribute("fileVersion"));
			document.setLanguage(rec.getAttribute("language"));
			document.setPublisher(rec.getAttributeAsString("publisher"));

			if (rec.getAttribute(TENANT_ID) != null)
				document.setTenantId(rec.getAttributeAsLong(TENANT_ID));

			if (rec.getAttributeAsFloat("size") != null)
				document.setFileSize(rec.getAttributeAsLong("size"));

			if (rec.getAttributeAsFloat(PAGES) != null)
				document.setPages(rec.getAttributeAsInt(PAGES));

			if (rec.getAttributeAsInt(INDEXED) != null)
				document.setIndexed(rec.getAttributeAsInt(INDEXED));

			if (rec.getAttributeAsInt(STATUS) != null)
				document.setStatus(rec.getAttributeAsInt(STATUS));

			setFlags(rec, document);

			setUsers(rec, document);

			setWorkflow(rec, document);

			setDates(rec, document);

			setFolder(rec, document);
		}
		return document;
	}

	private static void setUsers(Record rec, GUIDocument document) {
		if (rec.getAttribute(LOCK_USER_ID) != null)
			document.setLockUserId(rec.getAttributeAsLong(LOCK_USER_ID));

		if (rec.getAttribute(LOCK_USER) != null)
			document.setLockUser(rec.getAttribute(LOCK_USER));

		if (rec.getAttribute(PUBLISHER_ID) != null)
			document.setPublisherId(rec.getAttributeAsLong(PUBLISHER_ID));

		if (rec.getAttribute(CREATOR_ID) != null)
			document.setCreatorId(rec.getAttributeAsLong(CREATOR_ID));
	}

	private static void setFlags(Record rec, GUIDocument document) {
		if (rec.getAttributeAsInt(IMMUTABLE) != null)
			document.setImmutable(rec.getAttributeAsInt(IMMUTABLE));

		if (rec.getAttributeAsInt(PASSWORD) != null)
			document.setPasswordProtected(rec.getAttributeAsBoolean(PASSWORD));

		if (rec.getAttributeAsInt(SIGNED) != null)
			document.setSigned(rec.getAttributeAsInt(SIGNED));

		if (rec.getAttributeAsInt(STAMPED) != null)
			document.setStamped(rec.getAttributeAsInt(STAMPED));

		if (rec.getAttributeAsInt(BOOKMARKED) != null)
			document.setBookmarked(rec.getAttributeAsBoolean(BOOKMARKED));
	}

	private static void setDates(Record rec, GUIDocument document) {
		if (rec.getAttributeAsDate(LAST_MODIFIED) != null)
			document.setLastModified(rec.getAttributeAsDate(LAST_MODIFIED));
		if (rec.getAttributeAsDate(PUBLISHED) != null)
			document.setDate(rec.getAttributeAsDate(PUBLISHED));
		if (rec.getAttributeAsDate(CREATED) != null)
			document.setCreation(rec.getAttributeAsDate(CREATED));
	}

	private static void setWorkflow(Record rec, GUIDocument document) {
		if (rec.getAttributeAsInt(WORKFLOW_STATUS) != null)
			document.setWorkflowStatus(rec.getAttributeAsString(WORKFLOW_STATUS));

		if (rec.getAttributeAsString(WORKFLOW_STATUS_DISPLAY) != null)
			document.setWorkflowStatusDisplay(rec.getAttributeAsString(WORKFLOW_STATUS_DISPLAY));
	}

	private static void setId(Record rec, GUIDocument document) {
		document.setId(rec.getAttributeAsLong("id"));
		if (rec.getAttribute(DOCREF) != null) {
			document.setDocRef(Long.parseLong(rec.getAttribute(DOCREF)));
			document.setDocRefType(rec.getAttribute("docrefType"));
		}
	}

	private static void setFolder(Record rec, GUIDocument document) {
		GUIFolder folder = new GUIFolder();
		if (FOLDER.equals(document.getType())) {
			folder.setId(Long.parseLong(rec.getAttributeAsString("id")));
		} else if (rec.getAttributeAsLong(FOLDER_ID) != null)
			folder.setId(rec.getAttributeAsLong(FOLDER_ID));
		else
			folder.setId(FolderController.get().getCurrentFolder().getFoldRef() != null
					? FolderController.get().getCurrentFolder().getFoldRef()
					: FolderController.get().getCurrentFolder().getId());
		folder.setName(rec.getAttribute(FILENAME));
		folder.setDescription(rec.getAttribute(COMMENT));

		document.setFolder(folder);
	}

	public static ListGridRecord fromDocument(GUIDocument doc) {
		ListGridRecord rec = new ListGridRecord();
		updateRecord(doc, rec);
		return rec;
	}

	public static void updateRecord(GUIDocument doc, Record rec) {
		if (rec == null || doc == null)
			return;

		if (FOLDER.equals(rec.getAttribute("type"))) {
			rec.setAttribute(FILENAME, doc.getFolder().getName());
			rec.setAttribute(COMMENT, doc.getFolder().getDescription());
			rec.setAttribute(FOLDER_ID, doc.getId());
			rec.setAttribute(TENANT_ID, doc.getTenantId());
		} else {
			rec.setAttribute(SUMMARY, rec.getAttribute(SUMMARY));

			updateId(doc, rec);

			rec.setAttribute(FILENAME, doc.getFileName());
			rec.setAttribute("size", doc.getFileSize());
			rec.setAttribute(PAGES, doc.getPages());
			rec.setAttribute(TENANT_ID, doc.getTenantId());

			updateIcon(doc, rec);

			rec.setAttribute(LAST_MODIFIED, doc.getLastModified());
			rec.setAttribute(PUBLISHED, doc.getDate());
			rec.setAttribute("publisher", doc.getPublisher());
			rec.setAttribute(PUBLISHER_ID, doc.getPublisherId());
			rec.setAttribute("creator", doc.getCreator());
			rec.setAttribute(CREATOR_ID, doc.getCreatorId());
			rec.setAttribute(CREATED, doc.getCreation());
			rec.setAttribute("customId", doc.getCustomId());
			rec.setAttribute("type", doc.getType());
			rec.setAttribute(IMMUTABLE, doc.getImmutable());
			rec.setAttribute(PASSWORD, doc.isPasswordProtected());
			rec.setAttribute(SIGNED, doc.getSigned());
			rec.setAttribute(STAMPED, doc.getStamped());
			rec.setAttribute("fileVersion", doc.getFileVersion());
			rec.setAttribute("version", doc.getVersion());
			rec.setAttribute(COMMENT, doc.getComment());
			rec.setAttribute("lastNote", doc.getLastNote());
			rec.setAttribute(WORKFLOW_STATUS, doc.getWorkflowStatus());
			rec.setAttribute(WORKFLOW_STATUS_DISPLAY, doc.getWorkflowStatusDisplay());
			rec.setAttribute("color", doc.getColor());
			rec.setAttribute("startPublishing", doc.getStartPublishing());
			rec.setAttribute("stopPublishing", doc.getStopPublishing());
			rec.setAttribute("publishedStatus", doc.getPublished() == 1 ? "yes" : "no");
			if (rec.getAttribute("score") == null)
				rec.setAttribute("score", doc.getScore());
			rec.setAttribute(SUMMARY, doc.getSummary());
			rec.setAttribute("rating", doc.getRating());
			rec.setAttribute("template", doc.getTemplate());
			rec.setAttribute(LOCK_USER_ID, doc.getLockUserId());
			rec.setAttribute(LOCK_USER, doc.getLockUser());
			rec.setAttribute(INDEXED, doc.getIndexed());
			rec.setAttribute(STATUS, doc.getStatus());
			rec.setAttribute(BOOKMARKED, doc.isBookmarked());
			rec.setAttribute("extResId", doc.getExtResId());
			rec.setAttribute("language", doc.getLanguage());
			rec.setAttribute("links", doc.getLinks()
					+ (Session.get().getConfigAsBoolean("gui.showdocattrsaslinks") ? doc.getDocAttrs() : 0));
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
			else if (att.getType() == GUIAttribute.TYPE_DOCUMENT && att.getStringValues() == null)
				value = att.getStringValue();
			rec.setAttribute("ext_" + name, value);
		}
	}

	private static void updateFolder(GUIDocument doc, Record rec) {
		if (doc.getFolder() != null) {
			rec.setAttribute(FOLDER_ID, doc.getFolder().getId());
			rec.setAttribute(FOLDER, doc.getFolder().getName());
		}
	}

	private static void updateIcon(GUIDocument doc, Record rec) {
		rec.setAttribute("icon", doc.getIcon());
		if (rec.getAttribute(DOCREF) != null && !rec.getAttribute(DOCREF).isEmpty() && doc.getIcon() != null
				&& !doc.getIcon().endsWith("-sc"))
			rec.setAttribute("icon", doc.getIcon() + "-sc");
	}

	private static void updateId(GUIDocument doc, Record rec) {
		rec.setAttribute("id", doc.getId());
		rec.setAttribute(DOCREF, doc.getDocRef());
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
		StringBuilder sortSpec = new StringBuilder();
		if (sortSpecifiers != null) {
			for (SortSpecifier sortSpecifier : sortSpecifiers) {
				String attribute = getDocAttribute(sortSpecifier.getAttributeAsString("property"));
				if (attribute == null)
					continue;

				if (sortSpec.length() > 0)
					sortSpec.append(",");
				sortSpec.append(attribute);
				sortSpec.append(" ");
				sortSpec.append(
						sortSpecifier.getAttributeAsString("direction").equalsIgnoreCase("ascending") ? "asc" : "desc");
			}
		}

		return sortSpec.toString();
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
		if (property.equalsIgnoreCase(FILENAME))
			attribute = "fileName";
		else if (property.equalsIgnoreCase("size"))
			attribute = "fileSize";
		else if (property.equalsIgnoreCase(DOCREF))
			attribute = "docRef";
		else if (property.equalsIgnoreCase("docreftype"))
			attribute = "docRefType";
		else if (property.equalsIgnoreCase(PUBLISHED))
			attribute = "date";
		else if (property.equalsIgnoreCase(CREATED))
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
			} catch (Exception t) {
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
			} catch (Exception t) {
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
			} catch (Exception t) {
				return null;
			}
		} else
			return spec;
	}
}