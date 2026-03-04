package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.text.DateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.StringTokenizer;
import java.util.stream.Collectors;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.jdbc.core.RowMapper;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.document.Bookmark;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.DocumentComparator;
import com.logicaldoc.core.document.DocumentDAO;
import com.logicaldoc.core.document.DocumentStatus;
import com.logicaldoc.core.document.EmbeddingStatus;
import com.logicaldoc.core.document.IndexingStatus;
import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.metadata.Attribute;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.user.Group;
import com.logicaldoc.core.security.user.User;
import com.logicaldoc.core.security.user.UserDAO;
import com.logicaldoc.i18n.I18N;
import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.util.spring.Context;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

/**
 * This servlet is responsible for documents data.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class DocumentsDataServlet extends AbstractDataServlet {

	private static final String INDEXED = "indexed";

	private static final Logger log = LoggerFactory.getLogger(DocumentsDataServlet.class);

	private final class ExtendedAttributeRowMapper implements RowMapper<Long> {
		private final Map<String, Object> extAttributesValues;

		private final Locale locale;

		private ExtendedAttributeRowMapper(Map<String, Object> extAttributesValues, Locale locale) {
			this.extAttributesValues = extAttributesValues;
			this.locale = locale;
		}

		@Override
		public Long mapRow(ResultSet rs, int row) throws SQLException {
			Long docId = rs.getLong(1);
			String name = rs.getString(2);
			int type = rs.getInt(3);

			String key = "%d-%s".formatted(docId, name);

			if (type == Attribute.TYPE_STRING) {
				if (StringUtils.isNotEmpty(rs.getString(8)))
					extAttributesValues.put(key, rs.getString(8));
				else
					extAttributesValues.put(key, rs.getString(4));
			} else if (type == Attribute.TYPE_INT) {
				extAttributesValues.put(key, rs.getLong(5));
			} else if (type == Attribute.TYPE_DOUBLE) {
				extAttributesValues.put(key, rs.getDouble(6));
			} else if (type == Attribute.TYPE_DATE) {
				extAttributesValues.put(key, rs.getTimestamp(7));
			} else if (type == Attribute.TYPE_USER || type == Attribute.TYPE_FOLDER
					|| type == Attribute.TYPE_DOCUMENT) {
				extAttributesValues.put(key, rs.getString(4));
			} else if (type == Attribute.TYPE_BOOLEAN) {
				extAttributesValues.put(key,
						rs.getLong(5) == 1L ? I18N.message("true", locale) : I18N.message("false", locale));
			}

			return null;
		}
	}

	private static final long serialVersionUID = 1L;

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response, Session session, Integer max,
			Locale locale) throws PersistenceException, IOException {

		int maxRecords = max != null ? max : 100;

		response.setContentType("text/xml");
		response.setCharacterEncoding("UTF-8");

		DocumentDAO dao = DocumentDAO.get();

		int page = getPage(request);

		Integer status = getStatus(request);

		PrintWriter writer = response.getWriter();
		writer.write("<list>");

		String sql = "select ld_docid from ld_bookmark where ld_type=" + Bookmark.TYPE_DOCUMENT
				+ " and ld_deleted = 0 and ld_userid = " + session.getUserId();
		List<Long> bookmarks = dao.queryForList(sql, Long.class);

		// The list of documents to be returned
		List<Document> documentsInCurrentPage = new ArrayList<>();

		/*
		 * Retrieve the names of the extended attributes to show
		 */
		List<String> extendedAttributes = new ArrayList<>();

		String extendedAttributesSpec = prepareExtendedAttributes(request, session, extendedAttributes);

		/*
		 * Contains the extended attributes of the documents. The key is
		 * documentId-atttributeName, the value is the attribute value. This
		 * fieldsMap is used to maximize the listing performances.
		 */
		final Map<String, Object> extendedAttributesValues = new HashMap<>();

		Document hiliteDoc = null;

		if (status != null && status.intValue() != DocumentStatus.ARCHIVED.ordinal()) {
			findDocumentsByStatus(session, maxRecords, page, status, documentsInCurrentPage);
		} else if (StringUtils.isNotEmpty(request.getParameter("docIds"))) {
			findDocumentsByIds(request, session, documentsInCurrentPage);
		} else {
			hiliteDoc = findDocumentsByFilters(request, session, locale, maxRecords, page, documentsInCurrentPage,
					extendedAttributes, extendedAttributesSpec, extendedAttributesValues);
		}

		/*
		 * Iterate over the documents printing the output
		 */
		for (Document document : documentsInCurrentPage) {
			printDocument(writer, document, hiliteDoc, bookmarks, extendedAttributes, extendedAttributesValues,
					session.getTenantName());
		}

		writer.write("</list>");
	}

	private void printDocument(PrintWriter writer, Document document, Document hiliteDoc, List<Long> bookmarks,
			List<String> extendedAttributes, final Map<String, Object> extendedAttributesValues, String tenant) {

		writer.print("<document>");
		writer.print(String.format("<id>%d</id>", document.getId()));

		printFolderAndDocRef(writer, document);
		printCustomIdAndRevision(writer, document);

		writer.print(String.format("<icon>%s</icon>", FileUtil.getBaseName(document.getIcon())));
		writer.print(String.format("<version>%s</version>", document.getVersion()));

		printDates(writer, document);

		writer.print(String.format("<creator><![CDATA[%s]]></creator>", document.getCreator()));

		writer.print(String.format("<size>%d</size>", document.getFileSize()));
		writer.print(String.format("<pages>%d</pages>", document.getPages()));

		writer.print(String.format("<status>%d</status>", document.getStatus().ordinal()));
		writer.print(String.format("<immutable>%b</immutable>", document.isImmutable()));
		writer.print(String.format("<indexed>%d</indexed>", document.getIndexed().ordinal()));
		writer.print(String.format("<embedded>%d</embedded>", document.getEmbeddingStatus().ordinal()));
		writer.print(String.format("<password>%b</password>", StringUtils.isNotEmpty(document.getPassword())));
		writer.print(String.format("<signed>%b</signed>", document.isSigned()));
		writer.print(String.format("<stamped>%b</stamped>", document.isStamped()));
		writer.print(String.format("<bookmarked>%b</bookmarked>",
				(bookmarks.contains(document.getId()) || bookmarks.contains(document.getDocRef()))));
		writer.print(String.format("<language>%s</language>", document.getLanguage()));
		writer.print(String.format("<links>%d</links>",
				document.getLinks()
						+ (Context.get().getConfig().getBoolean("%s.gui.showdocattrsaslinks".formatted(tenant), false)
								? document.getDocAttrs()
								: 0)));
		writer.print(String.format("<publisherId>%d</publisherId>", document.getPublisherId()));
		writer.print(String.format("<creatorId>%d</creatorId>", document.getCreatorId()));
		writer.print(String.format("<tenantId>%d</tenantId>", document.getTenantId()));

		printLockUser(writer, document);

		writer.print(String.format("<filename><![CDATA[%s]]></filename>", document.getFileName()));
		writer.print(String.format("<type><![CDATA[%s]]></type>", document.getType()));

		writer.print(String.format("<rating>%d</rating>", document.getRating() != null ? document.getRating() : 0));
		writer.print(String.format("<fileVersion><![CDATA[%s]]></fileVersion>", document.getFileVersion()));

		if (StringUtils.isNotEmpty(document.getComment()))
			writer.print(String.format("<comment><![CDATA[%s]]></comment>", document.getComment()));

		if (StringUtils.isNotEmpty(document.getLastNote()))
			writer.print(String.format("<lastNote><![CDATA[%s]]></lastNote>", document.getLastNote()));

		if (StringUtils.isNotEmpty(document.getWorkflowStatus()))
			writer.print(
					String.format("<workflowStatus><![CDATA[%s]]></workflowStatus>", document.getWorkflowStatus()));
		if (StringUtils.isNotEmpty(document.getWorkflowStatusDisplay()))
			writer.print(String.format("<workflowStatusDisplay><![CDATA[%s]]></workflowStatusDisplay>",
					document.getWorkflowStatusDisplay()));

		if (StringUtils.isNotEmpty(document.getColor()))
			writer.print(String.format("<color><![CDATA[%s]]></color>", document.getColor()));

		writer.print(String.format("<publishedStatus>%s</publishedStatus>", document.isPublishing() ? "yes" : "no"));

		if (document.getExtResId() != null)
			writer.print(String.format("<extResId><![CDATA[%s]]></extResId>", document.getExtResId()));

		if (document.getTemplateName() != null)
			writer.print(String.format("<template><![CDATA[%s]]></template>", document.getTemplateName()));

		if (StringUtils.isNotEmpty(document.getTgs()))
			writer.print(String.format("<tags><![CDATA[%s]]></tags>",
					document.getTgs().substring(1, document.getTgs().length() - 1)));

		printExtendedAttributes(writer, document, extendedAttributes, extendedAttributesValues);

		if (hiliteDoc != null && document.getId() == hiliteDoc.getId())
			writer.print("<order>1</order>");

		writer.print("</document>");
	}

	private void printExtendedAttributes(PrintWriter writer, Document document, List<String> extendedAttributes,
			final Map<String, Object> extendedAttributesValues) {
		DateFormat df = getDateFormat();
		if (!extendedAttributesValues.isEmpty())
			for (String name : extendedAttributes) {
				Object val = document.getValue(name);
				if (val != null) {
					writer.print("<ext_%s>".formatted(name));

					switch (val) {
						case Date date -> writer.print(df.format(date));
						case Integer intVal -> writer.print(Integer.toString(intVal));
						case Long longVal -> writer.print(Long.toString(longVal));
						case Double doubleVal -> writer.print(Double.toString(doubleVal));
						default -> writer.print("<![CDATA[%s]]>".formatted(val));
					}

					writer.print("</ext_%s>".formatted(name));
				}
			}
	}

	private void printDates(PrintWriter writer, Document document) {
		DateFormat df = getDateFormat();
		writer.print(String.format("<lastModified>%s</lastModified>",
				document.getLastModified() != null ? df.format(document.getLastModified()) : ""));
		writer.print(String.format("<published>%s</published>",
				document.getDate() != null ? df.format(document.getDate()) : ""));
		writer.print(String.format("<publisher><![CDATA[%s]]></publisher>", document.getPublisher()));
		writer.print(String.format("<created>%s</created>",
				document.getCreation() != null ? df.format(document.getCreation()) : ""));
		writer.print(String.format("<startPublishing>%s</startPublishing>",
				document.getStartPublishing() != null ? df.format(document.getStartPublishing()) : ""));
		writer.print(String.format("<stopPublishing>%s</stopPublishing>",
				document.getStopPublishing() != null ? df.format(document.getStopPublishing()) : ""));
	}

	private void printLockUser(PrintWriter writer, Document document) {
		if (document.getLockUserId() != null)
			writer.print(String.format("<lockUserId>%d</lockUserId>", document.getLockUserId()));
		if (document.getLockUser() != null)
			writer.print(String.format("<lockUser><![CDATA[%s]]></lockUser>", document.getLockUser()));
	}

	private void printFolderAndDocRef(PrintWriter writer, Document document) {
		if (document.getFolder() != null)
			writer.print(String.format("<folderId>%d</folderId>", document.getFolder().getId()));
		if (document.getDocRef() != null) {
			writer.print(String.format("<docref>%d</docref>", document.getDocRef()));
			if (document.getDocRefType() != null)
				writer.print(String.format("<docrefType>%s</docrefType>", document.getDocRefType()));
		}
	}

	private void printCustomIdAndRevision(PrintWriter writer, Document document) {
		writer.print(String.format("<customId><![CDATA[%s]]></customId>",
				document.getCustomId() != null ? document.getCustomId() : ""));
		writer.print(String.format("<revision><![CDATA[%s]]></revision>",
				document.getRevision() != null ? document.getRevision() : ""));
	}

	private void findDocumentsByIds(HttpServletRequest request, Session session, List<Document> documentsInCurrentPage)
			throws PersistenceException {
		DocumentDAO dao = DocumentDAO.get();

		String[] idsArray = request.getParameter("docIds").split(",");
		for (String id : idsArray) {
			Document doc = null;
			try {
				doc = dao.findById(Long.parseLong(id));
			} catch (Exception t) {
				// Nothing to do
			}
			if (doc == null || doc.getDeleted() == 1 || !dao.isReadAllowed(Long.parseLong(id), session.getUserId()))
				continue;
			documentsInCurrentPage.add(doc);
		}
	}

	private void findDocumentsByStatus(Session session, int maxRecords, int page, Integer status,
			List<Document> documentsInCurrentPage) throws PersistenceException {
		DocumentDAO dao = DocumentDAO.get();

		List<Document> docs = dao.findByLockUserAndStatus(session.getUserId(),
				status != null ? DocumentStatus.values()[status] : null);
		int begin = (page - 1) * maxRecords;
		int end = Math.min(begin + maxRecords - 1, docs.size() - 1);
		for (int i = begin; i <= end; i++) {
			Document doc = docs.get(i);
			if (!dao.isReadAllowed(doc.getId(), session.getUserId()))
				continue;
			documentsInCurrentPage.add(doc);
		}
	}

	private Document findDocumentsByFilters(HttpServletRequest request, Session session, Locale locale, int maxRecords,
			int page, List<Document> documentsInCurrentPage, List<String> extendedAttributes,
			String extendedAttributesSpec, final Map<String, Object> extendedAttributesValues)
			throws PersistenceException, IOException {

		UserDAO udao = UserDAO.get();
		User sessionUser = udao.findById(session.getUserId());
		udao.initialize(sessionUser);

		String sort = request.getParameter("sort");

		/*
		 * Load some filters from the current request
		 */
		Long folderId = getFolderId(request, session);

		// Check if the folderId is an alias
		folderId = getFolder(folderId);

		Long formId = getFormId(request);

		String filename = getFilename(request);

		retrieveExtendedAttributesValues(locale, extendedAttributes, extendedAttributesSpec, extendedAttributesValues,
				folderId, formId);

		/*
		 * Execute the Query
		 */
		List<Document> documents = exeucuteQuery(request, extendedAttributes, extendedAttributesValues, sessionUser,
				folderId, formId, filename);

		// If a sorting is specified sort the collection of documents
		sortDocuments(documents, sort);

		takeDocumentsInCurrentPage(documents, documentsInCurrentPage, maxRecords, page);

		Long hiliteDocId = getHiliteDocId(request);

		return retrieveHiliteDoc(documentsInCurrentPage, folderId, hiliteDocId);
	}

	private List<Document> exeucuteQuery(HttpServletRequest request, List<String> extendedAttributes,
			final Map<String, Object> extendedAttributesValues, User user, Long folderId, Long formId, String filename)
			throws PersistenceException {

		DocumentDAO dao = DocumentDAO.get();
		StringBuilder query = new StringBuilder("""
select A.id, A.customId, A.docRef, A.type, A.version, A.lastModified, A.date, A.publisher, A.creation, A.creator, A.fileSize, A.immutable, A.indexingStatus, A.lockUserId, A.fileName, A.status,
       A.signed, A.type, A.rating, A.fileVersion, A.comment, A.workflowStatus, A.startPublishing, A.stopPublishing, A.published, A.extResId, B.name, A.docRefType, A.stamped, A.lockUser,
       A.password, A.pages, A.workflowStatusDisplay, A.language, A.links+A.docAttrs, A.tgs, A.creatorId, A.publisherId, A.color, A.folder.id, A.tenantId, A.lastNote, A.revision, A.embeddingStatus
  from Document as A
  left outer join A.template as B
 where A.deleted = 0
   and not A.status=""");
		query.append(DocumentStatus.ARCHIVED.ordinal());

		if (folderId != null) {
			query.append(" and A.folder.id = %d".formatted(folderId));

			List<Long> forbiddenDocIds = getForbiddenDocumentIds(user, folderId);
			if (!forbiddenDocIds.isEmpty()) {
				query.append(" and A.id not in (");
				query.append(forbiddenDocIds.stream().map(id -> Long.toString(id)).collect(Collectors.joining(",")));
				query.append(") ");
			}
		}

		if (formId != null)
			query.append(" and A.formId = %d".formatted(formId));
		if (StringUtils.isNotEmpty(request.getParameter(INDEXED)))
			query.append(" and A.indexingStatus = %s".formatted(request.getParameter(INDEXED)));

		Map<String, Object> params = new HashMap<>();
		if (filename != null) {
			query.append(" and lower(A.fileName) like :fileName ");
			params.put("fileName", "%%%s%%".formatted(filename.toLowerCase()));
		}

		List<?> records = new ArrayList<>();
		if (folderId != null || filename != null || formId != null
				|| StringUtils.isNotEmpty(request.getParameter(INDEXED)))
			records = dao.findByQuery(query.toString(), params, null);

		return enrichRecords(records, extendedAttributes, extendedAttributesValues, user);
	}

	/**
	 * Identifies those documents in the current folder where there is at least
	 * one read revocation in regards to one of the user's groups
	 * 
	 * @param user the current user
	 * @param folderId identifier of the folder
	 * @return the list of forbidden Ids
	 * 
	 * @throws PersistenceException Error in the data layer
	 */
	private List<Long> getForbiddenDocumentIds(User user, long folderId) throws PersistenceException {
		if (user.isAdmin())
			return new ArrayList<>();

		DocumentDAO docDao = DocumentDAO.get();
		String groupIdsString = user.getGroups().stream().map(g -> Long.toString(g.getId()))
				.collect(Collectors.joining(","));

		StringBuilder forbiddenDocsQuery = new StringBuilder("""
select ld_docid
  from ld_document_acl, ld_document
 where ld_docid=ld_id
   and ld_deleted = 0
   and ld_read = 0
   and ld_groupId in(""");
		forbiddenDocsQuery.append(groupIdsString);
		forbiddenDocsQuery.append(") and ld_folderid = ");
		forbiddenDocsQuery.append(Long.toString(folderId));

		return docDao.queryForList(forbiddenDocsQuery.toString(), Long.class);
	}

	private List<Document> enrichRecords(List<?> records, List<String> extendedAttributes,
			final Map<String, Object> extendedAttributesValues, User user) throws PersistenceException {

		DocumentDAO dao = DocumentDAO.get();

		/*
		 * Iterate over records enriching the data
		 */
		List<Document> documents = new ArrayList<>();
		for (int i = 0; i < records.size(); i++) {
			Object[] cols = (Object[]) records.get(i);
			Document doc = new Document();
			doc.setId((Long) cols[0]);
			doc.setDocRef((Long) cols[2]);
			doc.setFileName((String) cols[14]);
			doc.setType((String) cols[17]);
			doc.setDocRefType((String) cols[27]);
			doc.setColor((String) cols[38]);
			doc.setTenantId((Long) cols[40]);
			doc.setIndexingStatus((IndexingStatus) cols[12]);
			doc.setEmbeddingStatus((EmbeddingStatus) cols[43]);

			Folder f = new Folder();
			f.setId((Long) cols[39]);
			doc.setFolder(f);

			// Replace with the real document if this is an alias
			if (doc.getDocRef() != null && doc.getDocRef().longValue() != 0L) {
				long aliasId = doc.getId();
				long aliasDocRef = doc.getDocRef();
				String aliasDocRefType = doc.getDocRefType();
				String aliasFileName = doc.getFileName();
				String aliasColor = doc.getColor();
				String aliasType = doc.getType();
				IndexingStatus aliasIndexed = doc.getIndexed();
				EmbeddingStatus aliasEmbedded = doc.getEmbeddingStatus();
				doc = dao.findById(aliasDocRef);

				if (doc != null) {
					dao.initialize(doc);
					doc.setId(aliasId);
					doc.setDocRef(aliasDocRef);
					doc.setDocRefType(aliasDocRefType);
					doc.setFileName(aliasFileName);
					doc.setColor(aliasColor);
					doc.setType(aliasType);
					doc.setIndexingStatus(aliasIndexed);
					doc.setEmbeddingStatus(aliasEmbedded);

					enrichAliasExtendedAttributes(doc, aliasId, extendedAttributes, extendedAttributesValues);
				} else
					continue;
			} else {
				doc.setStartPublishing((Date) cols[22]);
				doc.setStopPublishing((Date) cols[23]);
				doc.setPublished((Boolean) cols[24]);

				enrichPublishedDocument(doc, cols, extendedAttributes, extendedAttributesValues, user);
			}

			addDocument(doc, documents, user);
		}
		return documents;
	}

	private void enrichPublishedDocument(Document doc, Object[] cols, List<String> extendedAttributes,
			final Map<String, Object> extendedAttributesValues, User user) {
		if (doc.isPublishing() || user.isMemberOf(Group.GROUP_ADMIN) || user.isMemberOf("publisher")) {
			doc.setCustomId((String) cols[1]);
			doc.setVersion((String) cols[4]);
			doc.setLastModified((Date) cols[5]);
			doc.setDate((Date) cols[6]);
			doc.setPublisher((String) cols[7]);
			doc.setCreation((Date) cols[8]);
			doc.setCreator((String) cols[9]);
			doc.setFileSize((Long) cols[10]);
			doc.setImmutable((Boolean) cols[11]);
			doc.setIndexingStatus((IndexingStatus) cols[12]);
			doc.setEmbeddingStatus((EmbeddingStatus) cols[43]);
			doc.setLockUserId((Long) cols[13]);
			doc.setStatus((DocumentStatus) cols[15]);
			doc.setSigned((Boolean) cols[16]);
			doc.setRating((Integer) cols[18]);
			doc.setFileVersion((String) cols[19]);
			doc.setComment((String) cols[20]);
			doc.setLastNote((String) cols[41]);
			doc.setRevision((String) cols[42]);
			doc.setWorkflowStatus((String) cols[21]);
			doc.setExtResId((String) cols[25]);
			doc.setTemplateName((String) cols[26]);
			doc.setStamped((Boolean) cols[28]);
			doc.setLockUser((String) cols[29]);
			doc.setPassword((String) cols[30]);
			doc.setPages((Integer) cols[31]);
			doc.setWorkflowStatusDisplay((String) cols[32]);
			doc.setLanguage((String) cols[33]);
			doc.setLinks((Integer) cols[34]);
			doc.setTgs((String) cols[35]);
			doc.setCreatorId((Long) cols[36]);
			doc.setPublisherId((Long) cols[37]);

			for (String name : extendedAttributes) {
				Object val = extendedAttributesValues.get(doc.getId() + "-" + name);
				if (val != null) {
					doc.setValue(name, val);
				}
			}
		}
	}

	private void enrichAliasExtendedAttributes(Document doc, long aliasId, List<String> extendedAttributes,
			final Map<String, Object> extendedAttributesValues) {
		if (doc.getTemplate() != null)
			doc.setTemplateName(doc.getTemplate().getName());

		for (String name : extendedAttributes) {
			String key = aliasId + "-" + name;
			if (!extendedAttributesValues.containsKey(key) && doc.getValue(name) != null) {
				Attribute att = doc.getAttribute(name);
				if (att != null && (att.getType() == Attribute.TYPE_FOLDER || att.getType() == Attribute.TYPE_USER
						|| att.getType() == Attribute.TYPE_DOCUMENT)) {
					extendedAttributesValues.put(key, att.getStringValue());
					doc.setValue(name, att.getStringValue());
				} else
					extendedAttributesValues.put(key, doc.getValue(name));
			}
		}
	}

	private void addDocument(Document doc, List<Document> documents, User user) {
		if (doc.isPublishing() || user.isMemberOf(Group.GROUP_ADMIN) || user.isMemberOf("publisher"))
			documents.add(doc);
	}

	private Document retrieveHiliteDoc(List<Document> documentRecords, Long folderId, Long hiliteDocId)
			throws PersistenceException {

		DocumentDAO dao = DocumentDAO.get();
		Document hiliteDoc = null;

		// Always add the hilight doc as first element of the collection
		if (hiliteDocId != null) {
			hiliteDoc = dao.findById(hiliteDocId);
			if (hiliteDoc != null && folderId != null && hiliteDoc.getFolder() != null
					&& hiliteDoc.getFolder().getId() == folderId) {
				dao.initialize(hiliteDoc);
			} else
				hiliteDoc = null;
		}
		if (hiliteDoc != null && !documentRecords.contains(hiliteDoc))
			documentRecords.add(0, hiliteDoc);
		return hiliteDoc;
	}

	private void takeDocumentsInCurrentPage(List<Document> documents, List<Document> documentRecords, int maxRecords,
			int page) {
		int begin = (page - 1) * maxRecords;
		int end = Math.min(begin + maxRecords - 1, documents.size() - 1);
		for (int i = begin; i <= end; i++)
			documentRecords.add(documents.get(i));
	}

	private void sortDocuments(List<Document> documents, String sort) {
		if (StringUtils.isNotEmpty(sort)) {
			// make the sorting to be case insensitive (add lower
			// function)
			StringBuilder ciSort = new StringBuilder();
			StringTokenizer st = new StringTokenizer(sort, ",", false);
			while (st.hasMoreElements()) {
				String token = (String) st.nextElement();
				String field = token.split(" ")[0].trim();
				String direction = token.split(" ")[1].trim();

				if (ciSort.length() > 0)
					ciSort.append(",");
				ciSort.append("lower(");
				ciSort.append(field);
				ciSort.append(") ");
				ciSort.append(direction);
			}

			Collections.sort(documents, DocumentComparator.getComparator(ciSort.toString()));
		}
	}

	private void retrieveExtendedAttributesValues(Locale locale, List<String> extendedAttributes,
			String extendedAttributesSpec, final Map<String, Object> extAttributesValues, Long folderId, Long formId)
			throws PersistenceException {
		DocumentDAO dao = DocumentDAO.get();
		if (extendedAttributes.isEmpty())
			return;

		log.debug("Search for extended attributes {}", extendedAttributesSpec);

		StringBuilder query = new StringBuilder(
				"select ld_docid, ld_name, ld_type, ld_stringvalue, ld_intvalue, ld_doublevalue, ld_datevalue, ld_stringvalues ");
		query.append(" from ld_document_ext where ld_docid in (");
		query.append("select D.ld_id from ld_document D where D.ld_deleted=0 ");
		if (folderId != null)
			query.append(" and D.ld_folderid = %d".formatted(folderId));
		if (formId != null)
			query.append(" and D.ld_formid = %d".formatted(formId));
		query.append(
				") and ld_name in ('%s')".formatted(extendedAttributes.stream().collect(Collectors.joining("','"))));

		dao.query(query.toString(), new ExtendedAttributeRowMapper(extAttributesValues, locale), null);
	}

	private String getFilename(HttpServletRequest request) {
		String filename = null;
		if (StringUtils.isNotEmpty(request.getParameter("filename")))
			filename = request.getParameter("filename");
		return filename;
	}

	private Long getFormId(HttpServletRequest request) {
		Long formId = null;
		if (StringUtils.isNotEmpty(request.getParameter("formId")))
			formId = Long.parseLong(request.getParameter("formId"));
		return formId;
	}

	private Long getFolder(Long folderId) throws PersistenceException {
		FolderDAO fDao = FolderDAO.get();
		Folder folder = fDao.findById(folderId);
		if (folder.getFoldRef() != null)
			folderId = folder.getFoldRef();
		return folderId;
	}

	private Long getFolderId(HttpServletRequest request, Session session) throws PersistenceException, IOException {
		FolderDAO fDao = FolderDAO.get();
		Long folderId = null;
		if (StringUtils.isNotEmpty(request.getParameter("folderId")))
			folderId = Long.parseLong(request.getParameter("folderId"));

		if (folderId != null && session != null && !fDao.isReadAllowed(folderId, session.getUserId()))
			throw new IOException(
					String.format("Folder %s is not accessible by user %s", folderId, session.getUsername()));
		return folderId;
	}

	private String prepareExtendedAttributes(HttpServletRequest request, Session session,
			List<String> extendedAttributes) {
		String extAttributeNames = Context.get().getConfig().getProperty(session.getTenantName() + ".search.extattr");
		if (request.getParameter("extattr") != null)
			extAttributeNames = request.getParameter("extattr");
		if (StringUtils.isNotEmpty(extAttributeNames)) {
			StringTokenizer st = new StringTokenizer(extAttributeNames.trim(), ",;");
			while (st.hasMoreElements())
				extendedAttributes.add(st.nextToken().trim());
		}
		return extAttributeNames;
	}

	private Long getHiliteDocId(HttpServletRequest request) {
		Long hiliteDocId = null;
		if (StringUtils.isNotEmpty(request.getParameter("hiliteDocId")))
			hiliteDocId = Long.parseLong(request.getParameter("hiliteDocId"));
		return hiliteDocId;
	}

	private Integer getStatus(HttpServletRequest request) {
		Integer status = null;
		if (StringUtils.isNotEmpty(request.getParameter("status")))
			status = Integer.parseInt(request.getParameter("status"));
		return status;
	}

	private int getPage(HttpServletRequest request) {
		int page = 1;
		if (StringUtils.isNotEmpty(request.getParameter("page")))
			page = Integer.parseInt(request.getParameter("page"));
		return page;
	}
}