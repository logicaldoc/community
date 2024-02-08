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

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.lang.StringUtils;
import org.springframework.jdbc.core.RowMapper;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.document.AbstractDocument;
import com.logicaldoc.core.document.Bookmark;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.DocumentComparator;
import com.logicaldoc.core.document.dao.DocumentDAO;
import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.metadata.Attribute;
import com.logicaldoc.core.security.Group;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.User;
import com.logicaldoc.core.security.dao.UserDAO;
import com.logicaldoc.i18n.I18N;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.io.FileUtil;

/**
 * This servlet is responsible for documents data.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class DocumentsDataServlet extends AbstractDataServlet {

	private static final String INDEXED = "indexed";

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

			String key = docId + "-" + name;

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

		DocumentDAO dao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);

		int page = getPage(request);

		Integer status = getStatus(request);

		PrintWriter writer = response.getWriter();
		writer.write("<list>");

		String sql = "select ld_docid from ld_bookmark where ld_type=" + Bookmark.TYPE_DOCUMENT
				+ " and ld_deleted = 0 and ld_userid = " + session.getUserId();
		@SuppressWarnings("unchecked")
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

		if (status != null && status.intValue() != AbstractDocument.DOC_ARCHIVED) {
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
		writer.print("<id>" + document.getId() + "</id>");

		printFolderAndDocRef(writer, document);

		writer.print("<icon>" + FileUtil.getBaseName(document.getIcon()) + "</icon>");
		writer.print("<version>" + document.getVersion() + "</version>");

		printDates(writer, document);

		writer.print("<creator><![CDATA[" + document.getCreator() + "]]></creator>");

		writer.print("<size>" + document.getFileSize() + "</size>");
		writer.print("<pages>" + document.getPages() + "</pages>");

		writer.print("<status>" + document.getStatus() + "</status>");
		writer.print("<immutable>" + document.getImmutable() + "</immutable>");
		writer.print("<indexed>" + document.getIndexed() + "</indexed>");
		writer.print("<password>" + StringUtils.isNotEmpty(document.getPassword()) + "</password>");
		writer.print("<signed>" + document.getSigned() + "</signed>");
		writer.print("<stamped>" + document.getStamped() + "</stamped>");
		writer.print("<bookmarked>" + (bookmarks.contains(document.getId()) || bookmarks.contains(document.getDocRef()))
				+ "</bookmarked>");
		writer.print("<language>" + document.getLanguage() + "</language>");
		writer.print("<links>" + (document.getLinks()
				+ (Context.get().getProperties().getBoolean(tenant + ".gui.showdocattrsaslinks", false)
						? document.getDocAttrs()
						: 0))
				+ "</links>");
		writer.print("<publisherId>" + document.getPublisherId() + "</publisherId>");
		writer.print("<creatorId>" + document.getCreatorId() + "</creatorId>");
		writer.print("<tenantId>" + document.getTenantId() + "</tenantId>");

		printLockUser(writer, document);

		writer.print("<filename><![CDATA[" + document.getFileName() + "]]></filename>");
		writer.print("<type><![CDATA[" + document.getType() + "]]></type>");

		writer.print("<rating>" + (document.getRating() != null ? document.getRating() : "0") + "</rating>");
		writer.print("<fileVersion><![CDATA[" + document.getFileVersion() + "]]></fileVersion>");
		writer.print(
				"<comment><![CDATA[" + (document.getComment() != null ? document.getComment() : "") + "]]></comment>");
		writer.print("<workflowStatus><![CDATA["
				+ (document.getWorkflowStatus() != null ? document.getWorkflowStatus() : "") + "]]></workflowStatus>");
		writer.print("<workflowStatusDisplay><![CDATA["
				+ (document.getWorkflowStatusDisplay() != null ? document.getWorkflowStatusDisplay() : "")
				+ "]]></workflowStatusDisplay>");

		if (StringUtils.isNotEmpty(document.getColor()))
			writer.print("<color><![CDATA[" + document.getColor() + "]]></color>");

		writer.print("<publishedStatus>" + (document.isPublishing() ? "yes" : "no") + "</publishedStatus>");

		if (document.getExtResId() != null)
			writer.print("<extResId><![CDATA[" + document.getExtResId() + "]]></extResId>");

		if (document.getTemplateName() != null)
			writer.print("<template><![CDATA[" + document.getTemplateName() + "]]></template>");

		if (StringUtils.isNotEmpty(document.getTgs()))
			writer.print(
					"<tags><![CDATA[" + document.getTgs().substring(1, document.getTgs().length() - 1) + "]]></tags>");

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
					writer.print("<ext_" + name + ">");
					if (val instanceof Date date)
						writer.print(df.format(date));
					else if (val instanceof Integer intVal)
						writer.print(Integer.toString(intVal));
					else if (val instanceof Long longVal)
						writer.print(Long.toString(longVal));
					else if (val instanceof Double doubleVal)
						writer.print(Double.toString(doubleVal));
					else
						writer.print("<![CDATA[" + val + "]]>");
					writer.print("</ext_" + name + ">");
				}
			}
	}

	private void printDates(PrintWriter writer, Document document) {
		DateFormat df = getDateFormat();
		writer.print(
				"<lastModified>" + (document.getLastModified() != null ? df.format(document.getLastModified()) : "")
						+ "</lastModified>");
		writer.print(
				"<published>" + (document.getDate() != null ? df.format(document.getDate()) : "") + "</published>");
		writer.print("<publisher><![CDATA[" + document.getPublisher() + "]]></publisher>");
		writer.print(
				"<created>" + (document.getCreation() != null ? df.format(document.getCreation()) : "") + "</created>");
		if (document.getStartPublishing() != null)
			writer.print("<startPublishing>" + df.format(document.getStartPublishing()) + "</startPublishing>");
		else
			writer.print("<startPublishing></startPublishing>");

		if (document.getStopPublishing() != null)
			writer.print("<stopPublishing>" + df.format(document.getStopPublishing()) + "</stopPublishing>");
		else
			writer.print("<stopPublishing></stopPublishing>");
	}

	private void printLockUser(PrintWriter writer, Document document) {
		if (document.getLockUserId() != null)
			writer.print("<lockUserId>" + document.getLockUserId() + "</lockUserId>");
		if (document.getLockUser() != null)
			writer.print("<lockUser><![CDATA[" + document.getLockUser() + "]]></lockUser>");
	}

	private void printFolderAndDocRef(PrintWriter writer, Document document) {
		if (document.getFolder() != null)
			writer.print("<folderId>" + document.getFolder().getId() + "</folderId>");
		writer.print("<customId><![CDATA[" + (document.getCustomId() != null ? document.getCustomId() : "")
				+ "]]></customId>");
		if (document.getDocRef() != null) {
			writer.print("<docref>" + document.getDocRef() + "</docref>");
			if (document.getDocRefType() != null)
				writer.print("<docrefType>" + document.getDocRefType() + "</docrefType>");
		}
	}

	private void findDocumentsByIds(HttpServletRequest request, Session session, List<Document> documentsInCurrentPage)
			throws PersistenceException {
		DocumentDAO dao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);

		String[] idsArray = request.getParameter("docIds").split(",");
		for (String id : idsArray) {
			Document doc = null;
			try {
				doc = dao.findById(Long.parseLong(id));
			} catch (Exception t) {
				// Nothing to do
			}
			if (doc == null || doc.getDeleted() == 1 || !dao.isReadEnabled(Long.parseLong(id), session.getUserId()))
				continue;
			documentsInCurrentPage.add(doc);
		}
	}

	private void findDocumentsByStatus(Session session, int maxRecords, int page, Integer status,
			List<Document> documentsInCurrentPage) throws PersistenceException {
		DocumentDAO dao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);

		List<Document> docs = dao.findByLockUserAndStatus(session.getUserId(), status);
		int begin = (page - 1) * maxRecords;
		int end = Math.min(begin + maxRecords - 1, docs.size() - 1);
		for (int i = begin; i <= end; i++) {
			Document doc = docs.get(i);
			if (!dao.isReadEnabled(doc.getId(), session.getUserId()))
				continue;
			documentsInCurrentPage.add(doc);
		}
	}

	private Document findDocumentsByFilters(HttpServletRequest request, Session session, Locale locale, int maxRecords,
			int page, List<Document> documentsInCurrentPage, List<String> extendedAttributes,
			String extendedAttributesSpec, final Map<String, Object> extendedAttributesValues)
			throws PersistenceException, IOException {

		Context context = Context.get();
		UserDAO udao = (UserDAO) context.getBean(UserDAO.class);
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
		List<Document> documents = exeucuteQuey(request, extendedAttributes, extendedAttributesValues, sessionUser,
				folderId, formId, filename);

		// If a sorting is specified sort the collection of documents
		sortDocuments(documents, sort);

		takeDocumentsInCurrentPage(documents, documentsInCurrentPage, maxRecords, page);

		Long hiliteDocId = getHiliteDocId(request);

		return retrieveHiliteDoc(documentsInCurrentPage, folderId, hiliteDocId);
	}

	private List<Document> exeucuteQuey(HttpServletRequest request, List<String> extendedAttributes,
			final Map<String, Object> extendedAttributesValues, User user, Long folderId, Long formId, String filename)
			throws PersistenceException {

		DocumentDAO dao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		StringBuilder query = new StringBuilder(
				"""
								select A.id, A.customId, A.docRef, A.type, A.version, A.lastModified, A.date, A.publisher,
								       A.creation, A.creator, A.fileSize, A.immutable, A.indexed, A.lockUserId, A.fileName, A.status,
								       A.signed, A.type, A.rating, A.fileVersion, A.comment, A.workflowStatus,
								       A.startPublishing, A.stopPublishing, A.published, A.extResId,
								       B.name, A.docRefType, A.stamped, A.lockUser, A.password, A.pages,
								       A.workflowStatusDisplay, A.language, A.links+A.docAttrs, A.tgs, A.creatorId,
								       A.publisherId, A.color, A.folder.id, A.tenantId
								  from Document as A
								  left outer join A.template as B
						         where A.deleted = 0
						           and not A.status=
						""");
		query.append(AbstractDocument.DOC_ARCHIVED);
		
		if (folderId != null) {
			query.append(" and A.folder.id=" + folderId);
			
			List<Long> forbiddenDocIds = getForbiddenDocumentIds(user, folderId);
			if (!forbiddenDocIds.isEmpty()) {
				query.append(" and A.id not in(");
				query.append(forbiddenDocIds.stream().map(id -> Long.toString(id)).collect(Collectors.joining(",")));
				query.append(") ");
			}
		}

		if (formId != null)
			query.append(" and A.formId=" + Long.toString(formId));
		if (StringUtils.isNotEmpty(request.getParameter(INDEXED)))
			query.append(" and A.indexed=" + request.getParameter(INDEXED));

		Map<String, Object> params = new HashMap<>();
		if (filename != null) {
			query.append(" and lower(A.fileName) like :fileName ");
			params.put("fileName", "%" + filename.toLowerCase() + "%");
		}

		List<Object> records = new ArrayList<>();
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
		if (user.isMemberOf("admin"))
			return new ArrayList<>();

		DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		String groupIdsString = user.getGroups().stream().map(g -> Long.toString(g.getId()))
				.collect(Collectors.joining(","));

		StringBuilder forbiddenDocsQuery = new StringBuilder("""
							select ld_docid
						      from ld_document_acl, ld_document
				             where ld_docid=ld_id
				               and ld_deleted = 0
				               and ld_read = 0
				               and ld_groupId in(
				""");
		forbiddenDocsQuery.append(groupIdsString);
		forbiddenDocsQuery.append(") and ld_folderid = ");
		forbiddenDocsQuery.append(Long.toString(folderId));

		@SuppressWarnings("unchecked")
		List<Long> forbiddenDocIds = (List<Long>) docDao.queryForList(forbiddenDocsQuery.toString(), Long.class);
		return forbiddenDocIds;
	}

	private List<Document> enrichRecords(List<Object> records, List<String> extendedAttributes,
			final Map<String, Object> extendedAttributesValues, User user) throws PersistenceException {

		DocumentDAO dao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);

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
			doc.setIndexed((Integer) cols[12]);

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
				int aliasIndexed = doc.getIndexed();
				doc = dao.findById(aliasDocRef);

				if (doc != null) {
					dao.initialize(doc);
					doc.setId(aliasId);
					doc.setDocRef(aliasDocRef);
					doc.setDocRefType(aliasDocRefType);
					doc.setFileName(aliasFileName);
					doc.setColor(aliasColor);
					doc.setType(aliasType);
					doc.setIndexed(aliasIndexed);

					enrichAliasExtendedAttributes(doc, aliasId, extendedAttributes, extendedAttributesValues);
				} else
					continue;
			} else {
				doc.setStartPublishing((Date) cols[22]);
				doc.setStopPublishing((Date) cols[23]);
				doc.setPublished((Integer) cols[24]);

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
			doc.setImmutable((Integer) cols[11]);
			doc.setIndexed((Integer) cols[12]);
			doc.setLockUserId((Long) cols[13]);
			doc.setStatus((Integer) cols[15]);
			doc.setSigned((Integer) cols[16]);
			doc.setRating((Integer) cols[18]);
			doc.setFileVersion((String) cols[19]);
			doc.setComment((String) cols[20]);
			doc.setWorkflowStatus((String) cols[21]);
			doc.setExtResId((String) cols[25]);
			doc.setTemplateName((String) cols[26]);
			doc.setStamped((Integer) cols[28]);
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

		DocumentDAO dao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
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
		DocumentDAO dao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
		if (extendedAttributes.isEmpty())
			return;

		log.debug("Search for extended attributes {}", extendedAttributesSpec);

		StringBuilder query = new StringBuilder(
				"select ld_docid, ld_name, ld_type, ld_stringvalue, ld_intvalue, ld_doublevalue, ld_datevalue, ld_stringvalues ");
		query.append(" from ld_document_ext where ld_docid in (");
		query.append("select D.ld_id from ld_document D where D.ld_deleted=0 ");
		if (folderId != null)
			query.append(" and D.ld_folderid=" + Long.toString(folderId));
		if (formId != null)
			query.append(" and D.ld_formid=" + Long.toString(formId));
		query.append(") and ld_name in ");
		query.append(extendedAttributes.toString().replace("[", "('").replace("]", "')").replace(",", "','")
				.replace(" ", ""));

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
		FolderDAO fDao = (FolderDAO) Context.get().getBean(FolderDAO.class);
		Folder folder = fDao.findById(folderId);
		if (folder.getFoldRef() != null)
			folderId = folder.getFoldRef();
		return folderId;
	}

	private Long getFolderId(HttpServletRequest request, Session session) throws PersistenceException, IOException {
		FolderDAO fDao = (FolderDAO) Context.get().getBean(FolderDAO.class);
		Long folderId = null;
		if (StringUtils.isNotEmpty(request.getParameter("folderId")))
			folderId = Long.parseLong(request.getParameter("folderId"));

		if (folderId != null && session != null && !fDao.isReadEnabled(folderId, session.getUserId()))
			throw new IOException(
					String.format("Folder %s is not accessible by user %s", folderId, session.getUsername()));
		return folderId;
	}

	private String prepareExtendedAttributes(HttpServletRequest request, Session session,
			List<String> extendedAttributes) {
		String extAttributeNames = Context.get().getProperties()
				.getProperty(session.getTenantName() + ".search.extattr");
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