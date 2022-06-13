package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.StringTokenizer;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.jdbc.core.RowMapper;

import com.logicaldoc.core.document.AbstractDocument;
import com.logicaldoc.core.document.Bookmark;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.DocumentComparator;
import com.logicaldoc.core.document.dao.DocumentDAO;
import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.metadata.Attribute;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.User;
import com.logicaldoc.core.security.dao.UserDAO;
import com.logicaldoc.i18n.I18N;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.LocaleUtil;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.web.util.ServiceUtil;

/**
 * This servlet is responsible for documents data.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class DocumentsDataServlet extends HttpServlet {

	private static final long serialVersionUID = 1L;

	private static Logger log = LoggerFactory.getLogger(DocumentsDataServlet.class);

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		try {
			Context context = Context.get();
			Session session = ServiceUtil.validateSession(request);
			ContextProperties config = Context.get().getProperties();
			UserDAO udao = (UserDAO) context.getBean(UserDAO.class);
			User user = udao.findById(session.getUserId());
			udao.initialize(user);

			String locale = request.getParameter("locale");
			if (StringUtils.isEmpty(locale))
				locale = user.getLanguage();

			String sort = request.getParameter("sort");

			response.setContentType("text/xml");
			response.setCharacterEncoding("UTF-8");

			// Avoid resource caching
			response.setHeader("Pragma", "no-cache");
			response.setHeader("Cache-Control", "no-store");
			response.setDateHeader("Expires", 0);

			FolderDAO fDao = (FolderDAO) context.getBean(FolderDAO.class);
			DocumentDAO dao = (DocumentDAO) context.getBean(DocumentDAO.class);
			DateFormat df = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSXXX");

			int max = Context.get().getProperties().getInt(session.getTenantName() + ".gui.maxhistories", 100);
			if (StringUtils.isNotEmpty(request.getParameter("max")))
				max = Integer.parseInt(request.getParameter("max"));

			int page = 1;
			if (StringUtils.isNotEmpty(request.getParameter("page")))
				page = Integer.parseInt(request.getParameter("page"));

			Integer status = null;
			if (StringUtils.isNotEmpty(request.getParameter("status")))
				status = Integer.parseInt(request.getParameter("status"));

			Long hiliteDocId = null;
			if (StringUtils.isNotEmpty(request.getParameter("hiliteDocId")))
				hiliteDocId = Long.parseLong(request.getParameter("hiliteDocId"));
			Document hiliteDoc = null;

			PrintWriter writer = response.getWriter();
			writer.write("<list>");

			String sql = "select ld_docid from ld_bookmark where ld_type=" + Bookmark.TYPE_DOCUMENT
					+ " and ld_deleted = 0 and ld_userid = " + session.getUserId();
			@SuppressWarnings("unchecked")
			List<Long> bookmarks = (List<Long>) dao.queryForList(sql, Long.class);

			// The list of documents to be returned
			List<Document> documentRecords = new ArrayList<Document>();

			/*
			 * Retrieve the names of the extended attributes to show
			 */
			String extattrs = config.getProperty(session.getTenantName() + ".search.extattr");
			if (request.getParameter("extattr") != null)
				extattrs = request.getParameter("extattr");
			List<String> attrs = new ArrayList<String>();
			if (StringUtils.isNotEmpty(extattrs)) {
				StringTokenizer st = new StringTokenizer(extattrs.trim(), ",;");
				while (st.hasMoreElements())
					attrs.add(st.nextToken().trim());
			}

			/*
			 * Contains the extended attributes of the documents. The key is
			 * documentId-atttributeName, the value is the attribute value. This
			 * fieldsMap is used to maximize the listing performances.
			 */
			final Map<String, Object> extValues = new HashMap<String, Object>();

			if (status != null && status.intValue() != AbstractDocument.DOC_ARCHIVED) {
				List<Document> docs = dao.findByLockUserAndStatus(session.getUserId(), status);
				int begin = (page - 1) * max;
				int end = Math.min(begin + max - 1, docs.size() - 1);
				for (int i = begin; i <= end; i++) {
					Document doc = docs.get(i);
					if (!fDao.isReadEnabled(doc.getFolder().getId(), session.getUserId()))
						continue;
					documentRecords.add(doc);
				}
			} else if (StringUtils.isNotEmpty(request.getParameter("docIds"))) {
				String[] idsArray = request.getParameter("docIds").split(",");
				for (String id : idsArray) {
					Document doc = null;
					try {
						doc = dao.findById(Long.parseLong(id));
					} catch (Throwable t) {
					}
					if (doc == null || doc.getDeleted() == 1)
						continue;
					if (!fDao.isReadEnabled(doc.getFolder().getId(), session.getUserId()))
						continue;
					documentRecords.add(doc);
				}
			} else {
				/*
				 * Load some filters from the current request
				 */
				Long folderId = null;
				if (StringUtils.isNotEmpty(request.getParameter("folderId")))
					folderId = Long.parseLong(request.getParameter("folderId"));

				if (folderId != null && session != null && !fDao.isReadEnabled(folderId, session.getUserId()))
					throw new Exception(
							String.format("Folder %s is not accessible by user %s", folderId, session.getUsername()));

				// Check if the folderId is an alias
				Folder folder = fDao.findById(folderId);
				if (folder.getFoldRef() != null)
					folderId = folder.getFoldRef();

				Long formId = null;
				if (StringUtils.isNotEmpty(request.getParameter("formId")))
					formId = Long.parseLong(request.getParameter("formId"));

				String filename = null;
				if (StringUtils.isNotEmpty(request.getParameter("filename")))
					filename = request.getParameter("filename");

				if (!attrs.isEmpty()) {
					log.debug("Search for extended attributes {}", extattrs);

					StringBuffer query = new StringBuffer(
							"select ld_docid, ld_name, ld_type, ld_stringvalue, ld_intvalue, ld_doublevalue, ld_datevalue, ld_stringvalues ");
					query.append(" from ld_document_ext where ld_docid in (");
					query.append("select D.ld_id from ld_document D where D.ld_deleted=0 ");
					if (folderId != null)
						query.append(" and D.ld_folderid=" + Long.toString(folderId));
					if (formId != null)
						query.append(" and D.ld_formid=" + Long.toString(formId));
					query.append(") and ld_name in ");
					query.append(attrs.toString().replaceAll("\\[", "('").replaceAll("\\]", "')").replaceAll(",", "','")
							.replaceAll(" ", ""));

					final Locale l = LocaleUtil.toLocale(locale);

					dao.query(query.toString(), null, new RowMapper<Long>() {
						@Override
						public Long mapRow(ResultSet rs, int row) throws SQLException {
							Long docId = rs.getLong(1);
							String name = rs.getString(2);
							int type = rs.getInt(3);

							String key = docId + "-" + name;

							if (type == Attribute.TYPE_STRING) {
								if (StringUtils.isNotEmpty(rs.getString(8)))
									extValues.put(key, rs.getString(8));
								else
									extValues.put(key, rs.getString(4));
							} else if (type == Attribute.TYPE_INT) {
								extValues.put(key, rs.getLong(5));
							} else if (type == Attribute.TYPE_DOUBLE) {
								extValues.put(key, rs.getDouble(6));
							} else if (type == Attribute.TYPE_DATE) {
								extValues.put(key, rs.getTimestamp(7));
							} else if (type == Attribute.TYPE_USER || type == Attribute.TYPE_FOLDER) {
								extValues.put(key, rs.getString(4));
							} else if (type == Attribute.TYPE_BOOLEAN) {
								extValues.put(key,
										rs.getLong(5) == 1L ? I18N.message("true", l) : I18N.message("false", l));
							}

							return null;
						}
					}, null);
				}

				/*
				 * Execute the Query
				 */
				StringBuffer query = new StringBuffer(
						"select A.id, A.customId, A.docRef, A.type, A.version, A.lastModified, A.date, A.publisher,"
								+ " A.creation, A.creator, A.fileSize, A.immutable, A.indexed, A.lockUserId, A.fileName, A.status,"
								+ " A.signed, A.type, A.rating, A.fileVersion, A.comment, A.workflowStatus,"
								+ " A.startPublishing, A.stopPublishing, A.published, A.extResId,"
								+ " B.name, A.docRefType, A.stamped, A.lockUser, A.password, A.pages, "
								+ " A.workflowStatusDisplay, A.language, A.links, A.tgs, A.creatorId, A.publisherId, A.color from Document as A left outer join A.template as B ");
				query.append(" where A.deleted = 0 and not A.status=" + AbstractDocument.DOC_ARCHIVED);
				if (folderId != null)
					query.append(" and A.folder.id=" + folderId);
				if (formId != null)
					query.append(" and A.formId=" + Long.toString(formId));
				if (StringUtils.isNotEmpty(request.getParameter("indexed")))
					query.append(" and A.indexed=" + request.getParameter("indexed"));

				Object[] values = null;
				if (filename != null) {
					query.append(" and lower(A.fileName) like ?1 ");
					values = new Object[] { "%" + filename.toLowerCase() + "%" };
				}

				List<Document> documents = new ArrayList<Document>();
				List<Object> records = new ArrayList<Object>();
				if (folderId != null || filename != null || formId != null
						|| StringUtils.isNotEmpty(request.getParameter("indexed")))
					records = (List<Object>) dao.findByQuery(query.toString(), values, null);

				/*
				 * Iterate over records enriching the data
				 */
				for (int i = 0; i < records.size(); i++) {
					Object[] cols = (Object[]) records.get(i);

					Document doc = new Document();
					doc.setId((Long) cols[0]);
					doc.setDocRef((Long) cols[2]);
					doc.setFileName((String) cols[14]);
					doc.setType((String) cols[17]);
					doc.setDocRefType((String) cols[27]);
					doc.setColor((String) cols[38]);

					if (folderId != null) {
						Folder f = new Folder();
						f.setId(folderId);
						doc.setFolder(f);
					}

					// Replace with the real document if this is an alias
					if (doc.getDocRef() != null && doc.getDocRef().longValue() != 0L) {
						long aliasId = doc.getId();
						long aliasDocRef = doc.getDocRef();
						String aliasDocRefType = doc.getDocRefType();
						String aliasFileName = doc.getFileName();
						String aliasColor = doc.getColor();
						String aliasType = doc.getType();
						doc = dao.findById(aliasDocRef);

						if (doc != null) {
							dao.initialize(doc);
							doc.setId(aliasId);
							doc.setDocRef(aliasDocRef);
							doc.setDocRefType(aliasDocRefType);
							doc.setFileName(aliasFileName);
							doc.setColor(aliasColor);
							doc.setType(aliasType);
							doc.setTemplateName(doc.getTemplate().getName());

							for (String name : attrs) {
								String key = aliasId + "-" + name;
								if (!extValues.containsKey(key) && doc.getValue(name) != null) {
									Attribute att = doc.getAttribute(name);
									if (att != null && (att.getType() == Attribute.TYPE_FOLDER
											|| att.getType() == Attribute.TYPE_USER)) {
										extValues.put(key, att.getStringValue());
										doc.setValue(name, att.getStringValue());
									} else
										extValues.put(key, doc.getValue(name));
								}
							}

						} else
							continue;
					} else {
						doc.setStartPublishing((Date) cols[22]);
						doc.setStopPublishing((Date) cols[23]);
						doc.setPublished((Integer) cols[24]);

						if (doc.isPublishing() || user.isMemberOf("admin") || user.isMemberOf("publisher")) {
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

							if (!extValues.isEmpty())
								for (String name : attrs) {
									Object val = extValues.get(doc.getId() + "-" + name);
									if (val != null) {
										doc.setValue(name, val);
									}
								}
						}
					}

					if (doc.isPublishing() || user.isMemberOf("admin") || user.isMemberOf("publisher"))
						documents.add(doc);
				}

				// If a sorting is specified sort the collection of documents
				if (StringUtils.isNotEmpty(sort)) {
					// make the sorting to be case insensitive (add lower
					// function)
					StringBuffer ciSort = new StringBuffer();
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

				int begin = (page - 1) * max;
				int end = Math.min(begin + max - 1, documents.size() - 1);
				for (int i = begin; i <= end; i++)
					documentRecords.add(documents.get(i));

				// Always add the hilight doc as first element of the collection
				if (hiliteDocId != null) {
					hiliteDoc = dao.findById(hiliteDocId);
					if (hiliteDoc!=null && folderId != null && hiliteDoc != null && hiliteDoc.getFolder() != null
							&& hiliteDoc.getFolder().getId() == folderId) {
						dao.initialize(hiliteDoc);
					} else
						hiliteDoc = null;
				}
				if (hiliteDoc != null) {
					if (!documentRecords.contains(hiliteDoc))
						documentRecords.add(0, hiliteDoc);
				}
			}

			/*
			 * Iterate over the documents printing the output
			 */
			for (Document doc : documentRecords) {
				writer.print("<document>");
				writer.print("<id>" + doc.getId() + "</id>");
				if (doc.getFolder() != null)
					writer.print("<folderId>" + doc.getFolder().getId() + "</folderId>");
				writer.print("<customId><![CDATA[" + (doc.getCustomId() != null ? doc.getCustomId() : "")
						+ "]]></customId>");
				if (doc.getDocRef() != null) {
					writer.print("<docref>" + doc.getDocRef() + "</docref>");
					if (doc.getDocRefType() != null)
						writer.print("<docrefType>" + doc.getDocRefType() + "</docrefType>");
				}

				writer.print("<icon>" + FilenameUtils.getBaseName(doc.getIcon()) + "</icon>");
				writer.print("<version>" + doc.getVersion() + "</version>");
				writer.print("<lastModified>" + (doc.getLastModified() != null ? df.format(doc.getLastModified()) : "")
						+ "</lastModified>");
				writer.print("<published>" + (doc.getDate() != null ? df.format(doc.getDate()) : "") + "</published>");
				writer.print("<publisher><![CDATA[" + doc.getPublisher() + "]]></publisher>");
				writer.print(
						"<created>" + (doc.getCreation() != null ? df.format(doc.getCreation()) : "") + "</created>");
				writer.print("<creator><![CDATA[" + doc.getCreator() + "]]></creator>");
				writer.print("<size>" + doc.getFileSize() + "</size>");
				writer.print("<pages>" + doc.getPages() + "</pages>");

				writer.print("<status>" + doc.getStatus() + "</status>");
				writer.print("<immutable>" + doc.getImmutable() + "</immutable>");
				writer.print("<indexed>" + doc.getIndexed() + "</indexed>");
				writer.print("<password>" + StringUtils.isNotEmpty(doc.getPassword()) + "</password>");
				writer.print("<signed>" + doc.getSigned() + "</signed>");
				writer.print("<stamped>" + doc.getStamped() + "</stamped>");
				writer.print("<bookmarked>" + (bookmarks.contains(doc.getId()) || bookmarks.contains(doc.getDocRef()))
						+ "</bookmarked>");
				writer.print("<language>" + doc.getLanguage() + "</language>");
				writer.print("<links>" + doc.getLinks() + "</links>");
				writer.print("<publisherId>" + doc.getPublisherId() + "</publisherId>");
				writer.print("<creatorId>" + doc.getCreatorId() + "</creatorId>");

				if (doc.getLockUserId() != null)
					writer.print("<lockUserId>" + doc.getLockUserId() + "</lockUserId>");
				if (doc.getLockUser() != null)
					writer.print("<lockUser><![CDATA[" + doc.getLockUser() + "]]></lockUser>");
				writer.print("<filename><![CDATA[" + doc.getFileName() + "]]></filename>");
				writer.print("<type><![CDATA[" + doc.getType() + "]]></type>");

				writer.print("<rating>" + (doc.getRating() != null ? doc.getRating() : "0") + "</rating>");
				writer.print("<fileVersion><![CDATA[" + doc.getFileVersion() + "]]></fileVersion>");
				writer.print(
						"<comment><![CDATA[" + (doc.getComment() != null ? doc.getComment() : "") + "]]></comment>");
				writer.print("<workflowStatus><![CDATA["
						+ (doc.getWorkflowStatus() != null ? doc.getWorkflowStatus() : "") + "]]></workflowStatus>");
				writer.print("<workflowStatusDisplay><![CDATA["
						+ (doc.getWorkflowStatusDisplay() != null ? doc.getWorkflowStatusDisplay() : "")
						+ "]]></workflowStatusDisplay>");

				if (StringUtils.isNotEmpty(doc.getColor()))
					writer.print("<color><![CDATA[" + doc.getColor() + "]]></color>");

				if (doc.getStartPublishing() != null)
					writer.print("<startPublishing>" + df.format(doc.getStartPublishing()) + "</startPublishing>");
				else
					writer.print("<startPublishing></startPublishing>");

				if (doc.getStopPublishing() != null)
					writer.print("<stopPublishing>" + df.format(doc.getStopPublishing()) + "</stopPublishing>");
				else
					writer.print("<stopPublishing></stopPublishing>");

				writer.print("<publishedStatus>" + (doc.isPublishing() ? "yes" : "no") + "</publishedStatus>");

				if (doc.getExtResId() != null)
					writer.print("<extResId><![CDATA[" + doc.getExtResId() + "]]></extResId>");

				if (doc.getTemplateName() != null)
					writer.print("<template><![CDATA[" + doc.getTemplateName() + "]]></template>");

				if (StringUtils.isNotEmpty(doc.getTgs()))
					writer.print(
							"<tags><![CDATA[" + doc.getTgs().substring(1, doc.getTgs().length() - 1) + "]]></tags>");

				if (!extValues.isEmpty())
					for (String name : attrs) {
						Object val = doc.getValue(name);
						if (val != null) {
							writer.print("<ext_" + name + ">");
							if (val instanceof Date)
								writer.print(df.format((Date) val));
							else if (val instanceof Integer)
								writer.print(Integer.toString((Integer) val));
							else if (val instanceof Long)
								writer.print(Long.toString((Long) val));
							else if (val instanceof Double)
								writer.print(Double.toString((Double) val));
							else
								writer.print("<![CDATA[" + val + "]]>");
							writer.print("</ext_" + name + ">");
						}
					}

				if (hiliteDoc != null && doc.getId() == hiliteDoc.getId())
					writer.print("<order>1</order>");

				writer.print("</document>");
			}

			writer.write("</list>");
		} catch (Throwable e) {
			log.error(e.getMessage(), e);
			if (e instanceof ServletException)
				throw (ServletException) e;
			else if (e instanceof IOException)
				throw (IOException) e;
			else
				throw new ServletException(e.getMessage(), e);
		}
	}
}