package com.logicaldoc.core.searchengine;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.stream.Collectors;

import org.apache.commons.lang.StringUtils;
import org.java.plugin.registry.Extension;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.jdbc.core.RowMapper;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.document.AbstractDocument;
import com.logicaldoc.core.document.DocumentDAO;
import com.logicaldoc.core.document.DocumentStatus;
import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.metadata.Attribute;
import com.logicaldoc.core.metadata.Template;
import com.logicaldoc.core.security.TenantDAO;
import com.logicaldoc.core.security.user.Group;
import com.logicaldoc.core.security.user.User;
import com.logicaldoc.core.security.user.UserDAO;
import com.logicaldoc.core.security.user.UserEvent;
import com.logicaldoc.core.security.user.UserHistory;
import com.logicaldoc.core.security.user.UserHistoryDAO;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.util.plugin.PluginRegistry;
import com.logicaldoc.util.spring.Context;

/**
 * This class executes a search against the full-text indexes
 * 
 * @author Michael Scholz
 */
public abstract class Search {
	protected static final Logger log = LoggerFactory.getLogger(Search.class);

	protected boolean moreHitsPresent = false;

	protected SearchOptions options;

	protected List<Hit> hits = new ArrayList<>();

	protected long estimatedHitsNumber = 0;

	protected long execTime = 0;

	protected User searchUser;

	public static Search get(SearchOptions opt) {
		// Acquire the 'Search' extensions of the core plugin
		PluginRegistry registry = PluginRegistry.getInstance();
		Collection<Extension> extensions = new ArrayList<>();
		try {
			extensions = registry.getExtensions("logicaldoc-core", "Search");
		} catch (Exception e) {
			log.error(e.getMessage());
		}

		Search search = null;

		for (Extension ext : extensions) {
			int type = Integer.parseInt(ext.getParameter("type").valueAsString());
			if (type == opt.getType()) {
				String className = ext.getParameter("class").valueAsString();
				try {
					search = (Search) Class.forName(className).getDeclaredConstructor().newInstance();
					search.setOptions(opt);
				} catch (Exception e) {
					log.error(e.getMessage());
				}
				break;
			}
		}

		return search;
	}

	public static SearchOptions newOptions(int type) {
		// Acquire the 'Search' extensions of the core plugin
		PluginRegistry registry = PluginRegistry.getInstance();
		Collection<Extension> extensions = new ArrayList<>();
		try {
			extensions = registry.getExtensions("logicaldoc-core", "Search");
		} catch (Exception e) {
			log.error(e.getMessage());
		}

		SearchOptions options = null;

		for (Extension ext : extensions) {
			int t = Integer.parseInt(ext.getParameter("type").valueAsString());
			if (t == type) {
				String className = ext.getParameter("options").valueAsString();
				try {
					options = (SearchOptions) Class.forName(className).getDeclaredConstructor().newInstance();
					options.setType(type);
				} catch (Exception e) {
					log.error(e.getMessage());
				}
				break;
			}
		}

		if (options == null)
			log.error("Unable to find a search definition of type {}", type);

		return options;
	}

	protected Search() {
	}

	/**
	 * Perform the search
	 * 
	 * @return The list of hits
	 * 
	 * @throws SearchException raised in case of error during the search
	 */
	public final List<Hit> search() throws SearchException {
		log.info("Launch search");
		log.info("Expression: {}", options.getExpression());

		initSearchUser();

		if (searchUser == null) {
			log.warn("Unexisting user");
			return hits;
		}

		Date start = new Date();
		hits.clear();
		moreHitsPresent = false;

		internalSearch();

		ContextProperties config = Context.get().getConfig();
		String extattrs;
		try {
			extattrs = config.getProperty(TenantDAO.get().getTenantName(searchUser.getTenantId()) + ".search.extattr");
		} catch (PersistenceException e) {
			throw new SearchException(e);
		}

		if (StringUtils.isNotEmpty(extattrs) && !hits.isEmpty()) {
			// the names of the extended attributes to show
			List<String> attrs = Arrays.asList(extattrs.trim().split(","));

			StringBuilder idsString = new StringBuilder("(");
			idsString.append(hits.stream().map(h -> Long.toString(h.getId())).collect(Collectors.joining(",")));
			idsString.append(")");

			log.debug("Start searching for extended attributes: {}", attrs);

			// Search for extended attributes, key is docId-name
			final Map<String, Attribute> extAtt = new HashMap<>();

			DocumentDAO ddao = DocumentDAO.get();
			StringBuilder query = new StringBuilder();

			if (hits.get(0).getType().startsWith("folder")) {
				query.append(
						"select ld_folderid, ld_name, ld_type, ld_stringvalue, ld_intvalue, ld_doublevalue, ld_datevalue, ld_stringvalues ");
				query.append(" from ld_folder_ext where ld_folderid in ");
			} else {
				query.append(
						"select ld_docid, ld_name, ld_type, ld_stringvalue, ld_intvalue, ld_doublevalue, ld_datevalue, ld_stringvalues ");
				query.append(" from ld_document_ext where ld_docid in ");
			}

			query.append(idsString);
			query.append(" and ld_name in ");
			query.append(attrs.toString().replace("[", "('").replace("]", "')").replace(",", "','").replace(" ", ""));

			try {
				ddao.query(query.toString(), new RowMapper<Long>() {
					@Override
					public Long mapRow(ResultSet rs, int row) throws SQLException {
						Long docId = rs.getLong(1);
						String name = rs.getString(2);

						Attribute ext = new Attribute();
						ext.setName(name);
						ext.setStringValue(rs.getString(4));
						ext.setIntValue(rs.getLong(5));
						ext.setDoubleValue(rs.getDouble(6));
						ext.setDateValue(rs.getDate(7));
						ext.setStringValues(rs.getString(8));
						ext.setType(rs.getInt(3));
						extAtt.put(docId + "-" + name, ext);
						return null;
					}
				}, null);
			} catch (PersistenceException e) {
				log.error(e.getMessage(), e);
			}

			copyExtendedAttributesToHits(attrs, extAtt);

			log.debug("End searching for extended attributes");
		}

		Date finish = new Date();
		execTime = finish.getTime() - start.getTime();
		log.info("Search completed in {} ms and found {} hits (estimated {})", execTime, hits.size(),
				estimatedHitsNumber);
		UserHistory transaction = options.getTransaction();
		if (transaction == null)
			transaction = new UserHistory();
		transaction.setUser(searchUser);
		transaction.setComment(StringUtils.left(options.toString(), 500));
		transaction.setEvent(UserEvent.SEARCH);
		try {
			UserHistoryDAO.get().store(transaction);
		} catch (PersistenceException e) {
			log.info("Error trying to save search history", e);
		}
		return hits;
	}

	protected Collection<Long> getAccessibleFolderIds() throws SearchException {
		FolderDAO fdao = FolderDAO.get();

		/*
		 * We have to see what folders the user can access. But we need to
		 * perform this check only if the search is not restricted to one folder
		 * only.
		 */
		Collection<Long> accessibleFolderIds = new TreeSet<>();
		boolean searchInSingleFolder = (options.getFolderId() != null && !options.isSearchInSubPath());
		if (!searchInSingleFolder) {
			try {
				log.debug("Accessible folders search");
				if (options.getFolderId() != null)
					accessibleFolderIds = fdao.findFolderIdByUserIdInPath(options.getUserId(), options.getFolderId());
				else
					accessibleFolderIds = fdao.findFolderIdByUserId(options.getUserId(), null, true);
				log.debug("End of accessible folders search");
			} catch (PersistenceException e) {
				throw new SearchException(e);
			}
		}
		return accessibleFolderIds;
	}

	/**
	 * Retrieves the ids of those document inside a hits collection that cannot
	 * be accessed by the search user
	 * 
	 * @param hits The hists resulting from the search
	 * @param accessibleFolderIds The ids of the folders accessible by the user
	 * 
	 * @return A collection of document IDs not accessible by the user
	 * 
	 * @throws SearchException error in the data layer
	 */
	protected Set<Long> getDeniedDocIds(List<Hit> hits, Collection<Long> accessibleFolderIds) throws SearchException {
		HashSet<Long> denied = new HashSet<>();
		if (searchUser.isAdmin() || hits.isEmpty())
			return denied;

		DocumentDAO dao = DocumentDAO.get();

		// Detect those hits outside the accessible folders
		for (Hit hit : hits)
			if (accessibleFolderIds != null && !accessibleFolderIds.contains(hit.getFolder().getId()))
				denied.add(hit.getId());

		// Detect those hits with specific read prohibition for the search user.
		try {
			StringBuilder query = new StringBuilder(
					"select ld_docid from ld_document_acl where ld_read=0 and ld_docid in (");
			query.append(hits.stream().map(h -> Long.toString(h.getId())).collect(Collectors.joining(",")));
			query.append(") and ld_groupid in (");
			query.append(searchUser.getGroups().stream().map(g -> Long.toString(g.getId()))
					.collect(Collectors.joining(",")));
			query.append(") ");
			if (!denied.isEmpty()) {
				// skip those docs already marked as denied
				query.append(" and not ld_docid in (");
				query.append(denied.stream().map(Object::toString).collect(Collectors.joining(",")));
				query.append(")");
			}
			denied.addAll(dao.queryForList(query.toString(), Long.class));
		} catch (PersistenceException e) {
			throw new SearchException(e.getMessage(), e);
		}

		return denied;

	}

	private void copyExtendedAttributesToHits(List<String> atributeNames, final Map<String, Attribute> extAttribute) {
		for (Hit h : hits) {
			for (String name : atributeNames) {
				Attribute att = extAttribute.get(h.getId() + "-" + name);
				if (h.getDocRef() != null && h.getDocRef().longValue() != 0L)
					att = extAttribute.get(h.getDocRef() + "-" + name);
				if (att != null)
					h.getAttributes().put(name, att);
			}
		}
	}

	private void initSearchUser() throws SearchException {
		UserDAO uDao = UserDAO.get();
		try {
			searchUser = uDao.findById(options.getUserId());
			uDao.initialize(searchUser);
		} catch (PersistenceException e1) {
			throw new SearchException(e1);
		}

		if (searchUser != null && log.isInfoEnabled())
			log.info("Search User: {}", searchUser.getUsername());
	}

	/**
	 * Builds a SQL condition restricting results to the specified document IDs.
	 * 
	 * @param hitsIds The set of document IDs to include in the SQL filter.
	 * @param idColumnExpression The SQL column/expression representing the
	 *        document ID to compare against.
	 * @return A SQL fragment implementing the ID restriction
	 */
	protected String buildHitsIdsCondition(Set<Long> hitsIds, String idColumnExpression) {
		if (hitsIds == null || hitsIds.isEmpty()) {
			return "";
		}

		StringBuilder condition = new StringBuilder(" and (");
		FolderDAO folderDao = FolderDAO.get();

		if (folderDao.isOracle()) { // @formatter:off
			condition.append(" (")
			         .append(idColumnExpression)
			         .append(",0) in ( ");

			String inList = hitsIds.stream()
			                       .map(id -> "(" + id + ",0)")
			                       .collect(Collectors.joining(","));

			condition.append(inList).append(" )");
		} else {
			// Default: standard IN clause
			String inList = hitsIds.toString().replace('[', '(').replace(']', ')');
			condition.append(" ")
			         .append(idColumnExpression)
			         .append(" in ")
			         .append(inList); // @formatter:on
		}

		condition.append(")");
		return condition.toString();
	}

	/**
	 * Executes a rich SQL query to load full document metadata for the given
	 * hit IDs, maps the results to {@link Hit} objects
	 *
	 * @param opt the search options driving the current search
	 * @param hitsMap a map of hits keyed by document ID; it may already contain
	 *        partially-filled hits (e.g. with scores) and will be enriched with
	 *        DB metadata
	 * @param tenantId the tenant identifier to restrict the query to
	 * @param accessibleFolderIds the set of folder IDs the current user can
	 *        access
	 * @throws SearchException if a persistence error occurs
	 */
	protected void enrichAndPopulateHits(SearchOptions opt, Map<Long, Hit> hitsMap, long tenantId,
			Collection<Long> accessibleFolderIds) throws SearchException {
		if (hitsMap.isEmpty())
			return;

		Set<Long> hitsIds = hitsMap.keySet();

		// Build the condition on IDs
		String hitsIdsCondition = buildHitsIdsCondition(hitsIds, "A.ld_id");

		// Build the main rich-query SQL
		StringBuilder richQuery = new StringBuilder(
				"select A.ld_id, A.ld_customid, A.ld_docref, A.ld_type, A.ld_version, A.ld_lastmodified, ");
		richQuery.append(" A.ld_date, A.ld_publisher, A.ld_creation, A.ld_creator, A.ld_filesize, A.ld_immutable, ");
		richQuery.append(" A.ld_indexed, A.ld_lockuserid, A.ld_filename, A.ld_status, A.ld_signed, A.ld_type, ");
		richQuery.append(" A.ld_rating, A.ld_fileversion, A.ld_comment, A.ld_workflowstatus, A.ld_startpublishing, ");
		richQuery.append(" A.ld_stoppublishing, A.ld_published, ");
		richQuery.append(
				" FOLD.ld_name, A.ld_folderid, A.ld_tgs tags, A.ld_templateid, C.ld_name, A.ld_tenantid, A.ld_docreftype, ");
		richQuery.append(
				" A.ld_stamped, A.ld_password, A.ld_workflowstatusdisp, A.ld_language, A.ld_pages, A.ld_color, A.ld_lastnote, A.ld_revision ");
		richQuery.append(" from ld_document A ");
		richQuery.append(" join ld_folder FOLD on A.ld_folderid=FOLD.ld_id ");
		richQuery.append(" left outer join ld_template C on A.ld_templateid=C.ld_id ");
		richQuery.append(" where A.ld_deleted=0 and not A.ld_status=" + DocumentStatus.ARCHIVED.ordinal()
				+ " and A.ld_nature=" + AbstractDocument.NATURE_DOC + " and A.ld_folderid=FOLD.ld_id  ");
		richQuery.append(" and A.ld_tenantid = " + tenantId);
		// For normal users we have to exclude not published documents
		if (searchUser != null && !searchUser.isMemberOf(Group.GROUP_ADMIN) && !searchUser.isMemberOf("publisher")) {
			richQuery.append(" and A.ld_published = 1 ");
			richQuery.append(" and A.ld_startpublishing <= CURRENT_TIMESTAMP ");
			richQuery.append(" and ( A.ld_stoppublishing is null or A.ld_stoppublishing > CURRENT_TIMESTAMP )");
		}
		richQuery.append("  and A.ld_docref is null ");
		richQuery.append(hitsIdsCondition);

		// Aliases UNION
		if (options.isRetrieveAliases()) {
			String docRefCondition = buildHitsIdsCondition(hitsIds, "A.ld_docref");

			richQuery.append(
					" UNION select A.ld_id, REF.ld_customid, A.ld_docref, REF.ld_type, REF.ld_version, REF.ld_lastmodified, ");
			richQuery.append(
					" REF.ld_date, REF.ld_publisher, REF.ld_creation, REF.ld_creator, REF.ld_filesize, REF.ld_immutable, ");
			richQuery.append(
					" REF.ld_indexed, REF.ld_lockuserid, A.ld_filename, REF.ld_status, REF.ld_signed, REF.ld_type, ");
			richQuery.append(
					" REF.ld_rating, REF.ld_fileversion, A.ld_comment, REF.ld_workflowstatus, REF.ld_startpublishing, ");
			richQuery.append(" A.ld_stoppublishing, A.ld_published, ");
			richQuery.append(
					" FOLD.ld_name, A.ld_folderid, A.ld_tgs tags, REF.ld_templateid, C.ld_name, A.ld_tenantid, A.ld_docreftype, ");
			richQuery.append(
					" REF.ld_stamped, REF.ld_password, REF.ld_workflowstatusdisp, REF.ld_language, REF.ld_pages, A.ld_color, A.ld_lastnote, A.ld_revision ");
			richQuery.append(" from ld_document A  ");
			richQuery.append(" join ld_folder FOLD on A.ld_folderid=FOLD.ld_id ");
			richQuery.append(" join ld_document REF on A.ld_docref=REF.ld_id ");
			richQuery.append(" left outer join ld_template C on REF.ld_templateid=C.ld_id ");
			richQuery.append(" where A.ld_deleted=0 and not A.ld_status=" + DocumentStatus.ARCHIVED.ordinal()
					+ " and A.ld_nature=" + AbstractDocument.NATURE_DOC + " and A.ld_folderid=FOLD.ld_id ");
			richQuery.append(" and A.ld_tenantid = " + tenantId);
			// For normal users we have to exclude not published documents
			if (searchUser != null && !searchUser.isMemberOf(Group.GROUP_ADMIN)
					&& !searchUser.isMemberOf("publisher")) {
				richQuery.append(" and REF.ld_published = 1 ");
				richQuery.append(" and REF.ld_startpublishing <= CURRENT_TIMESTAMP ");
				richQuery.append(" and ( REF.ld_stoppublishing is null or REF.ld_stoppublishing > CURRENT_TIMESTAMP )");
			}
			richQuery.append("  and A.ld_docref is not null and REF.ld_deleted=0 and not A.ld_status="
					+ DocumentStatus.ARCHIVED.ordinal() + " and A.ld_docref = REF.ld_id ");
			richQuery.append(docRefCondition);
		}

		log.debug("Execute query {}", richQuery);

		DocumentDAO dao = DocumentDAO.get();
		try {
			dao.query(richQuery.toString(), new HitMapper(hitsMap), null);
		} catch (PersistenceException e) {
			throw new SearchException(e);
		}

		// Sort and populate hits
		List<Hit> sortedHitsList = new ArrayList<>(hitsMap.values());
		Collections.sort(sortedHitsList);

		populateHits(opt, accessibleFolderIds, sortedHitsList);
	}

	/**
	 * Populates the final {@link #hits} list from the given sorted hits,
	 * applying folder permissions and max-hits limits.
	 * 
	 * @param opt The search options
	 * @param accessibleFolderIds The set of folder IDs the current user is
	 *        allowed to access.
	 * @param sortedHitsList The relevance-sorted list of candidate hits to
	 *        filter and populate.
	 * @throws SearchException If permission checks fail.
	 */
	protected void populateHits(SearchOptions opt, Collection<Long> accessibleFolderIds, List<Hit> sortedHitsList)
			throws SearchException {

		Set<Long> forbiddenDocs = getDeniedDocIds(sortedHitsList, accessibleFolderIds);

		for (Hit hit : sortedHitsList) {
			if (options.getMaxHits() > 0 && hits.size() >= options.getMaxHits()) {
				// The maximum number of hits was reached
				moreHitsPresent = true;
				break;
			}
			if (StringUtils.isNotEmpty(hit.getFileName())
					&& ((searchUser.isMemberOf(Group.GROUP_ADMIN) && opt.getFolderId() == null)
							|| (accessibleFolderIds != null && accessibleFolderIds.contains(hit.getFolder().getId())))
					&& !forbiddenDocs.contains(hit.getId())) {
				hits.add(hit);
			}
		}
	}

	public static class HitMapper implements RowMapper<Hit> {

		private final Map<Long, Hit> hitsMap;

		public HitMapper(Map<Long, Hit> hitsMap) {
			this.hitsMap = hitsMap;
		}

		@Override
		public Hit mapRow(ResultSet rs, int rowNum) throws SQLException {
			Hit hit = hitsMap.get(rs.getLong(1));
			if (hit == null) {
				// This is an alias
				hit = new Hit();
				hitsMap.put(rs.getLong(1), hit);
			}

			hit.setId(rs.getLong(1));
			hit.setCustomId(rs.getString(2));
			if (rs.getLong(3) != 0L) {
				hit.setDocRef(rs.getLong(3));
				Hit master = hitsMap.get(rs.getLong(3));
				if (master != null) {
					hit.setContent(master.getContent());
					hit.setSummary(master.getSummary());
				}
				hit.setDocRefType(rs.getString(33));
			}
			hit.setType(rs.getString(4));
			hit.setVersion(rs.getString(5));
			hit.setLastModified(rs.getTimestamp(6));
			hit.setDate(rs.getTimestamp(7));
			hit.setPublisher(rs.getString(8));
			hit.setCreation(rs.getTimestamp(9));
			hit.setCreator(rs.getString(10));
			hit.setFileSize(rs.getLong(11));
			hit.setImmutable(rs.getInt(12) == 1);
			hit.setIndexingStatus(rs.getInt(13));
			hit.setLockUserId(rs.getLong(14));
			hit.setFileName(rs.getString(15));
			hit.setStatus(rs.getInt(16));
			hit.setSigned(rs.getInt(17) == 1);
			hit.setType(rs.getString(18));
			hit.setRating(rs.getInt(19));
			hit.setFileVersion(rs.getString(20));
			hit.setComment(rs.getString(21));
			hit.setWorkflowStatus(rs.getString(22));
			hit.setStartPublishing(rs.getTimestamp(23));
			hit.setStopPublishing(rs.getTimestamp(24));
			hit.setPublished(rs.getInt(25) == 1);

			Folder folder = new Folder();
			folder.setName(rs.getString(26));
			folder.setId(rs.getLong(27));
			hit.setFolder(folder);

			if (rs.getLong(29) != 0L) {
				Template t = new Template();
				t.setId(rs.getLong(29));
				t.setName(rs.getString(30));
				hit.setTemplate(t);
				hit.setTemplateId(t.getId());
			}

			hit.setTenantId(rs.getLong(31));
			hit.setStamped(rs.getInt(33) == 1);
			hit.setPassword(rs.getString(34));
			hit.setWorkflowStatusDisplay(rs.getString(35));
			hit.setLanguage(rs.getString(36));
			hit.setPages(rs.getInt(37));
			hit.setColor(rs.getString(38));
			hit.setLastNote(rs.getString(39));
			hit.setRevision(rs.getString(40));

			return hit;
		}
	}

	/**
	 * Concrete implementations must give a particular search algorithm that
	 * populates the hits list.
	 */
	protected abstract void internalSearch() throws SearchException;

	public List<Hit> getHits() {
		return hits;
	}

	public boolean isMoreHitsPresent() {
		return moreHitsPresent;
	}

	public void setMoreHitsPresent(boolean moreHitsPresent) {
		this.moreHitsPresent = moreHitsPresent;
	}

	public long getEstimatedHitsNumber() {
		return estimatedHitsNumber;
	}

	/**
	 * Query execution time in milliseconds
	 * 
	 * @return the execution time expressed in milliseconds
	 */
	public long getExecTime() {
		return execTime;
	}

	public SearchOptions getOptions() {
		return options;
	}

	public void setOptions(SearchOptions options) {
		this.options = options;
	}
}