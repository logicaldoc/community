package com.logicaldoc.core.searchengine;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
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
import com.logicaldoc.core.document.DocumentDAO;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.metadata.Attribute;
import com.logicaldoc.core.security.TenantDAO;
import com.logicaldoc.core.security.user.User;
import com.logicaldoc.core.security.user.UserDAO;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.util.plugin.PluginRegistry;

/**
 * This class executes a search against the full-text indexes
 * 
 * @author Michael Scholz
 */
public abstract class Search {
	protected static Logger log = LoggerFactory.getLogger(Search.class);

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

		ContextProperties config = Context.get().getProperties();
		TenantDAO tdao = (TenantDAO) Context.get().getBean(TenantDAO.class);
		String extattrs = config.getProperty(tdao.getTenantName(searchUser.getTenantId()) + ".search.extattr");

		if (StringUtils.isNotEmpty(extattrs) && !hits.isEmpty()) {
			// the names of the extended attributes to show
			List<String> attrs = Arrays.asList(extattrs.trim().split(","));

			StringBuilder idsString = new StringBuilder("(");
			idsString.append(hits.stream().map(h -> Long.toString(h.getId())).collect(Collectors.joining(",")));
			idsString.append(")");

			log.debug("Start searching for extended attributes: {}", attrs);

			// Search for extended attributes, key is docId-name
			final Map<String, Attribute> extAtt = new HashMap<>();

			DocumentDAO ddao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
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

		return hits;
	}

	protected Collection<Long> getAccessibleFolderIds() throws SearchException {
		FolderDAO fdao = (FolderDAO) Context.get().getBean(FolderDAO.class);

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
	@SuppressWarnings("unchecked")
	protected Set<Long> getDeniedDocIds(List<Hit> hits, Collection<Long> accessibleFolderIds) throws SearchException {
		HashSet<Long> denied = new HashSet<>();
		if (searchUser.isAdmin() || hits.isEmpty())
			return denied;

		DocumentDAO dao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);

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
		UserDAO uDao = (UserDAO) Context.get().getBean(UserDAO.class);
		try {
			searchUser = uDao.findById(options.getUserId());
		} catch (PersistenceException e1) {
			throw new SearchException(e1);
		}
		if (searchUser != null) {
			uDao.initialize(searchUser);
			log.info("Search User: {}", searchUser.getUsername());
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