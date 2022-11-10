package com.logicaldoc.web.service;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.dao.BookmarkDAO;
import com.logicaldoc.core.document.dao.DocumentDAO;
import com.logicaldoc.core.i18n.Language;
import com.logicaldoc.core.i18n.LanguageManager;
import com.logicaldoc.core.metadata.Attribute;
import com.logicaldoc.core.searchengine.FulltextSearchOptions;
import com.logicaldoc.core.searchengine.Hit;
import com.logicaldoc.core.searchengine.Search;
import com.logicaldoc.core.searchengine.SearchOptions;
import com.logicaldoc.core.searchengine.folder.FolderCriterion;
import com.logicaldoc.core.searchengine.folder.FolderSearchOptions;
import com.logicaldoc.core.searchengine.saved.SearchDAO;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.User;
import com.logicaldoc.core.security.dao.UserDAO;
import com.logicaldoc.core.util.UserUtil;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUIAttribute;
import com.logicaldoc.gui.common.client.beans.GUICriterion;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIResult;
import com.logicaldoc.gui.common.client.beans.GUISearchOptions;
import com.logicaldoc.gui.frontend.client.services.SearchService;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.LocaleUtil;

/**
 * Implementation of the SearchService
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class SearchServiceImpl extends AbstractRemoteService implements SearchService {

	private static final long serialVersionUID = 1L;

	protected static Logger log = LoggerFactory.getLogger(SearchServiceImpl.class);

	@Override
	public GUIResult search(GUISearchOptions options) throws ServerException {
		Session session = validateSession(getThreadLocalRequest());
		options.setUserId(session.getUserId());

		GUIResult result = new GUIResult();
		try {
			SearchOptions searchOptions = toSearchOptions(options);
			searchOptions.setTenantId(session.getTenantId());

			if (searchOptions instanceof FulltextSearchOptions) {
				Locale exprLoc = LocaleUtil.toLocale(options.getExpressionLanguage());

				Language lang = LanguageManager.getInstance().getLanguage(exprLoc);
				if (lang == null) {
					// Try to find another supported language
					exprLoc = LocaleUtil.toLocale(exprLoc.getLanguage());
					lang = LanguageManager.getInstance().getLanguage(exprLoc);

					if (exprLoc != null)
						((FulltextSearchOptions) searchOptions).setExpressionLanguage(exprLoc.getLanguage());
				}
			}

			// Retrieve the search machinery
			Search search = Search.get(searchOptions);

			try {
				log.info("Searching max {} hits", searchOptions.getMaxHits());
				search.search();
			} catch (Throwable e) {
				log.error(e.getMessage(), e);
			}

			result.setEstimatedHits(search.getEstimatedHitsNumber());

			List<Hit> hits = search.getHits();

			result.setTime(search.getExecTime());
			result.setHasMore(search.isMoreHitsPresent());

			BookmarkDAO bDao = (BookmarkDAO) Context.get().getBean(BookmarkDAO.class);
			List<Long> bookmarks = bDao.findBookmarkedDocs(session.getUserId());

			DocumentDAO docDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
			List<GUIDocument> guiResults = new ArrayList<GUIDocument>();
			for (Hit hit : hits) {
				GUIDocument h = null;
				if (hit.getType().startsWith("folder")) {
					h = DocumentServiceImpl.fromDocument(hit, null, null);
					h.setIcon(hit.getType());
				} else {
					Document doc = docDao.findById(hit.getId());
					if (doc != null) {
						h = DocumentServiceImpl.fromDocument(doc, null, null);
					} else {
						log.debug("Unexisting document {}", hit.getId());
						continue;
					}
				}

				h.setScore(hit.getScore());
				h.setSummary(hit.getSummary());
				h.setBookmarked(bookmarks.contains(hit.getId()) || bookmarks.contains(hit.getDocRef()));

				/*
				 * Apply the extended attributes. The template object in search
				 * hits may have not been fully compiled so the
				 * DocumentServiceImpl.fromDocument doesn't do the job
				 */
				List<GUIAttribute> extList = new ArrayList<GUIAttribute>();
				for (String name : hit.getAttributeNames()) {
					Attribute e = hit.getAttributes().get(name);
					GUIAttribute ext = new GUIAttribute();
					ext.setName(name);
					ext.setHidden(e.getHidden() == 1);
					ext.setMultiple(e.getMultiple() == 1);
					ext.setSetId(e.getSetId());
					ext.setDateValue(e.getDateValue());
					ext.setStringValue(e.getStringValue());
					ext.setIntValue(e.getIntValue());
					ext.setDoubleValue(e.getDoubleValue());
					ext.setBooleanValue(e.getBooleanValue());
					ext.setType(e.getType());
					ext.setParent(e.getParent());
					ext.setStringValues(e.getStringValues());
					if (e.getType() == Attribute.TYPE_USER)
						ext.setUsername(ext.getStringValue());
					extList.add(ext);
				}
				h.setAttributes(extList.toArray(new GUIAttribute[0]));

				if ("folder".equals(hit.getType()))
					h.setIcon("folder_closed");
				else if ("folderalias".equals(hit.getType()))
					h.setIcon("folder_alias_closed");
				else if ("pdf".equals(hit.getDocRefType()))
					h.setIcon("pdf");
				else
					h.setIcon(FilenameUtils.getBaseName(hit.getIcon()));
				guiResults.add(h);
			}
			result.setHits(guiResults.toArray(new GUIDocument[0]));

			return result;
		} catch (Throwable t) {
			return (GUIResult) throwServerException(session, log, t);
		}
	}

	@Override
	public boolean save(GUISearchOptions options) throws ServerException {
		Session session = validateSession(getThreadLocalRequest());

		try {
			SearchOptions opt = toSearchOptions(options);

			com.logicaldoc.core.searchengine.saved.SavedSearch search = new com.logicaldoc.core.searchengine.saved.SavedSearch();
			search.setName(options.getName());
			search.setUserId(session.getUserId());
			search.setTenantId(session.getTenantId());
			search.setDescription(options.getDescription());
			search.saveOptions(opt);

			SearchDAO dao = (SearchDAO) Context.get().getBean(SearchDAO.class);
			dao.store(search);

			log.debug("Saved query {}", opt.getName());
			return true;
		} catch (Throwable t) {
			return (Boolean) throwServerException(session, log, t);
		}
	}

	@Override
	public void delete(String[] names) throws ServerException {
		Session session = validateSession(getThreadLocalRequest());
		SearchDAO dao = (SearchDAO) Context.get().getBean(SearchDAO.class);

		try {
			for (String name : names) {
				com.logicaldoc.core.searchengine.saved.SavedSearch search = dao.findByUserIdAndName(session.getUserId(),
						name);
				if (search != null)
					dao.delete(search.getId());
			}
		} catch (Throwable t) {
			throwServerException(session, log, t);
		}

		try {
			legacyDelete(session.getUserId(), names);
		} catch (Throwable t) {
			// Nothing to do
		}
	}

	/**
	 * Deletes the searches from the filesystem (legacy format)
	 * 
	 * @param userId identifier of the user
	 * @param names the searches to delete
	 */
	@Deprecated(forRemoval = true, since = "9.0")
	private void legacyDelete(long userId, String[] names) {
		File dir = UserUtil.getUserResource(userId, "queries");
		for (String name : names) {
			File file = new File(dir, name + ".ser");
			try {
				FileUtils.forceDelete(file);
			} catch (IOException e) {
				log.error(e.getMessage());
			}
		}
	}

	@Override
	public GUISearchOptions load(String name) throws ServerException {
		Session session = validateSession(getThreadLocalRequest());
		SearchDAO dao = (SearchDAO) Context.get().getBean(SearchDAO.class);

		try {
			com.logicaldoc.core.searchengine.saved.SavedSearch search = dao.findByUserIdAndName(session.getUserId(),
					name);
			if (search != null)
				return toGUIOptions(search.readOptions());
			else
				return legacyLoad(session.getUserId(), name);
		} catch (Throwable t) {
			return (GUISearchOptions) throwServerException(session, log, t);
		}
	}

	/**
	 * Loads a search from the file system (legacy format)
	 * 
	 * @param name name of the saved search
	 * @param userId identifier of the user
	 * 
	 * @return the saved search options
	 * 
	 * @throws ServerException
	 */
	@Deprecated(forRemoval = true, since = "9.0")
	private GUISearchOptions legacyLoad(long userId, String name) {
		File dir = UserUtil.getUserResource(userId, "queries");
		File file = new File(dir, name + ".ser");
		SearchOptions opt = null;
		try {
			opt = SearchOptions.read(file);
		} catch (Exception e) {
			log.error(e.getMessage(), e);
		}
		return toGUIOptions(opt);
	}

	/**
	 * Load all the search options associated to all the searches saved for the
	 * current user
	 * 
	 * @param session the current session
	 * 
	 * @return the list of search options
	 * 
	 * @throws PersistenceException A problem at db level
	 * @throws IOException a generic IO error
	 */
	public static List<SearchOptions> getSearches(Session session) throws PersistenceException, IOException {
		SearchDAO dao = (SearchDAO) Context.get().getBean(SearchDAO.class);

		Map<String, SearchOptions> map = new HashMap<String, SearchOptions>();
		List<com.logicaldoc.core.searchengine.saved.SavedSearch> searches = dao.findByUserId(session.getUserId());
		for (com.logicaldoc.core.searchengine.saved.SavedSearch search : searches) {
			try {
				map.put(search.getName(), search.readOptions());
			} catch (Throwable e) {
				log.error("Cannot process saved search {} of user {}", search.getName(), session.getUsername(), e);
			}
		}

		List<SearchOptions> lecacyList = legacyGetSearches(session);
		for (SearchOptions searchOptions : lecacyList)
			if (!map.containsKey(searchOptions.getName()))
				map.put(searchOptions.getName(), searchOptions);

		return map.values().stream()
				.sorted((SearchOptions s1, SearchOptions s2) -> s1.getName().compareTo(s2.getName()))
				.collect(Collectors.toList());
	}

	@Deprecated(forRemoval = true, since = "9.0")
	private static List<SearchOptions> legacyGetSearches(Session session) {
		File file = UserUtil.getUserResource(session.getUserId(), "queries");
		if (!file.exists()) {
			return null;
		}

		// initiate the list
		List<SearchOptions> queries = new ArrayList<SearchOptions>();

		File[] searchesFiles = file.listFiles();
		for (int i = 0; i < searchesFiles.length; i++) {
			File searchFile = searchesFiles[i];
			SearchOptions opt = null;
			try {
				opt = SearchOptions.read(searchFile);
			} catch (Throwable e) {
				log.warn(e.getMessage());
			}

			if (opt != null)
				queries.add(opt);
		}

		return queries;
	}

	protected GUISearchOptions toGUIOptions(SearchOptions searchOptions) {
		GUISearchOptions op = new GUISearchOptions();
		op.setType(searchOptions.getType());
		op.setDescription(searchOptions.getDescription());
		op.setExpression(searchOptions.getExpression());
		op.setMaxHits(searchOptions.getMaxHits());
		op.setName(searchOptions.getName());
		op.setUserId(searchOptions.getUserId());
		op.setTopOperator(searchOptions.getTopOperator());
		op.setFolder(searchOptions.getFolderId());
		op.setSearchInSubPath(searchOptions.isSearchInSubPath());
		op.setTemplate(searchOptions.getTemplate());

		if (searchOptions.getType() == SearchOptions.TYPE_FULLTEXT) {
			op.setDateFrom(((FulltextSearchOptions) searchOptions).getDateFrom());
			op.setDateTo(((FulltextSearchOptions) searchOptions).getDateTo());
			op.setCreationFrom(((FulltextSearchOptions) searchOptions).getCreationFrom());
			op.setCreationTo(((FulltextSearchOptions) searchOptions).getCreationTo());
			op.setExpressionLanguage(((FulltextSearchOptions) searchOptions).getExpressionLanguage());
			op.setFields(((FulltextSearchOptions) searchOptions).getFields());
			op.setFormat(((FulltextSearchOptions) searchOptions).getFormat());
			op.setLanguage(((FulltextSearchOptions) searchOptions).getLanguage());
			op.setSizeMax(((FulltextSearchOptions) searchOptions).getSizeMax());
			op.setSizeMin(((FulltextSearchOptions) searchOptions).getSizeMin());
		} else if (searchOptions.getType() == SearchOptions.TYPE_FOLDERS) {
			List<GUICriterion> criteria = new ArrayList<GUICriterion>();
			for (FolderCriterion crit : ((FolderSearchOptions) searchOptions).getCriteria()) {
				GUICriterion criterion = new GUICriterion();
				criterion.setField(crit.getField());
				if (crit.getType() == Attribute.TYPE_DATE)
					criterion.setDateValue(crit.getDateValue());
				else if (crit.getType() == Attribute.TYPE_INT || crit.getType() == FolderCriterion.TYPE_FOLDER
						|| crit.getType() == Attribute.TYPE_USER || crit.getType() == Attribute.TYPE_BOOLEAN)
					criterion.setLongValue(crit.getLongValue());
				else if (crit.getType() == Attribute.TYPE_DOUBLE)
					criterion.setDoubleValue(crit.getDoubleValue());
				else if (crit.getType() == Attribute.TYPE_STRING || crit.getType() == FolderCriterion.TYPE_LANGUAGE)
					criterion.setStringValue(crit.getStringValue());

				criterion.setOperator(crit.getOperator().toLowerCase());
				criteria.add(criterion);
			}
			op.setCriteria(criteria.toArray(new GUICriterion[0]));
		}

		if (!searchOptions.getFilterIds().isEmpty()) {
			op.setFilterIds(searchOptions.getFilterIds().toArray(new Long[0]));
		}

		return op;
	}

	@Override
	public void shareSearch(String name, long[] userIds, long[] groupIds) throws ServerException {
		Session session = validateSession(getThreadLocalRequest());
		SearchDAO dao = (SearchDAO) Context.get().getBean(SearchDAO.class);

		try {
			com.logicaldoc.core.searchengine.saved.SavedSearch search = dao.findByUserIdAndName(session.getUserId(),
					name);
			if (search == null) {
				// Perhaps the search is not already in the database
				GUISearchOptions legacyOptions = legacyLoad(session.getUserId(), name);
				if (legacyOptions != null) {
					save(legacyOptions);
					search = dao.findByUserIdAndName(session.getUserId(), name);
				}
			}

			if (search != null) {
				HashSet<Long> users = new HashSet<Long>();
				if (userIds != null)
					for (Long uId : userIds) {
						if (!users.contains(uId))
							users.add(uId);
					}
				if (groupIds != null) {
					UserDAO gDao = (UserDAO) Context.get().getBean(UserDAO.class);
					for (Long gId : groupIds) {
						Set<User> usrs = gDao.findByGroup(gId);
						for (User user : usrs) {
							if (!users.contains(user.getId()))
								users.add(user.getId());
						}
					}
				}

				for (Long userId : users) {
					try {
						com.logicaldoc.core.searchengine.saved.SavedSearch clone = new com.logicaldoc.core.searchengine.saved.SavedSearch(
								search);
						clone.setUserId(userId);
						dao.store(clone);
					} catch (Throwable t) {
						log.warn("Cannot save search {} for user {}", search.getName(), userId, t);
					}
				}
			}
		} catch (Throwable t) {
			throwServerException(session, log, t);
		}
	}

	protected java.util.Date convertToJavaDate(Date source) {
		if (source == null)
			return source;

		Calendar cal = Calendar.getInstance();
		cal.setTime(source);
		return cal.getTime();
	}

	protected SearchOptions toSearchOptions(GUISearchOptions options) {
		SearchOptions searchOptions = Search.newOptions(options.getType());
		searchOptions.setTopOperator(options.getTopOperator());
		searchOptions.setDescription(options.getDescription());
		searchOptions.setExpression(options.getExpression());
		searchOptions.setMaxHits(options.getMaxHits());
		searchOptions.setName(options.getName());
		searchOptions.setUserId(options.getUserId());
		searchOptions.setCaseSensitive(options.getCaseSensitive() == 1);
		searchOptions.setRetrieveAliases(options.getRetrieveAliases() == 1);
		searchOptions.setFolderId(options.getFolder());
		searchOptions.setSearchInSubPath(options.isSearchInSubPath());
		searchOptions.setTemplate(options.getTemplate());

		if (options.getType() == SearchOptions.TYPE_FULLTEXT) {
			((FulltextSearchOptions) searchOptions).setDateFrom(convertToJavaDate(options.getDateFrom()));
			((FulltextSearchOptions) searchOptions).setDateTo(convertToJavaDate(options.getDateTo()));
			((FulltextSearchOptions) searchOptions).setCreationFrom(convertToJavaDate(options.getCreationFrom()));
			((FulltextSearchOptions) searchOptions).setCreationTo(convertToJavaDate(options.getCreationTo()));
			((FulltextSearchOptions) searchOptions).setExpressionLanguage(options.getExpressionLanguage());
			((FulltextSearchOptions) searchOptions).setFields(options.getFields());
			((FulltextSearchOptions) searchOptions).setFormat(options.getFormat());
			((FulltextSearchOptions) searchOptions).setLanguage(options.getLanguage());
			((FulltextSearchOptions) searchOptions).setSizeMax(options.getSizeMax());
			((FulltextSearchOptions) searchOptions).setSizeMin(options.getSizeMin());
		} else if (options.getType() == SearchOptions.TYPE_FOLDERS) {
			List<FolderCriterion> criteria = new ArrayList<FolderCriterion>();
			for (GUICriterion crit : options.getCriteria()) {
				FolderCriterion c = new FolderCriterion();
				c.setField(crit.getField());
				c.setComposition(options.getTopOperator());

				String operator = null;
				if ("icontains".equals(crit.getOperator()) || "inotcontains".equals(crit.getOperator()))
					operator = crit.getOperator().substring(1);
				else
					operator = crit.getOperator();

				c.setOperator(operator);

				if (crit.getLongValue() != null) {
					c.setLongValue(crit.getLongValue());
				} else if (crit.getDateValue() != null) {
					c.setDateValue(convertToJavaDate(crit.getDateValue()));
				} else if (crit.getDoubleValue() != null) {
					c.setDoubleValue(crit.getDoubleValue());
				} else {
					c.setValue(crit.getStringValue());
				}

				criteria.add(c);
			}
			((FolderSearchOptions) searchOptions).setCriteria(criteria);
		}

		if (options.getFilterIds() != null && options.getFilterIds().length > 0) {
			Set<Long> ids = new HashSet<Long>();
			for (Long id : options.getFilterIds()) {
				ids.add(id);
			}
			searchOptions.setFilterIds(ids);
		}

		return searchOptions;
	}
}