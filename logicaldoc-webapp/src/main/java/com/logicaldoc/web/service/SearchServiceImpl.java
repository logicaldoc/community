package com.logicaldoc.web.service;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.document.BookmarkDAO;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.DocumentDAO;
import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.folder.FolderDAO;
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
import com.logicaldoc.core.security.user.User;
import com.logicaldoc.core.security.user.UserDAO;
import com.logicaldoc.core.security.user.UserHistory;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUIAttribute;
import com.logicaldoc.gui.common.client.beans.GUICriterion;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIResult;
import com.logicaldoc.gui.common.client.beans.GUISearchOptions;
import com.logicaldoc.gui.frontend.client.services.SearchService;
import com.logicaldoc.util.LocaleUtil;
import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.util.spring.Context;

/**
 * Implementation of the SearchService
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class SearchServiceImpl extends AbstractRemoteService implements SearchService {

	private static final long serialVersionUID = 1L;

	private static final Logger log = LoggerFactory.getLogger(SearchServiceImpl.class);

	@Override
	public GUIResult search(GUISearchOptions options) throws ServerException {
		Session session = validateSession();
		options.setUserId(session.getUserId());

		GUIResult result = new GUIResult();
		try {
			List<Hit> hits = doSearch(options, session, result);

			BookmarkDAO bDao = Context.get(BookmarkDAO.class);
			List<Long> bookmarks = bDao.findBookmarkedDocs(session.getUserId());

			DocumentDAO docDao = Context.get(DocumentDAO.class);
			List<GUIDocument> guiResults = new ArrayList<>();
			DocumentServiceImpl documentServiceImpl = new DocumentServiceImpl();
			for (Hit hit : hits) {
				GUIDocument guiHit = null;
				if (hit.getType().startsWith("folder")) {
					guiHit = documentServiceImpl.fromDocument(hit, null, null);
					guiHit.setIcon(hit.getType());
				} else {
					Document doc = docDao.findById(hit.getId());
					docDao.initialize(doc);
					if (doc != null) {
						/*
						 * Apply the extended attributes. The template object in
						 * search hits may have not been fully compiled so the
						 * DocumentServiceImpl.fromDocument doesn't do the job
						 */
						guiHit = documentServiceImpl.fromDocument(doc, null, null);
					} else {
						log.debug("Unexisting document {}", hit.getId());
						continue;
					}
				}

				prepareHit(hit, guiHit, bookmarks);

				guiResults.add(guiHit);
			}
			result.setHits(guiResults);

			return result;
		} catch (Exception t) {
			return throwServerException(session, log, t);
		}
	}

	private void prepareHit(Hit hit, GUIDocument guiHit, List<Long> bookmarks) {
		guiHit.setScore(hit.getScore());
		guiHit.setSummary(hit.getSummary());
		guiHit.setBookmarked(bookmarks.contains(hit.getId()) || bookmarks.contains(hit.getDocRef()));
		if ("folder".equals(hit.getType()))
			guiHit.setIcon("folder_closed");
		else if ("folderalias".equals(hit.getType()))
			guiHit.setIcon("folder_alias_closed");
		else if ("pdf".equals(hit.getDocRefType()))
			guiHit.setIcon("pdf");
		else
			guiHit.setIcon(FileUtil.getBaseName(hit.getIcon()));

		/*
		 * If any extended attribute was retrieved from the fulltext index,
		 * apply them
		 */
		List<GUIAttribute> extList = new ArrayList<>();
		for (String name : hit.getAttributeNames()) {
			Attribute e = hit.getAttributes().get(name);
			GUIAttribute ext = new GUIAttribute();
			ext.setName(name);
			ext.setHidden(e.getHidden() == 1);
			ext.setReadonly(e.getReadonly() == 1);
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
		guiHit.setAttributes(extList);
	}

	private List<Hit> doSearch(GUISearchOptions options, Session session, GUIResult result) {
		SearchOptions searchOptions = prepareSearchOptions(options, session);

		// Retrieve the search machinery
		Search search = Search.get(searchOptions);

		try {
			log.info("Searching max {} hits", searchOptions.getMaxHits());
			search.search();
		} catch (Exception e) {
			log.error(e.getMessage(), e);
		}

		result.setEstimatedHits(search.getEstimatedHitsNumber());

		List<Hit> hits = search.getHits();

		result.setTime(search.getExecTime());
		result.setHasMore(search.isMoreHitsPresent());
		return hits;
	}

	private SearchOptions prepareSearchOptions(GUISearchOptions options, Session session) {
		SearchOptions searchOptions = toSearchOptions(options);
		searchOptions.setTenantId(session.getTenantId());
		searchOptions.setTransaction(new UserHistory(session));

		if (searchOptions instanceof FulltextSearchOptions fulltextOptions) {
			Locale exprLoc = LocaleUtil.toLocale(options.getExpressionLanguage());

			Language lang = LanguageManager.getInstance().getLanguage(exprLoc);
			if (lang == null) {
				// Try to find another supported language
				exprLoc = LocaleUtil.toLocale(exprLoc.getLanguage());

				if (exprLoc != null)
					fulltextOptions.setExpressionLanguage(exprLoc.getLanguage());
			}
		}
		return searchOptions;
	}

	@Override
	public boolean save(GUISearchOptions options) throws ServerException {
		Session session = validateSession();

		try {
			SearchOptions opt = toSearchOptions(options);

			com.logicaldoc.core.searchengine.saved.SavedSearch search = new com.logicaldoc.core.searchengine.saved.SavedSearch();
			search.setName(options.getName());
			search.setUserId(session.getUserId());
			search.setTenantId(session.getTenantId());
			search.setDescription(options.getDescription());
			search.saveOptions(opt);

			Context.get(SearchDAO.class).store(search);

			log.debug("Saved search {}", opt.getName());
			return true;
		} catch (Exception t) {
			return throwServerException(session, log, t);
		}
	}

	@Override
	public void delete(List<String> names) throws ServerException {
		Session session = validateSession();
		SearchDAO dao = Context.get(SearchDAO.class);

		try {
			for (String name : names) {
				com.logicaldoc.core.searchengine.saved.SavedSearch search = dao.findByUserIdAndName(session.getUserId(),
						name);
				if (search != null)
					dao.delete(search.getId());
			}
		} catch (Exception t) {
			throwServerException(session, log, t);
		}
	}

	@Override
	public GUISearchOptions load(String name) throws ServerException {
		Session session = validateSession();

		try {
			com.logicaldoc.core.searchengine.saved.SavedSearch search = Context.get(SearchDAO.class)
					.findByUserIdAndName(session.getUserId(), name);

			GUISearchOptions guiOptions = toGUIOptions(search.readOptions());
			if (guiOptions.getFolder() != null) {
				Folder fld = Context.get(FolderDAO.class).findById(guiOptions.getFolder());
				if (fld != null)
					guiOptions.setFolderName(fld.getName());
			}

			return guiOptions;
		} catch (Exception t) {
			return throwServerException(session, log, t);
		}
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
	 */
	public static List<SearchOptions> getSearches(Session session) throws PersistenceException {
		SearchDAO dao = Context.get(SearchDAO.class);

		Map<String, SearchOptions> map = new HashMap<>();
		List<com.logicaldoc.core.searchengine.saved.SavedSearch> searches = dao.findByUserId(session.getUserId());
		for (com.logicaldoc.core.searchengine.saved.SavedSearch search : searches) {
			try {
				map.put(search.getName(), search.readOptions());
			} catch (Exception e) {
				log.warn("Cannot read saved search {} of user {}", search.getName(), session.getUsername());
				log.warn(e.getMessage(), e);
			}
		}

		return map.values().stream()
				.sorted((SearchOptions s1, SearchOptions s2) -> s1.getName().compareTo(s2.getName())).toList();
	}

	protected GUISearchOptions toGUIOptions(SearchOptions searchOptions) {
		GUISearchOptions guiOptions = new GUISearchOptions();
		guiOptions.setType(searchOptions.getType());
		guiOptions.setDescription(searchOptions.getDescription());
		guiOptions.setExpression(searchOptions.getExpression());
		guiOptions.setMaxHits(searchOptions.getMaxHits());
		guiOptions.setName(searchOptions.getName());
		guiOptions.setUserId(searchOptions.getUserId());
		guiOptions.setTopOperator(searchOptions.getTopOperator());
		guiOptions.setFolder(searchOptions.getFolderId());
		guiOptions.setSearchInSubPath(searchOptions.isSearchInSubPath());
		guiOptions.setTemplate(searchOptions.getTemplate());
		guiOptions.setCaseSensitive(searchOptions.isCaseSensitive() ? 1 : 0);
		guiOptions.setRetrieveAliases(searchOptions.isRetrieveAliases() ? 1 : 0);

		if (searchOptions.getType() == SearchOptions.TYPE_FULLTEXT) {
			guiOptions.setDateFrom(((FulltextSearchOptions) searchOptions).getDateFrom());
			guiOptions.setDateTo(((FulltextSearchOptions) searchOptions).getDateTo());
			guiOptions.setCreationFrom(((FulltextSearchOptions) searchOptions).getCreationFrom());
			guiOptions.setCreationTo(((FulltextSearchOptions) searchOptions).getCreationTo());
			guiOptions.setExpressionLanguage(((FulltextSearchOptions) searchOptions).getExpressionLanguage());
			guiOptions.setFields(((FulltextSearchOptions) searchOptions).getFields().stream()
					.filter(f -> !"title".equals(f)).collect(Collectors.toList()));
			guiOptions.setFormat(((FulltextSearchOptions) searchOptions).getFormat());
			guiOptions.setLanguage(((FulltextSearchOptions) searchOptions).getLanguage());
			guiOptions.setSizeMax(((FulltextSearchOptions) searchOptions).getSizeMax());
			guiOptions.setSizeMin(((FulltextSearchOptions) searchOptions).getSizeMin());
		} else if (searchOptions.getType() == SearchOptions.TYPE_FOLDERS) {
			List<GUICriterion> criteria = new ArrayList<>();
			for (FolderCriterion criterion : ((FolderSearchOptions) searchOptions).getCriteria()) {
				GUICriterion guiCriterion = new GUICriterion();
				guiCriterion.setField(criterion.getField());
				if (criterion.getType() == Attribute.TYPE_DATE)
					guiCriterion.setDateValue(criterion.getDateValue());
				else if (criterion.getType() == Attribute.TYPE_INT || criterion.getType() == FolderCriterion.TYPE_FOLDER
						|| criterion.getType() == Attribute.TYPE_USER || criterion.getType() == Attribute.TYPE_BOOLEAN)
					guiCriterion.setLongValue(criterion.getLongValue());
				else if (criterion.getType() == Attribute.TYPE_DOUBLE)
					guiCriterion.setDoubleValue(criterion.getDoubleValue());
				else if (criterion.getType() == Attribute.TYPE_STRING
						|| criterion.getType() == FolderCriterion.TYPE_LANGUAGE)
					guiCriterion.setStringValue(criterion.getStringValue());

				guiCriterion.setOperator(criterion.getOperator().toLowerCase());

				if (!"folder".equals(guiCriterion.getField()))
					criteria.add(guiCriterion);
			}
			guiOptions.setCriteria(criteria);
		}

		guiOptions.setFilterIds(new ArrayList<>(searchOptions.getFilterIds()));

		return guiOptions;
	}

	@Override
	public void shareSearch(String name, List<Long> userIds, List<Long> groupIds) throws ServerException {
		Session session = validateSession();

		try {
			com.logicaldoc.core.searchengine.saved.SavedSearch search = loadSavedSearch(name, session);
			if (search == null)
				return;

			List<Long> allUsers = new ArrayList<>();
			allUsers.addAll(userIds);
			addUsersFromGroups(groupIds, allUsers);

			for (Long userId : allUsers)
				store(search, userId);
		} catch (Exception t) {
			throwServerException(session, log, t);
		}
	}

	private void store(com.logicaldoc.core.searchengine.saved.SavedSearch search, Long userId) {
		try {
			SearchDAO dao = Context.get(SearchDAO.class);
			com.logicaldoc.core.searchengine.saved.SavedSearch clone = new com.logicaldoc.core.searchengine.saved.SavedSearch(
					search);
			clone.setUserId(userId);
			dao.store(clone);
		} catch (Exception t) {
			log.warn("Cannot save search {} for user {}", search.getName(), userId, t);
		}
	}

	private void addUsersFromGroups(Collection<Long> groupIds, Collection<Long> users) throws PersistenceException {
		UserDAO gDao = Context.get(UserDAO.class);
		for (Long gId : groupIds) {
			Set<User> usrs = gDao.findByGroup(gId);
			for (User user : usrs) {
				if (!users.contains(user.getId()))
					users.add(user.getId());
			}
		}
	}

	private com.logicaldoc.core.searchengine.saved.SavedSearch loadSavedSearch(String name, Session session)
			throws PersistenceException {
		SearchDAO dao = Context.get(SearchDAO.class);
		return dao.findByUserIdAndName(session.getUserId(), name);
	}

	protected java.util.Date convertToJavaDate(Date source) {
		if (source == null)
			return source;

		Calendar cal = Calendar.getInstance();
		cal.setTime(source);
		return cal.getTime();
	}

	protected SearchOptions toSearchOptions(GUISearchOptions guiOptions) {
		SearchOptions searchOptions = Search.newOptions(guiOptions.getType());
		searchOptions.setTopOperator(guiOptions.getTopOperator());
		searchOptions.setDescription(guiOptions.getDescription());
		searchOptions.setExpression(guiOptions.getExpression());
		searchOptions.setMaxHits(guiOptions.getMaxHits());
		searchOptions.setName(guiOptions.getName());
		searchOptions.setUserId(guiOptions.getUserId());
		searchOptions.setCaseSensitive(guiOptions.getCaseSensitive() == 1);
		searchOptions.setRetrieveAliases(guiOptions.getRetrieveAliases() == 1);
		searchOptions.setFolderId(guiOptions.getFolder());
		searchOptions.setSearchInSubPath(guiOptions.isSearchInSubPath());
		searchOptions.setTemplate(guiOptions.getTemplate());

		if (guiOptions.getType() == SearchOptions.TYPE_FULLTEXT) {
			((FulltextSearchOptions) searchOptions).setDateFrom(convertToJavaDate(guiOptions.getDateFrom()));
			((FulltextSearchOptions) searchOptions).setDateTo(convertToJavaDate(guiOptions.getDateTo()));
			((FulltextSearchOptions) searchOptions).setCreationFrom(convertToJavaDate(guiOptions.getCreationFrom()));
			((FulltextSearchOptions) searchOptions).setCreationTo(convertToJavaDate(guiOptions.getCreationTo()));
			((FulltextSearchOptions) searchOptions).setExpressionLanguage(guiOptions.getExpressionLanguage());
			((FulltextSearchOptions) searchOptions).setFields(new HashSet<>(guiOptions.getFields()));
			((FulltextSearchOptions) searchOptions).setFormat(guiOptions.getFormat());
			((FulltextSearchOptions) searchOptions).setLanguage(guiOptions.getLanguage());
			((FulltextSearchOptions) searchOptions).setSizeMax(guiOptions.getSizeMax());
			((FulltextSearchOptions) searchOptions).setSizeMin(guiOptions.getSizeMin());
		} else if (guiOptions.getType() == SearchOptions.TYPE_FOLDERS) {
			List<FolderCriterion> criteria = new ArrayList<>();
			for (GUICriterion guiCriterion : guiOptions.getCriteria()) {
				FolderCriterion criterion = new FolderCriterion();
				criterion.setField(guiCriterion.getField());
				criterion.setComposition(guiOptions.getTopOperator());

				String operator = null;
				if ("icontains".equals(guiCriterion.getOperator()) || "inotcontains".equals(guiCriterion.getOperator()))
					operator = guiCriterion.getOperator().substring(1);
				else
					operator = guiCriterion.getOperator();

				criterion.setOperator(operator);

				if (guiCriterion.getLongValue() != null) {
					criterion.setLongValue(guiCriterion.getLongValue());
				} else if (guiCriterion.getDateValue() != null) {
					criterion.setDateValue(convertToJavaDate(guiCriterion.getDateValue()));
				} else if (guiCriterion.getDoubleValue() != null) {
					criterion.setDoubleValue(guiCriterion.getDoubleValue());
				} else {
					criterion.setValue(guiCriterion.getStringValue());
				}

				criteria.add(criterion);
			}
			((FolderSearchOptions) searchOptions).setCriteria(criteria);
		}

		searchOptions.setFilterIds(new HashSet<>(guiOptions.getFilterIds()));

		return searchOptions;
	}
}