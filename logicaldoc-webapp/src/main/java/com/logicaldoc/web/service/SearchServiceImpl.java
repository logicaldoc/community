package com.logicaldoc.web.service;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

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
import com.logicaldoc.core.searchengine.saved.SearchDAO;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.user.User;
import com.logicaldoc.core.security.user.UserDAO;
import com.logicaldoc.core.security.user.UserHistory;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUIAttribute;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIResult;
import com.logicaldoc.gui.common.client.beans.GUISearchOptions;
import com.logicaldoc.gui.frontend.client.services.SearchService;
import com.logicaldoc.util.LocaleUtil;
import com.logicaldoc.util.io.FileUtil;

/**
 * Implementation of the SearchService
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class SearchServiceImpl extends AbstractRemoteService implements SearchService {

	private static final long serialVersionUID = 1L;

	private static final Logger log = LoggerFactory.getLogger(SearchServiceImpl.class);

	/**
	 * A map for the {@link GUISearchOptions} - {@link SearchOptions}
	 * converters, key is the search type
	 */
	private static final Map<Integer, SearchOptionsConverter> converters = new HashMap<>();

	public static void registerConverter(Integer type, SearchOptionsConverter converter) {
		converters.put(type, converter);
	}

	static {
		registerConverter(GUISearchOptions.TYPE_FULLTEXT, new FulltextOptionsConverter());
		registerConverter(GUISearchOptions.TYPE_FOLDERS, new FolderOptionsConverter());
		registerConverter(GUISearchOptions.TYPE_TAGS, new SearchOptionsConverter());
	}

	@Override
	public GUIResult search(GUISearchOptions options) throws ServerException {
		Session session = validateSession();
		options.setUserId(session.getUserId());

		GUIResult result = new GUIResult();
		try {
			List<Hit> hits = doSearch(options, session, result);

			List<Long> bookmarks = BookmarkDAO.get().findBookmarkedDocs(session.getUserId());

			DocumentDAO docDao = DocumentDAO.get();
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
			ext.setHidden(e.isHidden());
			ext.setReadonly(e.isReadonly());
			ext.setMultiple(e.isMultiple());
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
		SearchOptions searchOptions = converters.get(options.getType()).toSearchOptions(options);
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
			SearchOptions opt = converters.get(options.getType()).toSearchOptions(options);

			com.logicaldoc.core.searchengine.saved.SavedSearch search = new com.logicaldoc.core.searchengine.saved.SavedSearch();
			search.setName(options.getName());
			search.setUserId(session.getUserId());
			search.setTenantId(session.getTenantId());
			search.setDescription(options.getDescription());
			search.saveOptions(opt);

			SearchDAO.get().store(search);

			log.debug("Saved search {}", opt.getName());
			return true;
		} catch (Exception t) {
			return throwServerException(session, log, t);
		}
	}

	@Override
	public void delete(List<String> names) throws ServerException {
		Session session = validateSession();
		SearchDAO dao = SearchDAO.get();

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
			com.logicaldoc.core.searchengine.saved.SavedSearch search = SearchDAO.get()
					.findByUserIdAndName(session.getUserId(), name);

			SearchOptions options = search.readOptions();
			GUISearchOptions guiOptions = converters.get(options.getType()).toGUISearchOptions(search.readOptions());
			if (guiOptions.getFolder() != null) {
				Folder fld = FolderDAO.get().findById(guiOptions.getFolder());
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
		SearchDAO dao = SearchDAO.get();

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
			SearchDAO dao = SearchDAO.get();
			com.logicaldoc.core.searchengine.saved.SavedSearch clone = new com.logicaldoc.core.searchengine.saved.SavedSearch(
					search);
			clone.setUserId(userId);
			dao.store(clone);
		} catch (Exception t) {
			log.warn("Cannot save search {} for user {}", search.getName(), userId, t);
		}
	}

	private void addUsersFromGroups(Collection<Long> groupIds, Collection<Long> users) throws PersistenceException {
		UserDAO gDao = UserDAO.get();
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
		SearchDAO dao = SearchDAO.get();
		return dao.findByUserIdAndName(session.getUserId(), name);
	}

}