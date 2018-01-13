package com.logicaldoc.web.service;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Set;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.gwt.user.server.rpc.RemoteServiceServlet;
import com.logicaldoc.core.document.dao.BookmarkDAO;
import com.logicaldoc.core.i18n.Language;
import com.logicaldoc.core.i18n.LanguageManager;
import com.logicaldoc.core.metadata.Attribute;
import com.logicaldoc.core.searchengine.FulltextSearchOptions;
import com.logicaldoc.core.searchengine.Hit;
import com.logicaldoc.core.searchengine.Search;
import com.logicaldoc.core.searchengine.SearchOptions;
import com.logicaldoc.core.searchengine.folder.FolderCriterion;
import com.logicaldoc.core.searchengine.folder.FolderSearchOptions;
import com.logicaldoc.core.security.Session;
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
import com.logicaldoc.web.util.ServiceUtil;

/**
 * Implementation of the SearchService
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 6.0
 */
public class SearchServiceImpl extends RemoteServiceServlet implements SearchService {

	private static final long serialVersionUID = 1L;

	protected static Logger log = LoggerFactory.getLogger(SearchServiceImpl.class);

	@Override
	public GUIResult search(GUISearchOptions options) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());
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
				search.search();
			} catch (Exception e) {
				log.error(e.getMessage(), e);
			}

			result.setSuggestion(search.getSuggestion());
			result.setEstimatedHits(search.getEstimatedHitsNumber());

			List<Hit> hits = search.getHits();

			result.setTime(search.getExecTime());
			result.setHasMore(search.isMoreHitsPresent());

			BookmarkDAO bDao = (BookmarkDAO) Context.get().getBean(BookmarkDAO.class);
			List<Long> bookmarks = bDao.findBookmarkedDocs(session.getUserId());

			List<GUIDocument> guiResults = new ArrayList<GUIDocument>();
			for (Hit hit : hits) {
				GUIDocument h = DocumentServiceImpl.fromDocument(hit, null, null);
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
					ext.setSetId(e.getSetId());
					ext.setDateValue(e.getDateValue());
					ext.setStringValue(e.getStringValue());
					ext.setIntValue(e.getIntValue());
					ext.setDoubleValue(e.getDoubleValue());
					ext.setBooleanValue(e.getBooleanValue());
					ext.setType(e.getType());
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
			return (GUIResult) ServiceUtil.throwServerException(session, log, t);
		}
	}

	@Override
	public boolean save(GUISearchOptions options) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		try {
			SearchOptions opt = toSearchOptions(options);

			File file = UserUtil.getUserResource(session.getUserId(), "queries");
			file = new File(file, opt.getName() + ".ser");
			if (file.exists()) {
				return false;
			}

			try {
				opt.write(file);
			} catch (Exception e) {
				log.error(e.getMessage());
			}

			log.debug("Saved query " + opt.getName());
			return true;
		} catch (Throwable t) {
			return (Boolean) ServiceUtil.throwServerException(session, log, t);
		}
	}

	@Override
	public void delete(String[] names) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		try {
			File dir = UserUtil.getUserResource(session.getUserId(), "queries");

			for (String name : names) {
				File file = new File(dir, name + ".ser");
				try {
					FileUtils.forceDelete(file);
				} catch (IOException e) {
					log.error(e.getMessage());
				}
			}
		} catch (Throwable t) {
			ServiceUtil.throwServerException(session, log, t);
		}
	}

	@Override
	public GUISearchOptions load(String name) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		try {
			File dir = UserUtil.getUserResource(session.getUserId(), "queries");
			File file = new File(dir, name + ".ser");
			SearchOptions opt = null;
			try {
				opt = SearchOptions.read(file);
			} catch (Exception e) {
				log.error(e.getMessage(), e);
			}
			return toGUIOptions(opt);
		} catch (Throwable t) {
			return (GUISearchOptions) ServiceUtil.throwServerException(session, log, t);
		}
	}

	/**
	 * Load all the search options associated to all the searches saved for the
	 * current user.
	 * 
	 * @return the list of search options
	 */
	public List<SearchOptions> getSearches(Session session) {
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
			} catch (Exception e) {
				log.error(e.getMessage(), e);
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

				criterion.setField(crit.getFieldName());
				if (crit.isExtendedAttribute())
					criterion.setField(crit.getFieldName() + " type:" + crit.getType());

				if (crit.getType() == Attribute.TYPE_DATE)
					criterion.setDateValue(crit.getDateValue());
				else if (crit.getType() == Attribute.TYPE_INT || crit.getType() == FolderCriterion.TYPE_FOLDER
						|| crit.getType() == Attribute.TYPE_BOOLEAN)
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

	protected SearchOptions toSearchOptions(GUISearchOptions options) {
		SearchOptions searchOptions = Search.newOptions(options.getType());
		searchOptions.setTopOperator(options.getTopOperator());
		searchOptions.setDescription(options.getDescription());
		searchOptions.setExpression(options.getExpression());
		searchOptions.setMaxHits(options.getMaxHits());
		searchOptions.setName(options.getName());
		searchOptions.setUserId(options.getUserId());
		searchOptions.setCaseSensitive(options.getCaseSensitive() == 1);
		searchOptions.setRetrieveAliases(options.getRetrieveAliases());
		searchOptions.setFolderId(options.getFolder());
		searchOptions.setSearchInSubPath(options.isSearchInSubPath());
		searchOptions.setTemplate(options.getTemplate());

		if (options.getType() == SearchOptions.TYPE_FULLTEXT) {
			((FulltextSearchOptions) searchOptions).setDateFrom(options.getDateFrom());
			((FulltextSearchOptions) searchOptions).setDateTo(options.getDateTo());
			((FulltextSearchOptions) searchOptions).setCreationFrom(options.getCreationFrom());
			((FulltextSearchOptions) searchOptions).setCreationTo(options.getCreationTo());
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
					c.setValue(crit.getLongValue());
				} else if (crit.getDateValue() != null) {
					c.setValue(crit.getDateValue());
				} else if (crit.getDoubleValue() != null) {
					c.setValue(crit.getDoubleValue());
				} else {
					c.setValue(crit.getStringValue());
				}

				criteria.add(c);
			}
			((FolderSearchOptions) searchOptions).setCriteria(criteria.toArray(new FolderCriterion[0]));
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