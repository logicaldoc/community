package com.logicaldoc.web.service;

import java.util.ArrayList;
import java.util.List;
import java.util.StringTokenizer;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.gwt.user.server.rpc.RemoteServiceServlet;
import com.logicaldoc.core.document.dao.DocumentDAO;
import com.logicaldoc.core.i18n.LanguageManager;
import com.logicaldoc.core.parser.ParserFactory;
import com.logicaldoc.core.searchengine.SearchEngine;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUIParameter;
import com.logicaldoc.gui.common.client.beans.GUISearchEngine;
import com.logicaldoc.gui.frontend.client.services.SearchEngineService;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.web.util.ServiceUtil;

/**
 * Implementation of the SearchEngineService
 * 
 * @author Matteo Caruso - Logical Objects
 * @since 6.0
 */
public class SearchEngineServiceImpl extends RemoteServiceServlet implements SearchEngineService {

	private static final long serialVersionUID = 1L;

	private static Logger log = LoggerFactory.getLogger(SearchEngineServiceImpl.class);

	@Override
	public GUISearchEngine getInfo() throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		try {
			GUISearchEngine searchEngine = new GUISearchEngine();

			SearchEngine indexer = (SearchEngine) Context.get().getBean(SearchEngine.class);
			searchEngine.setLocked(indexer.isLocked());

			ContextProperties conf = Context.get().getProperties();
			searchEngine.setExcludePatters(conf.getProperty(session.getTenantName() + ".index.excludes"));
			searchEngine.setIncludePatters(conf.getProperty(session.getTenantName() + ".index.includes"));
			searchEngine.setParsingTimeout(conf.getInt(session.getTenantName() + ".parser.timeout", 0));
			searchEngine.setMaxTextFileSize(conf.getInt(session.getTenantName() + ".parser.txt.maxsize", 0));
			searchEngine.setDir(conf.getProperty("index.dir"));

			if (StringUtils.isNotEmpty(conf.getProperty("index.batch")))
				searchEngine.setBatch(new Integer(conf.getProperty("index.batch")));
			else
				searchEngine.setBatch(0);

			if (StringUtils.isNotEmpty(conf.getProperty("index.maxtext")))
				searchEngine.setMaxText(new Integer(conf.getProperty("index.maxtext")));
			else
				searchEngine.setMaxText(0);

			// Populate the list of supported languages
			searchEngine.setLanguages("");
			LanguageManager lm = LanguageManager.getInstance();
			List<String> langs = lm.getLanguagesAsString(session.getTenantName());
			for (String lang : langs) {
				searchEngine.setLanguages(searchEngine.getLanguages() + "," + lang);
			}
			if (searchEngine.getLanguages().startsWith(","))
				searchEngine.setLanguages(searchEngine.getLanguages().substring(1));

			return searchEngine;
		} catch (Exception t) {
			return (GUISearchEngine) ServiceUtil.throwServerException(session, log, t);
		}
	}

	@Override
	public void rescheduleAll(final boolean dropIndex) throws ServerException {
		final Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		if (dropIndex)
			try {
				SearchEngine indexer = (SearchEngine) Context.get().getBean(SearchEngine.class);
				indexer.dropIndex();
			} catch (Exception e) {
				log.error(e.getMessage(), e);
				throw new RuntimeException(e.getMessage(), e);
			}

		Runnable task = new Runnable() {
			public void run() {
				try {
					DocumentDAO documentDao = (DocumentDAO) Context.get().getBean(DocumentDAO.class);
					documentDao.bulkUpdate("set ld_indexed=0 where ld_indexed=1 "
							+ (!dropIndex ? " and ld_tenantid=" + session.getTenantId() : ""), null);
				} catch (Exception t) {
					log.error(t.getMessage(), t);
					throw new RuntimeException(t.getMessage(), t);
				}
			}
		};

		Thread recreateThread = new Thread(task);
		recreateThread.start();

		return;
	}

	@Override
	public void unlock() throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		try {
			SearchEngine indexer = (SearchEngine) Context.get().getBean(SearchEngine.class);
			indexer.unlock();
		} catch (Exception t) {
			ServiceUtil.throwServerException(session, log, t);
		}
	}

	@Override
	public String check() throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		try {
			SearchEngine indexer = (SearchEngine) Context.get().getBean(SearchEngine.class);
			return indexer.check();
		} catch (Exception t) {
			return (String) ServiceUtil.throwServerException(session, log, t);
		}
	}

	@Override
	public void save(GUISearchEngine searchEngine) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		try {
			ContextProperties conf = Context.get().getProperties();
			conf.setProperty(session.getTenantName() + ".index.excludes",
					searchEngine.getExcludePatters() != null ? searchEngine.getExcludePatters() : "");
			conf.setProperty(session.getTenantName() + ".index.includes",
					searchEngine.getIncludePatters() != null ? searchEngine.getIncludePatters() : "");
			conf.setProperty(session.getTenantName() + ".parser.timeout",
					searchEngine.getParsingTimeout() != null ? Integer.toString(searchEngine.getParsingTimeout()) : "");
			conf.setProperty(session.getTenantName() + ".parser.txt.maxsize",
					searchEngine.getMaxTextFileSize() != null ? Integer.toString(searchEngine.getMaxTextFileSize()) : "");
			
			if (session.getTenantId() == Tenant.DEFAULT_ID) {
				conf.setProperty("index.batch", Integer.toString(searchEngine.getBatch()));
				conf.setProperty("index.maxtext", Integer.toString(searchEngine.getMaxText()));
				conf.setProperty("index.dir", searchEngine.getDir());
			}

			conf.write();
		} catch (Exception t) {
			ServiceUtil.throwServerException(session, log, t);
		}
	}

	@Override
	public void setLanguageStatus(String language, boolean active) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		try {
			ContextProperties conf = Context.get().getProperties();
			conf.setProperty(session.getTenantName() + ".lang." + language, active ? "enabled" : "disabled");
			conf.write();
		} catch (Throwable t) {
			ServiceUtil.throwServerException(session, log, t);
		}
	}

	@Override
	public void setAliases(String extension, String aliases) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		try {
			StringTokenizer st = new StringTokenizer(aliases, ",", false);
			List<String> buf = new ArrayList<String>();
			while (st.hasMoreElements())
				buf.add(((String) st.nextElement()).trim());

			ParserFactory.setAliases(extension, buf.toArray(new String[0]));
		} catch (Throwable t) {
			ServiceUtil.throwServerException(session, log, t);
		}
	}

	@Override
	public long countEntries() throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());
		try {
			SearchEngine indexer = (SearchEngine) Context.get().getBean(SearchEngine.class);
			return indexer.getCount();
		} catch (Throwable t) {
			return (Long) ServiceUtil.throwServerException(session, log, t);
		}
	}

	@Override
	public void reorderTokenFilters(String[] filters) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());
		try {
			ContextProperties conf = Context.get().getProperties();
			int i = 1;
			for (String filter : filters)
				conf.setProperty("index.tokenfilter." + filter + ".position", Integer.toString(i++));
			conf.write();
		} catch (Throwable t) {
			ServiceUtil.throwServerException(session, log, t);
		}
	}

	@Override
	public void saveTokenFilterSettings(String filter, GUIParameter[] settings) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());
		try {
			String prefix = "index.tokenfilter." + filter + ".";
			ContextProperties conf = Context.get().getProperties();
			for (GUIParameter setting : settings)
				conf.setProperty(prefix + setting.getName(), setting.getValue().trim());
			conf.write();
		} catch (Throwable t) {
			ServiceUtil.throwServerException(session, log, t);
		}
	}

	@Override
	public void setTokenFilterStatus(String filter, boolean active) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());
		try {
			ContextProperties conf = Context.get().getProperties();
			conf.setProperty("index.tokenfilter." + filter, active ? "enabled" : "disabled");
			conf.write();
		} catch (Throwable t) {
			ServiceUtil.throwServerException(session, log, t);
		}
	}
}