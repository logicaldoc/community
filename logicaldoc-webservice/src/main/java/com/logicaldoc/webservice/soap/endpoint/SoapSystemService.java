package com.logicaldoc.webservice.soap.endpoint;

import java.lang.reflect.InvocationTargetException;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.apache.commons.beanutils.BeanUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.SystemInfo;
import com.logicaldoc.core.generic.Generic;
import com.logicaldoc.core.generic.GenericDAO;
import com.logicaldoc.core.i18n.Language;
import com.logicaldoc.core.i18n.LanguageManager;
import com.logicaldoc.core.security.SessionManager;
import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.core.security.authentication.AuthenticationException;
import com.logicaldoc.core.stats.StatsCollector;
import com.logicaldoc.util.Context;
import com.logicaldoc.webservice.AbstractService;
import com.logicaldoc.webservice.WebserviceException;
import com.logicaldoc.webservice.model.WSParameter;
import com.logicaldoc.webservice.model.WSSystemInfo;
import com.logicaldoc.webservice.soap.SystemService;

/**
 * System Web Service Implementation
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.1
 */
public class SoapSystemService extends AbstractService implements SystemService {

	protected static Logger log = LoggerFactory.getLogger(SoapSystemService.class);

	@Override
	public List<WSParameter> getStatistics(String sid)
			throws AuthenticationException, WebserviceException, PersistenceException {
		validateSession(sid);

		List<WSParameter> parameters = new ArrayList<>();
		try {
			/*
			 * Repository statistics
			 */
			WSParameter docDirSize = getDocDirSize();
			parameters.add(docDirSize);

			WSParameter userDirSize = getUserDirSize();
			parameters.add(userDirSize);

			WSParameter indexDirSize = getIndexDirSize();
			parameters.add(indexDirSize);

			WSParameter importDirSize = getImportDirSize();
			parameters.add(importDirSize);

			WSParameter exportDirSize = getExportDirSize();
			parameters.add(exportDirSize);

			WSParameter pluginsDirSize = getPluginsDirSize();
			parameters.add(pluginsDirSize);

			WSParameter dbDirSize = getDbDirSize();
			parameters.add(dbDirSize);

			WSParameter logsDirSize = getLogsDirSize();
			parameters.add(logsDirSize);

			/*
			 * Document statistics
			 */
			GenericDAO genDao = (GenericDAO) Context.get().getBean(GenericDAO.class);
			Generic gen = genDao.findByAlternateKey(StatsCollector.STAT, "notindexeddocs", null, Tenant.DEFAULT_ID);
			WSParameter notIndexed = new WSParameter();
			notIndexed.setName("docs_notindexed");
			notIndexed.setValue(gen != null ? Long.toString(gen.getInteger1()) : "0");
			parameters.add(notIndexed);

			gen = genDao.findByAlternateKey(StatsCollector.STAT, "indexeddocs", null, Tenant.DEFAULT_ID);
			WSParameter indexed = new WSParameter();
			indexed.setName("docs_indexed");
			indexed.setValue(gen != null ? Long.toString(gen.getInteger1()) : "0");
			parameters.add(indexed);

			gen = genDao.findByAlternateKey(StatsCollector.STAT, "deleteddocs", null, Tenant.DEFAULT_ID);
			WSParameter deletedDocs = new WSParameter();
			deletedDocs.setName("docs_trash");
			deletedDocs.setValue(gen != null ? Long.toString(gen.getInteger1()) : "0");
			parameters.add(deletedDocs);

			/*
			 * Folders statistics
			 */
			gen = genDao.findByAlternateKey(StatsCollector.STAT, "withdocs", null, Tenant.DEFAULT_ID);
			WSParameter notEmptyFolders = new WSParameter();
			notEmptyFolders.setName("folder_withdocs");
			notEmptyFolders.setValue(gen != null ? Long.toString(gen.getInteger1()) : "0");
			parameters.add(notEmptyFolders);

			gen = genDao.findByAlternateKey(StatsCollector.STAT, "empty", null, Tenant.DEFAULT_ID);
			WSParameter emptyFolders = new WSParameter();
			emptyFolders.setName("folder_empty");
			emptyFolders.setValue(gen != null ? Long.toString(gen.getInteger1()) : "0");
			parameters.add(emptyFolders);

			gen = genDao.findByAlternateKey(StatsCollector.STAT, "deletedfolders", null, Tenant.DEFAULT_ID);
			WSParameter deletedFolders = new WSParameter();
			deletedFolders.setName("folder_trash");
			deletedFolders.setValue(gen != null ? Long.toString(gen.getInteger1()) : "0");
			parameters.add(deletedFolders);

			/*
			 * Last run
			 */
			gen = genDao.findByAlternateKey(StatsCollector.STAT, "lastrun", null, Tenant.DEFAULT_ID);
			DateFormat df = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
			Date date = gen != null ? df.parse(gen.getString1()) : null;
			WSParameter lastrun = new WSParameter();
			lastrun.setName("stats_lastrun");
			if (date != null) {
				lastrun.setValue(df.format(date));
			} else {
				lastrun.setValue("");
			}
			parameters.add(lastrun);
		} catch (Exception e) {
			log.error(e.getMessage(), e);
		}

		return parameters;
	}

	private WSParameter getStat(String statSubtype, String paramName) throws PersistenceException {
		GenericDAO genDao = (GenericDAO) Context.get().getBean(GenericDAO.class);
		Generic gen = genDao.findByAlternateKey(StatsCollector.STAT, statSubtype, null, Tenant.DEFAULT_ID);
		WSParameter logsDirSize = new WSParameter();
		logsDirSize.setName(paramName);
		if (gen != null)
			logsDirSize.setValue(gen.getString1());
		else
			logsDirSize.setValue("0");
		return logsDirSize;
	}

	private WSParameter getLogsDirSize() throws PersistenceException {
		return getStat("logdir", "repo_logs");
	}

	private WSParameter getDbDirSize() throws PersistenceException {
		return getStat("dbdir", "repo_database");
	}

	private WSParameter getPluginsDirSize() throws PersistenceException {
		return getStat("plugindir", "repo_plugins");
	}

	private WSParameter getExportDirSize() throws PersistenceException {
		return getStat("exportdir", "repo_export");
	}

	private WSParameter getImportDirSize() throws PersistenceException {
		return getStat("importdir", "repo_import");
	}

	private WSParameter getIndexDirSize() throws PersistenceException {
		return getStat("indexdir", "repo_fulltextindex");
	}

	private WSParameter getUserDirSize() throws PersistenceException {
		return getStat("userdir", "repo_users");
	}

	private WSParameter getDocDirSize() throws PersistenceException {
		return getStat("docdir", "repo_docs");
	}

	@Override
	public List<String> getLanguages(String tenantOrSid) {
		List<String> langs = new ArrayList<>();

		String t = Tenant.DEFAULT_NAME;
		if (tenantOrSid != null) {
			if (SessionManager.get().get(tenantOrSid) != null)
				t = SessionManager.get().get(tenantOrSid).getTenantName();
			else
				t = tenantOrSid;
		}

		try {
			for (Language lang : LanguageManager.getInstance().getActiveLanguages(t))
				langs.add(lang.getLocale().toString());
		} catch (Exception e) {
			log.error(e.getMessage(), e);
		}

		return langs;
	}

	@Override
	public WSSystemInfo getInfo() throws WebserviceException {
		try {
			SystemInfo info = SystemInfo.get();

			WSSystemInfo wsInfo = new WSSystemInfo();
			BeanUtils.copyProperties(wsInfo, info);

			wsInfo.setFeatures(new ArrayList<>(info.getFeatures()));

			DateFormat df = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss Z");
			wsInfo.setDate(df.format(info.getDate()));

			return wsInfo;
		} catch (IllegalAccessException | InvocationTargetException e) {
			throw new WebserviceException(e.getMessage(), e);
		}
	}
}
