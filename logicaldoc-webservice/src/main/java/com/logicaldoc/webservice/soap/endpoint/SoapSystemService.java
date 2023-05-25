package com.logicaldoc.webservice.soap.endpoint;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.apache.commons.beanutils.BeanUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.SystemInfo;
import com.logicaldoc.core.generic.Generic;
import com.logicaldoc.core.generic.GenericDAO;
import com.logicaldoc.core.i18n.Language;
import com.logicaldoc.core.i18n.LanguageManager;
import com.logicaldoc.core.security.SessionManager;
import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.core.stats.StatsCollector;
import com.logicaldoc.util.Context;
import com.logicaldoc.webservice.AbstractService;
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
	public WSParameter[] getStatistics(String sid) throws Exception {
		validateSession(sid);


		WSParameter[] parameters = new WSParameter[15];
		try {
			/*
			 * Repository statistics
			 */
			WSParameter docDirSize = getDocDirSize();
			parameters[0] = docDirSize;

			WSParameter userDirSize = getUserDirSize();
			parameters[1] = userDirSize;

			WSParameter indexDirSize = getIndexDirSize();
			parameters[2] = indexDirSize;

			WSParameter importDirSize = getImportDirSize();
			parameters[3] = importDirSize;

			WSParameter exportDirSize = getExportDirSize();
			parameters[4] = exportDirSize;

			WSParameter pluginsDirSize = getPluginsDirSize();
			parameters[5] = pluginsDirSize;

			WSParameter dbDirSize = getDbDirSize();
			parameters[6] = dbDirSize;

			WSParameter logsDirSize = getLogsDirSize();
			parameters[7] = logsDirSize;

			/*
			 * Document statistics
			 */
			GenericDAO genDao = (GenericDAO) Context.get().getBean(GenericDAO.class);
			Generic gen = genDao.findByAlternateKey(StatsCollector.STAT, "notindexeddocs", null, Tenant.DEFAULT_ID);
			WSParameter notIndexed = new WSParameter();
			notIndexed.setName("docs_notindexed");
			notIndexed.setValue(gen != null ? Long.toString(gen.getInteger1()) : "0");
			parameters[8] = notIndexed;

			gen = genDao.findByAlternateKey(StatsCollector.STAT, "indexeddocs", null, Tenant.DEFAULT_ID);
			WSParameter indexed = new WSParameter();
			indexed.setName("docs_indexed");
			indexed.setValue(gen != null ? Long.toString(gen.getInteger1()) : "0");
			parameters[9] = indexed;

			gen = genDao.findByAlternateKey(StatsCollector.STAT, "deleteddocs", null, Tenant.DEFAULT_ID);
			WSParameter deletedDocs = new WSParameter();
			deletedDocs.setName("docs_trash");
			deletedDocs.setValue(gen != null ? Long.toString(gen.getInteger1()) : "0");
			parameters[10] = deletedDocs;

			/*
			 * Folders statistics
			 */
			gen = genDao.findByAlternateKey(StatsCollector.STAT, "withdocs", null, Tenant.DEFAULT_ID);
			WSParameter notEmptyFolders = new WSParameter();
			notEmptyFolders.setName("folder_withdocs");
			notEmptyFolders.setValue(gen != null ? Long.toString(gen.getInteger1()) : "0");
			parameters[11] = notEmptyFolders;

			gen = genDao.findByAlternateKey(StatsCollector.STAT, "empty", null, Tenant.DEFAULT_ID);
			WSParameter emptyFolders = new WSParameter();
			emptyFolders.setName("folder_empty");
			emptyFolders.setValue(gen != null ? Long.toString(gen.getInteger1()) : "0");
			parameters[12] = emptyFolders;

			gen = genDao.findByAlternateKey(StatsCollector.STAT, "deletedfolders", null, Tenant.DEFAULT_ID);
			WSParameter deletedFolders = new WSParameter();
			deletedFolders.setName("folder_trash");
			deletedFolders.setValue(gen != null ? Long.toString(gen.getInteger1()) : "0");
			parameters[13] = deletedFolders;

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
			parameters[14] = lastrun;
		} catch (Exception e) {
			log.error(e.getMessage(), e);
		}

		return parameters;
	}

	private WSParameter getStat(String statSubtype, String paramName) {
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
	
	private WSParameter getLogsDirSize() {
		return getStat("logdir", "repo_logs");
	}

	private WSParameter getDbDirSize() {
		return getStat("dbdir", "repo_database");
	}

	private WSParameter getPluginsDirSize() {
		return getStat("plugindir", "repo_plugins");
	}

	private WSParameter getExportDirSize() {
		return getStat("exportdir", "repo_export");
	}

	private WSParameter getImportDirSize() {
		return getStat("importdir", "repo_import");
	}

	private WSParameter getIndexDirSize() {
		return getStat("indexdir", "repo_fulltextindex");
	}

	private WSParameter getUserDirSize() {
		return getStat("userdir", "repo_users");
	}

	private WSParameter getDocDirSize() {
		return getStat("docdir", "repo_docs");
	}

	@Override
	public String[] getLanguages(String tenantOrSid) throws Exception {
		List<String> langs = new ArrayList<>();

		String t = Tenant.DEFAULT_NAME;
		if (tenantOrSid != null)
			if (SessionManager.get().get(tenantOrSid) != null)
				t = SessionManager.get().get(tenantOrSid).getTenantName();
			else
				t = tenantOrSid;

		try {
			for (Language lang : LanguageManager.getInstance().getActiveLanguages(t)) {
				langs.add(lang.getLocale().toString());
			}
		} catch (Exception e) {
			log.error(e.getMessage(), e);
		}

		return langs.toArray(new String[0]);
	}

	@Override
	public WSSystemInfo getInfo() throws Exception {
		SystemInfo inf = SystemInfo.get();
		WSSystemInfo info = new WSSystemInfo();
		BeanUtils.copyProperties(info, inf);

		DateFormat df = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss Z");
		info.setDate(df.format(inf.getDate()));

		return info;
	}
}
