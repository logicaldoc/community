package com.logicaldoc.webservice.soap.endpoint;

import java.lang.reflect.InvocationTargetException;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
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
import com.logicaldoc.core.security.user.UserDAO;
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
		return getTenantStatistics(sid, Tenant.SYSTEM_ID);
	}

	@Override
	public List<WSParameter> getTenantStatistics(String sid, long tenantId)
			throws AuthenticationException, WebserviceException, PersistenceException {
		validateSession(sid);

		List<WSParameter> parameters = new ArrayList<>();
		try {
			/*
			 * Repository statistics
			 */
			parameters.add(getStat("docdir", "repo_storage", tenantId));
			parameters.add(getStat("userdir", "repo_users", Tenant.SYSTEM_ID));
			parameters.add(getStat("indexdir", "repo_fulltextindex", Tenant.SYSTEM_ID));
			parameters.add(getStat("importdir", "repo_import", Tenant.SYSTEM_ID));
			parameters.add(getStat("exportdir", "repo_export", Tenant.SYSTEM_ID));
			parameters.add(getStat("plugindir", "repo_plugins", Tenant.SYSTEM_ID));
			parameters.add(getStat("dbdir", "repo_database", Tenant.SYSTEM_ID));
			parameters.add(getStat("logdir", "repo_logs", Tenant.SYSTEM_ID));

			/*
			 * Document statistics
			 */
			parameters.add(getStat("notindexeddocs", "docs_notindexed", tenantId));
			parameters.add(getStat("notindexabledocs", "docs_notindexable", tenantId));
			parameters.add(getStat("indexeddocs", "docs_indexed", tenantId));
			parameters.add(getStat("deleteddocs", "docs_trash", tenantId));
			parameters.add(getStat("archiveddocs", "docs_archived", tenantId));
			parameters.add(getStat("totaldocs", "docs_total", tenantId));

			/*
			 * Folders statistics
			 */
			parameters.add(getStat("withdocs", "folder_withdocs", tenantId));
			parameters.add(getStat("empty", "folder_empty", tenantId));
			parameters.add(getStat("deletedfolders", "folder_trash", tenantId));

			/*
			 * Users statistics
			 */
			UserDAO userDao = Context.get().getBean(UserDAO.class);
			WSParameter users = new WSParameter();
			users.setName("users_regular");
			users.setValue(Long.toString(userDao.count(tenantId != Tenant.SYSTEM_ID ? tenantId : null)));
			parameters.add(users);
			
			WSParameter readonly = new WSParameter();
			readonly.setName("users_readonly");
			readonly.setValue(Long.toString(userDao.countGuests(tenantId != Tenant.SYSTEM_ID ? tenantId : null)));
			parameters.add(readonly);
					
			/*
			 * Last run
			 */
			GenericDAO genDao = Context.get().getBean(GenericDAO.class);
			Generic gen = genDao.findByAlternateKey(StatsCollector.STAT, "lastrun", null, Tenant.SYSTEM_ID);
			DateFormat df = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
			WSParameter lastrun = new WSParameter();
			lastrun.setName("stats_lastrun");
			if (gen.getDate1() != null) {
				lastrun.setValue(df.format(gen.getDate1()));
			} else {
				lastrun.setValue("");
			}
			parameters.add(lastrun);
		} catch (Exception e) {
			log.error(e.getMessage(), e);
		}

		return parameters;
	}

	private WSParameter getStat(String statSubtype, String paramName, long tenantId) throws PersistenceException {
		GenericDAO genDao = Context.get().getBean(GenericDAO.class);
		Generic gen = genDao.findByAlternateKey(StatsCollector.STAT, statSubtype, null, tenantId);
		WSParameter parameter = new WSParameter();
		parameter.setName(paramName);
		if (gen != null)
			parameter.setValue(Long.toString(gen.getInteger1()));
		else
			parameter.setValue("0");
		return parameter;
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
