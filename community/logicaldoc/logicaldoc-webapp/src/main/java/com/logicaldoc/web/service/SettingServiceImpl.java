package com.logicaldoc.web.service;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.TreeSet;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.gwt.user.server.rpc.RemoteServiceServlet;
import com.logicaldoc.core.communication.EMail;
import com.logicaldoc.core.communication.EMailSender;
import com.logicaldoc.core.conversion.FormatConverter;
import com.logicaldoc.core.generic.Generic;
import com.logicaldoc.core.generic.GenericDAO;
import com.logicaldoc.core.security.Menu;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.core.security.dao.UserDAO;
import com.logicaldoc.core.store.Storer;
import com.logicaldoc.core.store.StorerManager;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUIDashlet;
import com.logicaldoc.gui.common.client.beans.GUIEmailSettings;
import com.logicaldoc.gui.common.client.beans.GUIParameter;
import com.logicaldoc.gui.frontend.client.services.SettingService;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.web.util.ServiceUtil;
import com.logicaldoc.web.util.ServletUtil;

/**
 * Implementation of the SettingService
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.0
 */
public class SettingServiceImpl extends RemoteServiceServlet implements SettingService {

	private static final long serialVersionUID = 1L;

	private static Logger log = LoggerFactory.getLogger(SettingServiceImpl.class);

	@Override
	public GUIEmailSettings loadEmailSettings() throws ServerException {
		Session session = ServiceUtil.checkMenu(getThreadLocalRequest(), Menu.SETTINGS);

		GUIEmailSettings emailSettings = new GUIEmailSettings();
		try {
			ContextProperties conf = Context.get().getProperties();

			emailSettings.setSmtpServer(conf.getProperty(session.getTenantName() + ".smtp.host"));
			emailSettings.setPort(Integer.parseInt(conf.getProperty(session.getTenantName() + ".smtp.port")));
			emailSettings
					.setUsername(!conf.getProperty(session.getTenantName() + ".smtp.username").trim().isEmpty() ? conf
							.getProperty(session.getTenantName() + ".smtp.username") : "");
			emailSettings.setPwd(!conf.getProperty(session.getTenantName() + ".smtp.password").trim().isEmpty() ? conf
					.getProperty(session.getTenantName() + ".smtp.password") : "");
			emailSettings.setConnSecurity(conf.getProperty(session.getTenantName() + ".smtp.connectionSecurity"));
			emailSettings
					.setSecureAuth("true".equals(conf.getProperty(session.getTenantName() + ".smtp.authEncripted")) ? true
							: false);
			emailSettings.setSenderEmail(conf.getProperty(session.getTenantName() + ".smtp.sender"));
			emailSettings.setUserAsFrom(conf.getBoolean(session.getTenantName() + ".smtp.userasfrom", true));

			log.info("Email settings data loaded successfully.");
		} catch (Exception e) {
			log.error("Exception loading Email settings data: {}", e.getMessage(), e);
		}

		return emailSettings;
	}

	@Override
	public void saveEmailSettings(GUIEmailSettings settings) throws ServerException {
		Session session = ServiceUtil.checkMenu(getThreadLocalRequest(), Menu.SETTINGS);

		try {
			ContextProperties conf = Context.get().getProperties();

			conf.setProperty(session.getTenantName() + ".smtp.host", settings.getSmtpServer());
			conf.setProperty(session.getTenantName() + ".smtp.port", Integer.toString(settings.getPort()));
			conf.setProperty(session.getTenantName() + ".smtp.username",
					!settings.getUsername().trim().isEmpty() ? settings.getUsername() : "");
			conf.setProperty(session.getTenantName() + ".smtp.password",
					!settings.getPwd().trim().isEmpty() ? settings.getPwd() : "");
			conf.setProperty(session.getTenantName() + ".smtp.connectionSecurity", settings.getConnSecurity());
			conf.setProperty(session.getTenantName() + ".smtp.authEncripted", settings.isSecureAuth() ? "true"
					: "false");
			conf.setProperty(session.getTenantName() + ".smtp.sender", settings.getSenderEmail());
			conf.setProperty(session.getTenantName() + ".smtp.userasfrom", "" + settings.isUserAsFrom());

			conf.write();

			EMailSender sender = (EMailSender) Context.get().getBean(EMailSender.class);
			sender.setHost(conf.getProperty(Tenant.DEFAULT_NAME + ".smtp.host"));
			sender.setPort(Integer.parseInt(conf.getProperty(Tenant.DEFAULT_NAME + ".smtp.port")));
			sender.setUsername(conf.getProperty(Tenant.DEFAULT_NAME + ".smtp.username"));
			sender.setPassword(conf.getProperty(Tenant.DEFAULT_NAME + ".smtp.password"));
			sender.setSender(conf.getProperty(Tenant.DEFAULT_NAME + ".smtp.sender"));
			sender.setAuthEncripted("true".equals(conf.getProperty(Tenant.DEFAULT_NAME + ".smtp.authEncripted")) ? true
					: false);
			sender.setConnectionSecurity(Integer.parseInt(conf.getProperty(Tenant.DEFAULT_NAME
					+ ".smtp.connectionSecurity")));

			log.info("Email settings data written successfully.");
		} catch (Exception e) {
			log.error("Exception writing Email settings data: {}", e.getMessage(), e);
		}
	}

	@Override
	public GUIParameter[] loadSettings() throws ServerException {
		ServiceUtil.checkMenu(getThreadLocalRequest(), Menu.SETTINGS);

		TreeSet<String> sortedSet = new TreeSet<String>();
		ContextProperties conf = Context.get().getProperties();
		for (Object key : conf.keySet()) {
			String name = key.toString();
			if (name.endsWith(".hidden") || name.endsWith("readonly"))
				continue;
			if (conf.containsKey(name + ".hidden")) {
				if ("true".equals(conf.getProperty(name + ".hidden")))
					continue;
			} else if (name.startsWith("product") || name.startsWith("skin") || name.startsWith("conf")
					|| name.startsWith("ldap") || name.startsWith("schedule") || name.contains(".smtp.")
					|| name.contains("password") || name.startsWith("ad") || name.startsWith("webservice")
					|| name.startsWith("webdav") || name.startsWith("cmis") || name.startsWith("runlevel")
					|| name.startsWith("aspect.") || name.startsWith("stat") || name.contains("index")
					|| name.equals("id") || name.contains(".lang.") || name.startsWith("reg.")
					|| name.startsWith("ocr.") || name.contains(".ocr.") || name.contains("barcode")
					|| name.startsWith("task.") || name.startsWith("store") || name.startsWith("advancedocr.")
					|| name.startsWith("command.") || name.contains(".gui.") || name.contains(".upload.")
					|| name.equals("userno") || name.contains(".search.") || name.contains("password")
					|| name.contains("tag.") || name.startsWith("jdbc.") || name.startsWith("cluster")
					|| name.startsWith("ip.") || name.contains(".extcall.") || name.contains("anonymous")
					|| name.startsWith("hibernate.") || name.contains(".session.") || name.contains("antivirus.")
					|| name.startsWith("login.") || name.equals("upload.maxsize") || name.startsWith("news.")
					|| name.equals("registry") || name.equals("searchengine") || name.equals("load")
					|| name.startsWith("ssl.") || name.contains(".tagcloud.") || name.startsWith("throttle.")
					|| name.contains("security.") || name.contains("parser.") || name.startsWith("quota.")
					|| name.equals("initialized") || name.startsWith("converter.") || name.startsWith("firewall.")
					|| name.contains(".2fa.") || name.startsWith("ftp."))
				continue;

			sortedSet.add(key.toString());
		}

		GUIParameter[] params = new GUIParameter[sortedSet.size()];
		int i = 0;
		for (String key : sortedSet) {
			GUIParameter p = new GUIParameter(key, conf.getProperty(key));
			params[i] = p;
			i++;
		}

		return params;
	}

	@Override
	public GUIParameter[] loadClientSettings() throws ServerException {
		Session session = ServiceUtil.checkMenu(getThreadLocalRequest(), Menu.SETTINGS);

		ContextProperties conf = Context.get().getProperties();
		List<GUIParameter> params = new ArrayList<GUIParameter>();
		for (Object key : conf.keySet()) {
			if (key.toString().equals("webservice.enabled") || key.toString().startsWith("webdav")
					|| key.toString().startsWith("cmis") || key.toString().startsWith("command.")
					|| key.toString().startsWith("openoffice") || key.toString().startsWith("ftp.")
					|| key.toString().startsWith(session.getTenantName() + ".extcall.")) {
				GUIParameter p = new GUIParameter(key.toString(), conf.getProperty(key.toString()));
				params.add(p);
			}
		}

		return params.toArray(new GUIParameter[0]);
	}

	@Override
	public void saveSettings(GUIParameter[] settings) throws ServerException {
		Session session = ServiceUtil.checkMenu(getThreadLocalRequest(), Menu.ADMINISTRATION);

		try {
			int counter = 0;

			ContextProperties conf = Context.get().getProperties();
			for (int i = 0; i < settings.length; i++) {
				if (settings[i] == null || StringUtils.isEmpty(settings[i].getName()))
					continue;
				conf.setProperty(settings[i].getName(), settings[i].getValue() != null ? settings[i].getValue() : "");
				counter++;
			}

			conf.write();

			log.info("Successfully saved {} parameters", counter);
		} catch (Throwable e) {
			ServiceUtil.throwServerException(session, log, e);
		}
	}

	@Override
	public GUIParameter[] loadSettingsByNames(String[] names) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		List<GUIParameter> values = new ArrayList<GUIParameter>();
		try {
			ContextProperties conf = Context.get().getProperties();

			for (int i = 0; i < names.length; i++) {
				if (names[i].endsWith("*")) {
					Map<String, String> map = conf.getProperties(names[i].substring(0, names[i].length() - 1));
					for (String key : map.keySet())
						values.add(new GUIParameter(key, map.get(key)));
				} else
					values.add(new GUIParameter(names[i], conf.getProperty(names[i])));
			}
		} catch (Throwable e) {
			ServiceUtil.throwServerException(session, log, e);
		}
		return values.toArray(new GUIParameter[0]);
	}

	@Override
	public GUIParameter[] loadGUISettings() throws ServerException {
		Session session = ServiceUtil.checkMenu(getThreadLocalRequest(), Menu.SETTINGS);

		ContextProperties conf = Context.get().getProperties();

		List<GUIParameter> params = new ArrayList<GUIParameter>();
		for (Object name : conf.keySet()) {
			if (name.toString().startsWith(session.getTenantName() + ".gui"))
				params.add(new GUIParameter(name.toString(), conf.getProperty(name.toString())));
		}
		if (session.getTenantName().equals(Tenant.DEFAULT_NAME))
			params.add(new GUIParameter("upload.maxsize", conf.getProperty("upload.maxsize")));
		params.add(new GUIParameter(session.getTenantName() + ".upload.disallow", conf.getProperty(session
				.getTenantName() + ".upload.disallow")));
		params.add(new GUIParameter(session.getTenantName() + ".search.hits", conf.getProperty(session.getTenantName()
				+ ".search.hits")));
		params.add(new GUIParameter(session.getTenantName() + ".search.extattr", conf.getProperty(session
				.getTenantName() + ".search.extattr")));
		params.add(new GUIParameter(session.getTenantName() + ".session.timeout", conf.getProperty(session
				.getTenantName() + ".session.timeout")));
		params.add(new GUIParameter(session.getTenantName() + ".session.heartbeat", conf.getProperty(session
				.getTenantName() + ".session.heartbeat")));

		return params.toArray(new GUIParameter[0]);
	}

	@Override
	public void saveDashlets(GUIDashlet[] dashlets) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());
		GenericDAO gDao = (GenericDAO) Context.get().getBean(GenericDAO.class);
		UserDAO uDao = (UserDAO) Context.get().getBean(UserDAO.class);

		try {
			/*
			 * Delete the actual dashlets for this user
			 */
			Map<String, Generic> settings = uDao.findUserSettings(session.getUserId(), "dashlet");
			for (Generic setting : settings.values()) {
				gDao.delete(setting.getId());
			}

			/*
			 * Now save the new dashlets
			 */
			for (GUIDashlet dashlet : dashlets) {
				Generic generic = new Generic("usersetting", "dashlet-" + dashlet.getId(), session.getUserId());
				generic.setInteger1((long) dashlet.getId());
				generic.setInteger2((long) dashlet.getColumn());
				generic.setInteger3((long) dashlet.getRow());
				generic.setString1(Long.toString(dashlet.getIndex()));
				gDao.store(generic);
			}
		} catch (Throwable e) {
			log.error(e.getMessage(), e);
		}
	}

	@Override
	public boolean testEmail(String email) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		ContextProperties config = Context.get().getProperties();
		EMailSender sender = new EMailSender(session.getTenantName());

		try {
			EMail mail;
			mail = new EMail();
			mail.setAccountId(-1);
			mail.setAuthor(config.getProperty(session.getTenantName() + ".smtp.sender"));
			mail.setAuthorAddress(config.getProperty(session.getTenantName() + ".smtp.sender"));
			mail.parseRecipients(email);
			mail.setFolder("outbox");
			mail.setSentDate(new Date());
			mail.setSubject("test");
			mail.setMessageText("test");

			log.info("Sending test email to {}", email);
			sender.send(mail);
			log.info("Test email sent");
			return true;
		} catch (Throwable e) {
			log.error(e.getMessage(), e);
			return false;
		}
	}

	@Override
	public boolean testStorage(int id) throws ServerException {
		ServiceUtil.validateSession(getThreadLocalRequest());
		try {
			Storer storer = StorerManager.get().newStorer(id);
			log.info("Testing storer {}", storer);
			return storer.test();
		} catch (Throwable e) {
			log.error(e.getMessage(), e);
			return false;
		}
	}

	@Override
	public void saveRegistration(String name, String email, String organization, String website) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		try {
			ContextProperties conf = Context.get().getProperties();
			conf.setProperty("reg.name", name != null ? name : "");
			conf.setProperty("reg.email", email != null ? email : "");
			conf.setProperty("reg.organization", organization != null ? organization : "");
			conf.setProperty("reg.website", website != null ? website : "");

			conf.write();

			log.info("Successfully saved registration informations");
		} catch (Throwable e) {
			ServiceUtil.throwServerException(session, log, e);
		}
	}

	@Override
	public GUIParameter[] loadConverterParameters(String converter) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		List<GUIParameter> parameters = new ArrayList<GUIParameter>();
		try {
			ServletUtil.checkMenu(getThreadLocalRequest(), 1750L);
			FormatConverter conv = (FormatConverter) Class.forName(converter).newInstance();
			for (String name : conv.getParameterNames())
				parameters.add(new GUIParameter(name, conv.getParameter(name)));
			return parameters.toArray(new GUIParameter[0]);
		} catch (Throwable e) {
			return (GUIParameter[]) ServiceUtil.throwServerException(session, log, e);
		}
	}
}