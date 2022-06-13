package com.logicaldoc.web.service;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.TreeSet;
import java.util.stream.Collectors;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.gwt.user.server.rpc.RemoteServiceServlet;
import com.logicaldoc.core.SystemInfo;
import com.logicaldoc.core.communication.EMail;
import com.logicaldoc.core.communication.EMailSender;
import com.logicaldoc.core.conversion.FormatConverter;
import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.generic.Generic;
import com.logicaldoc.core.generic.GenericDAO;
import com.logicaldoc.core.security.Menu;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.core.sequence.SequenceDAO;
import com.logicaldoc.core.store.Storer;
import com.logicaldoc.core.store.StorerManager;
import com.logicaldoc.gui.common.client.ServerException;
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
			emailSettings.setUsername(!conf.getProperty(session.getTenantName() + ".smtp.username").trim().isEmpty()
					? conf.getProperty(session.getTenantName() + ".smtp.username")
					: "");
			emailSettings.setPwd(!conf.getProperty(session.getTenantName() + ".smtp.password").trim().isEmpty()
					? conf.getProperty(session.getTenantName() + ".smtp.password")
					: "");
			emailSettings.setConnSecurity(conf.getProperty(session.getTenantName() + ".smtp.connectionSecurity"));
			emailSettings.setSecureAuth(
					"true".equals(conf.getProperty(session.getTenantName() + ".smtp.authEncripted")) ? true : false);
			emailSettings.setSenderEmail(conf.getProperty(session.getTenantName() + ".smtp.sender"));
			emailSettings.setUserAsFrom(conf.getBoolean(session.getTenantName() + ".smtp.userasfrom", true));
			emailSettings.setFoldering(conf.getInt(session.getTenantName() + ".smtp.save.foldering", 3));

			emailSettings.setTargetFolder(FolderServiceImpl.getFolder(session,
					conf.getLong(session.getTenantName() + ".smtp.save.folderId", 0)));

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
			conf.setProperty(session.getTenantName() + ".smtp.authEncripted",
					settings.isSecureAuth() ? "true" : "false");
			conf.setProperty(session.getTenantName() + ".smtp.sender", settings.getSenderEmail());
			conf.setProperty(session.getTenantName() + ".smtp.userasfrom", "" + settings.isUserAsFrom());
			conf.setProperty(session.getTenantName() + ".smtp.save.foldering",
					Integer.toString(settings.getFoldering()));
			conf.setProperty(session.getTenantName() + ".smtp.save.folderId",
					settings.getTargetFolder() != null ? Long.toString(settings.getTargetFolder().getId()) : "");

			conf.write();

			// Always update the settings for the default sencer
			EMailSender sender = (EMailSender) Context.get().getBean(EMailSender.class);
			sender.setHost(conf.getProperty(Tenant.DEFAULT_NAME + ".smtp.host"));
			sender.setPort(Integer.parseInt(conf.getProperty(Tenant.DEFAULT_NAME + ".smtp.port")));
			sender.setUsername(conf.getProperty(Tenant.DEFAULT_NAME + ".smtp.username"));
			sender.setPassword(conf.getProperty(Tenant.DEFAULT_NAME + ".smtp.password"));
			sender.setSender(conf.getProperty(Tenant.DEFAULT_NAME + ".smtp.sender"));
			sender.setAuthEncripted(
					"true".equals(conf.getProperty(Tenant.DEFAULT_NAME + ".smtp.authEncripted")) ? true : false);
			sender.setConnectionSecurity(
					Integer.parseInt(conf.getProperty(Tenant.DEFAULT_NAME + ".smtp.connectionSecurity")));
			sender.setFoldering(conf.getInt(Tenant.DEFAULT_NAME + ".smtp.save.foldering", EMailSender.FOLDERING_MONTH));
			sender.setFolderId(conf.getProperty(Tenant.DEFAULT_NAME + ".smtp.save.folderId") != null
					? conf.getLong(Tenant.DEFAULT_NAME + ".smtp.save.folderId", 0L)
					: null);

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
					|| name.contains("tag.") || name.startsWith("cluster") || name.startsWith("ip.")
					|| name.contains(".extcall.") || name.contains("anonymous") || name.startsWith("hibernate.")
					|| name.contains(".session.") || name.contains("antivirus.") || name.startsWith("login.")
					|| name.equals("upload.maxsize") || name.startsWith("news.") || name.equals("registry")
					|| name.equals("searchengine") || name.equals("load") || name.startsWith("ssl.")
					|| name.contains(".tagcloud.") || name.startsWith("throttle.") || name.contains("security.")
					|| name.contains("parser.") || name.startsWith("quota.") || name.equals("initialized")
					|| name.startsWith("converter.") || name.startsWith("firewall.") || name.contains(".2fa.")
					|| name.startsWith("ftp.") || name.startsWith("cas.") || name.startsWith("cache.")
					|| name.startsWith("jdbc.") || name.startsWith("comparator.") || name.contains(".via.")
					|| name.contains(".downloadticket.") || name.startsWith("zonalocr.") || name.endsWith(".charset"))
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
	public GUIParameter[] loadProtocolSettings() throws ServerException {
		ServiceUtil.checkMenu(getThreadLocalRequest(), Menu.SETTINGS);

		ContextProperties conf = Context.get().getProperties();
		List<GUIParameter> params = new ArrayList<GUIParameter>();
		for (Object key : conf.keySet()) {
			if (key.toString().startsWith("webservice.") || key.toString().startsWith("webdav")
					|| key.toString().startsWith("cmis") || key.toString().startsWith("ftp.")) {
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
			GenericDAO gDao = (GenericDAO) Context.get().getBean(GenericDAO.class);
			int counter = 0;
			ContextProperties conf = Context.get().getProperties();
			for (int i = 0; i < settings.length; i++) {
				if (settings[i] == null || StringUtils.isEmpty(settings[i].getName()))
					continue;

				if (settings[i].getName().endsWith("gui.welcome")) {
					/*
					 * This is a setting we save into the database
					 */
					Generic setting = gDao.findByAlternateKey("guisetting", "gui.welcome", 0L, session.getTenantId());
					if (setting == null)
						setting = new Generic("guisetting", "gui.welcome", 0L, session.getTenantId());
					setting.setString1(settings[i].getValue());
					gDao.store(setting);
				} else if (settings[i].getName().endsWith("gui.tag.vocabulary")) {
					/*
					 * This is a setting we save into the database
					 */
					Generic setting = gDao.findByAlternateKey("guisetting", "gui.tag.vocabulary", 0L,
							session.getTenantId());
					if (setting == null)
						setting = new Generic("guisetting", "gui.tag.vocabulary", 0L, session.getTenantId());
					setting.setString1(settings[i].getValue());
					gDao.store(setting);
				} else {
					conf.setProperty(settings[i].getName(),
							settings[i].getValue() != null ? settings[i].getValue() : "");
					counter++;
				}
			}

			conf.write();

			log.info("Successfully saved {} parameters", counter);
		} catch (Throwable e) {
			ServiceUtil.throwServerException(session, log, e);
		}
	}

	@Override
	public void saveStorageSettings(GUIParameter[] settings) throws ServerException {
		saveSettings(settings);
		Storer storer = (Storer) Context.get().getBean(Storer.class);
		storer.init();
	}

	@Override
	public String[] removeStorage(int storageId) throws ServerException {
		Session session = ServiceUtil.checkMenu(getThreadLocalRequest(), Menu.ADMINISTRATION);

		try {
			ContextProperties config = Context.get().getProperties();
			if (storageId == config.getInt("store.write"))
				throw new Exception(
						"You cannot delete the storage " + storageId + " because it is the current default");

			FolderDAO dao = (FolderDAO) Context.get().getBean(FolderDAO.class);
			List<Folder> folders = (List<Folder>) dao.findByWhere("_entity.storage=" + storageId, null, null);
			if (!folders.isEmpty()) {
				List<String> paths = folders.stream().map(f -> dao.computePathExtended(f.getId()))
						.collect(Collectors.toList());
				paths.sort(null);
				return paths.toArray(new String[0]);
			} else {
				Map<String, String> settings = config.getProperties("store." + storageId + ".");
				for (String setting : settings.keySet())
					config.remove("store." + storageId + "." + setting);
				config.write();
				return null;
			}
		} catch (Throwable e) {
			return (String[]) ServiceUtil.throwServerException(session, log, e);
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
		String tenantName = session.getTenantName();

		ContextProperties conf = Context.get().getProperties();

		List<GUIParameter> params = new ArrayList<GUIParameter>();
		for (Object name : conf.keySet()) {
			if (name.toString().startsWith(tenantName + ".gui"))
				params.add(new GUIParameter(name.toString(), conf.getProperty(name.toString())));
		}

		params.add(new GUIParameter(tenantName + ".upload.maxsize", conf.getProperty(tenantName + ".upload.maxsize")));
		params.add(
				new GUIParameter(tenantName + ".upload.disallow", conf.getProperty(tenantName + ".upload.disallow")));
		params.add(new GUIParameter(tenantName + ".search.hits", conf.getProperty(tenantName + ".search.hits")));
		params.add(new GUIParameter(tenantName + ".search.extattr", conf.getProperty(tenantName + ".search.extattr")));
		params.add(
				new GUIParameter(tenantName + ".session.timeout", conf.getProperty(tenantName + ".session.timeout")));
		params.add(new GUIParameter(tenantName + ".session.heartbeat",
				conf.getProperty(tenantName + ".session.heartbeat")));
		params.add(new GUIParameter(tenantName + ".downloadticket.behavior",
				conf.getProperty(tenantName + ".downloadticket.behavior")));
		params.add(new GUIParameter(tenantName + ".charset", conf.getProperty(tenantName + ".charset")));

		/*
		 * Now go into the DB
		 */
		GenericDAO gDao = (GenericDAO) Context.get().getBean(GenericDAO.class);
		List<Generic> generics = gDao.findByTypeAndSubtype("guisetting", null, null, session.getTenantId());
		for (Generic gen : generics) {
			params.add(new GUIParameter(tenantName + "." + gen.getSubtype(), gen.getString1()));
		}

		return params.toArray(new GUIParameter[0]);
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
			mail.setSubject("Hello from " + SystemInfo.get().getProduct());
			mail.setMessageText("This is a test email from " + SystemInfo.get().getProduct());

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
	public void saveRegistration(String name, String email, String organization, String website)
			throws ServerException {
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
			FormatConverter conv = (FormatConverter) Class.forName(converter).getDeclaredConstructor().newInstance();
			for (String name : conv.getParameterNames())
				parameters.add(new GUIParameter(name, conv.getParameter(name)));
			return parameters.toArray(new GUIParameter[0]);
		} catch (Throwable e) {
			return (GUIParameter[]) ServiceUtil.throwServerException(session, log, e);
		}
	}

	@Override
	public void saveExtensionAliases(String extension, String aliases) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		try {
			ContextProperties config = Context.get().getProperties();
			Map<String, String> aliasMap = config.getProperties("converter.alias.");

			// Get all the keys that correspond to the same target
			// extension
			Set<String> keys = aliasMap.entrySet().stream().filter(entry -> Objects.equals(entry.getValue(), extension))
					.map(Map.Entry::getKey).collect(Collectors.toSet());

			// Delete all actual aliases
			for (String key : keys)
				config.remove("converter.alias." + key);

			// Now add the new ones
			if (StringUtils.isNotEmpty(aliases)) {
				String[] tokens = aliases.split(",");
				for (String token : tokens)
					config.setProperty(
							"converter.alias."
									+ token.toLowerCase().trim().replace(" ", "").replace(".", "").replace("=", ""),
							extension);
			}

			config.write();
		} catch (Throwable e) {
			ServiceUtil.throwServerException(session, log, e);
		}
	}

	@Override
	public GUIParameter[] loadWebserviceStats(Long tenantId) throws ServerException {
		Session session = ServiceUtil.validateSession(getThreadLocalRequest());

		try {
			ServiceUtil.checkMenu(getThreadLocalRequest(), Menu.SETTINGS);

			List<GUIParameter> params = new ArrayList<GUIParameter>();

			// Retrieve API calls stats
			SequenceDAO dao = (SequenceDAO) Context.get().getBean(SequenceDAO.class);
			GUIParameter p = new GUIParameter("webservice.apicalls",
					"" + dao.getCurrentValue("wscall", 0, tenantId != null ? tenantId : Tenant.SYSTEM_ID));
			params.add(p);

			SimpleDateFormat df = new SimpleDateFormat("yyyyMM");
			p = new GUIParameter("webservice.apicalls.current",
					"" + dao.getCurrentValue("wscall-" + df.format(new Date()), 0,
							tenantId != null ? tenantId : Tenant.SYSTEM_ID));
			params.add(p);

			return params.toArray(new GUIParameter[0]);
		} catch (Throwable e) {
			return (GUIParameter[]) ServiceUtil.throwServerException(session, log, e);
		}
	}
}