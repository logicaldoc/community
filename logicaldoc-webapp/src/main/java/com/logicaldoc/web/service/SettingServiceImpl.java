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

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.SystemInfo;
import com.logicaldoc.core.communication.EMail;
import com.logicaldoc.core.communication.EMailSender;
import com.logicaldoc.core.conversion.FormatConverter;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.generic.Generic;
import com.logicaldoc.core.generic.GenericDAO;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.core.security.menu.Menu;
import com.logicaldoc.core.sequence.SequenceDAO;
import com.logicaldoc.core.store.Storer;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUIEmailSettings;
import com.logicaldoc.gui.common.client.beans.GUIParameter;
import com.logicaldoc.gui.frontend.client.services.SettingService;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.util.sql.SqlUtil;
import com.logicaldoc.web.firewall.HttpFirewall;
import com.logicaldoc.web.util.ServletUtil;

/**
 * Implementation of the SettingService
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.0
 */
public class SettingServiceImpl extends AbstractRemoteService implements SettingService {

	private static final String CONVERTER_ALIAS = "converter.alias.";

	private static final String GUI_TAG_VOCABULARY = "gui.tag.vocabulary";

	private static final String GUISETTING = "guisetting";

	private static final String GUI_WELCOME = "gui.welcome";

	private static final String CHARSET = ".charset";

	private static final String HIDDEN = ".hidden";

	private static final String SMTP_SAVE_FOLDER_ID = ".smtp.save.folderId";

	private static final String SMTP_SAVE_FOLDERING = ".smtp.save.foldering";

	private static final String SMTP_USERASFROM = ".smtp.userasfrom";

	private static final String SMTP_SENDER = ".smtp.sender";

	private static final String SMTP_AUTH_ENCRYPTED = ".smtp.authEncrypted";

	private static final String SMTP_CONNECTION_SECURITY = ".smtp.connectionSecurity";

	private static final String SMTP_PASSWORD = ".smtp.password";

	private static final String SMTP_USERNAME = ".smtp.username";

	private static final String SMTP_PORT = ".smtp.port";

	private static final String SMTP_HOST = ".smtp.host";

	private static final long serialVersionUID = 1L;

	private static Logger log = LoggerFactory.getLogger(SettingServiceImpl.class);

	@Override
	public GUIEmailSettings loadEmailSettings() throws ServerException {
		Session session = checkMenu(getThreadLocalRequest(), Menu.SETTINGS);

		GUIEmailSettings emailSettings = new GUIEmailSettings();
		try {
			ContextProperties conf = Context.get().getProperties();

			emailSettings.setSmtpServer(conf.getProperty(session.getTenantName() + SMTP_HOST));
			emailSettings.setPort(Integer.parseInt(conf.getProperty(session.getTenantName() + SMTP_PORT)));
			emailSettings.setUsername(!conf.getProperty(session.getTenantName() + SMTP_USERNAME).trim().isEmpty()
					? conf.getProperty(session.getTenantName() + SMTP_USERNAME)
					: "");
			emailSettings.setPwd(!conf.getProperty(session.getTenantName() + SMTP_PASSWORD).trim().isEmpty()
					? conf.getProperty(session.getTenantName() + SMTP_PASSWORD)
					: "");
			emailSettings.setConnSecurity(conf.getProperty(session.getTenantName() + SMTP_CONNECTION_SECURITY));
			emailSettings.setSecureAuth("true".equals(conf.getProperty(session.getTenantName() + SMTP_AUTH_ENCRYPTED)));
			emailSettings.setSenderEmail(conf.getProperty(session.getTenantName() + SMTP_SENDER));
			emailSettings.setUserAsFrom(conf.getBoolean(session.getTenantName() + SMTP_USERASFROM, true));
			emailSettings.setFoldering(conf.getInt(session.getTenantName() + SMTP_SAVE_FOLDERING, 3));

			emailSettings.setTargetFolder(new FolderServiceImpl().getFolder(session,
					conf.getLong(session.getTenantName() + SMTP_SAVE_FOLDER_ID, 0)));

			log.info("Email settings data loaded successfully.");
		} catch (Exception e) {
			log.error("Exception loading Email settings data: {}", e.getMessage(), e);
		}

		return emailSettings;
	}

	@Override
	public void saveEmailSettings(GUIEmailSettings settings) throws ServerException {
		Session session = checkMenu(getThreadLocalRequest(), Menu.SETTINGS);

		try {
			ContextProperties conf = Context.get().getProperties();

			conf.setProperty(session.getTenantName() + SMTP_HOST, settings.getSmtpServer());
			conf.setProperty(session.getTenantName() + SMTP_PORT, Integer.toString(settings.getPort()));
			conf.setProperty(session.getTenantName() + SMTP_USERNAME,
					!settings.getUsername().trim().isEmpty() ? settings.getUsername() : "");
			conf.setProperty(session.getTenantName() + SMTP_PASSWORD,
					!settings.getPwd().trim().isEmpty() ? settings.getPwd() : "");
			conf.setProperty(session.getTenantName() + SMTP_CONNECTION_SECURITY, settings.getConnSecurity());
			conf.setProperty(session.getTenantName() + SMTP_AUTH_ENCRYPTED, settings.isSecureAuth() ? "true" : "false");
			conf.setProperty(session.getTenantName() + SMTP_SENDER, settings.getSenderEmail());
			conf.setProperty(session.getTenantName() + SMTP_USERASFROM, "" + settings.isUserAsFrom());
			conf.setProperty(session.getTenantName() + SMTP_SAVE_FOLDERING, Integer.toString(settings.getFoldering()));
			conf.setProperty(session.getTenantName() + SMTP_SAVE_FOLDER_ID,
					settings.getTargetFolder() != null ? Long.toString(settings.getTargetFolder().getId()) : "");

			conf.write();

			// Always update the settings for the default sencer
			EMailSender sender = (EMailSender) Context.get().getBean(EMailSender.class);
			sender.setHost(conf.getProperty(Tenant.DEFAULT_NAME + SMTP_HOST));
			sender.setPort(Integer.parseInt(conf.getProperty(Tenant.DEFAULT_NAME + SMTP_PORT)));
			sender.setUsername(conf.getProperty(Tenant.DEFAULT_NAME + SMTP_USERNAME));
			sender.setPassword(conf.getProperty(Tenant.DEFAULT_NAME + SMTP_PASSWORD));
			sender.setSender(conf.getProperty(Tenant.DEFAULT_NAME + SMTP_SENDER));
			sender.setAuthEncrypted("true".equals(conf.getProperty(Tenant.DEFAULT_NAME + SMTP_AUTH_ENCRYPTED)));
			sender.setConnectionSecurity(
					Integer.parseInt(conf.getProperty(Tenant.DEFAULT_NAME + SMTP_CONNECTION_SECURITY)));
			sender.setFoldering(conf.getInt(Tenant.DEFAULT_NAME + SMTP_SAVE_FOLDERING, EMailSender.FOLDERING_MONTH));
			sender.setFolderId(conf.getProperty(Tenant.DEFAULT_NAME + SMTP_SAVE_FOLDER_ID) != null
					? conf.getLong(Tenant.DEFAULT_NAME + SMTP_SAVE_FOLDER_ID, 0L)
					: null);

			log.info("Email settings data written successfully.");
		} catch (Exception e) {
			log.error("Exception writing Email settings data: {}", e.getMessage(), e);
		}
	}

	@Override
	public GUIParameter[] loadSettings() throws ServerException {
		checkMenu(getThreadLocalRequest(), Menu.SETTINGS);

		TreeSet<String> sortedSet = new TreeSet<>();
		ContextProperties conf = Context.get().getProperties();
		for (Object key : conf.keySet()) {
			String name = key.toString();

			if (name.endsWith(HIDDEN) || name.endsWith("readonly")
					|| (conf.containsKey(name + HIDDEN) && "true".equals(conf.getProperty(name + HIDDEN)))
					|| name.startsWith("product") || name.startsWith("skin") || name.startsWith("conf")
					|| name.startsWith("ldap") || name.startsWith("schedule") || name.contains(".smtp.")
					|| name.contains("password") || name.startsWith("ad") || name.startsWith("webservice")
					|| name.startsWith("webdav") || name.startsWith("cmis") || name.startsWith("runlevel")
					|| name.startsWith("aspect.") || name.startsWith("stat") || name.contains("index")
					|| name.equals("id") || name.contains(".lang.") || name.startsWith("reg.")
					|| name.startsWith("ocr.") || name.contains(".ocr.") || name.contains("barcode")
					|| name.startsWith("task.") || name.startsWith("store") || name.startsWith("advancedocr.")
					|| name.startsWith("command.") || name.contains(".gui.") || name.contains(".upload.")
					|| name.equals("userno") || name.contains(".search.") || name.contains("tag.")
					|| name.startsWith("cluster") || name.startsWith("ip.") || name.contains(".extcall.")
					|| name.contains("anonymous") || name.startsWith("hibernate.") || name.contains(".session.")
					|| name.contains("antivirus.") || name.startsWith("login.") || name.equals("upload.maxsize")
					|| name.startsWith("news.") || name.equals("registry") || name.equals("searchengine")
					|| name.equals("load") || name.startsWith("ssl.") || name.contains(".tagcloud.")
					|| name.startsWith("throttle.") || name.contains("security.") || name.contains("parser.")
					|| name.startsWith("quota.") || name.equals("initialized") || name.startsWith("converter.")
					|| name.startsWith("firewall.") || name.contains(".2fa.") || name.startsWith("ftp.")
					|| name.startsWith("cas.") || name.startsWith("cache.") || name.startsWith("jdbc.")
					|| name.startsWith("comparator.") || name.contains(".via.") || name.contains(".downloadticket.")
					|| name.startsWith("zonalocr.") || name.endsWith(CHARSET) || name.startsWith("policy.")
					|| name.startsWith("cookies.") || name.startsWith("saml."))
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
		checkMenu(getThreadLocalRequest(), Menu.SETTINGS);

		ContextProperties conf = Context.get().getProperties();
		List<GUIParameter> params = new ArrayList<>();
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
		Session session = checkMenu(getThreadLocalRequest(), Menu.ADMINISTRATION);

		try {
			GenericDAO genericDao = (GenericDAO) Context.get().getBean(GenericDAO.class);
			int counter = 0;
			ContextProperties conf = Context.get().getProperties();
			for (int i = 0; i < settings.length; i++) {
				if (settings[i] == null || StringUtils.isEmpty(settings[i].getName()))
					continue;

				extracted(settings, i, session, genericDao, conf);
				counter++;
			}

			conf.write();

			log.info("Successfully saved {} parameters", counter);
		} catch (Exception e) {
			throwServerException(session, log, e);
		}
	}

	private void extracted(GUIParameter[] settings, int settingIndex, Session session, GenericDAO genericDao,
			ContextProperties conf) throws PersistenceException {
		if (settings[settingIndex].getName().endsWith(GUI_WELCOME)) {
			/*
			 * This is a setting we save into the database
			 */
			Generic setting = genericDao.findByAlternateKey(GUISETTING, GUI_WELCOME, 0L, session.getTenantId());
			if (setting == null)
				setting = new Generic(GUISETTING, GUI_WELCOME, 0L, session.getTenantId());
			setting.setString1(settings[settingIndex].getValue());
			genericDao.store(setting);
		} else if (settings[settingIndex].getName().endsWith(GUI_TAG_VOCABULARY)) {
			/*
			 * This is a setting we save into the database
			 */
			Generic setting = genericDao.findByAlternateKey(GUISETTING, GUI_TAG_VOCABULARY, 0L, session.getTenantId());
			if (setting == null)
				setting = new Generic(GUISETTING, GUI_TAG_VOCABULARY, 0L, session.getTenantId());
			setting.setString1(settings[settingIndex].getValue());
			genericDao.store(setting);
		} else {
			conf.setProperty(settings[settingIndex].getName(),
					settings[settingIndex].getValue() != null ? settings[settingIndex].getValue() : "");
		}
	}

	@Override
	public void saveFirewallSettings(GUIParameter[] settings) throws ServerException {
		saveSettings(settings);

		HttpFirewall firewall = (HttpFirewall) Context.get().getBean(HttpFirewall.class);
		ContextProperties config = Context.get().getProperties();

		firewall.setAllowBackSlash(config.getBoolean("firewall.allowBackSlash", false));
		firewall.setAllowSemicolon(config.getBoolean("firewall.allowSemicolon", false));
		firewall.setAllowUrlEncodedPercent(config.getBoolean("firewall.allowUrlEncodedPercent", false));
		firewall.setAllowUrlEncodedSlash(config.getBoolean("firewall.allowUrlEncodedSlash", false));
		firewall.setAllowUrlEncodedPeriod(config.getBoolean("firewall.allowUrlEncodedPeriod", false));
	}

	@Override
	public void saveStorageSettings(GUIParameter[] settings) throws ServerException {
		saveSettings(settings);
		Storer storer = (Storer) Context.get().getBean(Storer.class);
		storer.init();
	}

	@Override
	public String[] removeStorage(int storageId) throws ServerException {
		Session session = checkMenu(getThreadLocalRequest(), Menu.ADMINISTRATION);

		try {
			ContextProperties config = Context.get().getProperties();
			if (storageId == config.getInt("store.write"))
				throw new ServerException(
						"You cannot delete the storage " + storageId + " because it is the current default");

			FolderDAO dao = (FolderDAO) Context.get().getBean(FolderDAO.class);

			/*
			 * Search for those folders that refer this storage
			 */
			@SuppressWarnings("unchecked")
			List<Long> folderIds = dao.queryForList(
					"select ld_folderid from ld_folder_storage where ld_storageid = " + storageId + " and ld_nodeid = '"
							+ SqlUtil.doubleQuotesAndBackslashes(config.getProperty("id")) + "'",
					Long.class);
			if (!folderIds.isEmpty()) {
				List<String> paths = folderIds.stream().map(f -> {
					try {
						return dao.computePathExtended(f);
					} catch (PersistenceException e) {
						log.error(e.getMessage(), e);
						return "";
					}
				}).collect(Collectors.toList());
				paths.sort(null);
				return paths.toArray(new String[0]);
			} else {
				Map<String, String> settings = config.getProperties("store." + storageId + ".");
				for (String setting : settings.keySet())
					config.remove("store." + storageId + "." + setting);
				config.write();
				return new String[0];
			}
		} catch (Exception e) {
			return (String[]) throwServerException(session, log, e);
		}
	}

	@Override
	public GUIParameter[] loadSettingsByNames(String[] names) throws ServerException {
		Session session = validateSession();

		List<GUIParameter> values = new ArrayList<>();
		try {
			ContextProperties conf = Context.get().getProperties();

			for (int i = 0; i < names.length; i++) {
				if (names[i].endsWith("*")) {
					Map<String, String> map = conf.getProperties(names[i].substring(0, names[i].length() - 1));
					for (Map.Entry<String, String> entry : map.entrySet())
						values.add(new GUIParameter(entry.getKey(), entry.getValue()));
				} else
					values.add(new GUIParameter(names[i], conf.getProperty(names[i])));
			}
		} catch (Exception e) {
			throwServerException(session, log, e);
		}
		return values.toArray(new GUIParameter[0]);
	}

	@Override
	public GUIParameter[] loadGUISettings() throws ServerException {
		Session session = checkMenu(getThreadLocalRequest(), Menu.SETTINGS);
		String tenantName = session.getTenantName();

		ContextProperties conf = Context.get().getProperties();

		List<GUIParameter> params = new ArrayList<>();
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
		params.add(new GUIParameter(tenantName + CHARSET, conf.getProperty(tenantName + CHARSET)));

		/*
		 * Now go into the DB
		 */
		GenericDAO gDao = (GenericDAO) Context.get().getBean(GenericDAO.class);
		List<Generic> generics = gDao.findByTypeAndSubtype(GUISETTING, null, null, session.getTenantId());
		for (Generic gen : generics) {
			params.add(new GUIParameter(tenantName + "." + gen.getSubtype(), gen.getString1()));
		}

		return params.toArray(new GUIParameter[0]);
	}

	@Override
	public boolean testEmail(String email) throws ServerException {
		Session session = validateSession();

		ContextProperties config = Context.get().getProperties();
		EMailSender sender = new EMailSender(session.getTenantName());

		try {
			EMail mail;
			mail = new EMail();
			mail.setAccountId(-1);
			mail.setAuthor(config.getProperty(session.getTenantName() + SMTP_SENDER));
			mail.setAuthorAddress(config.getProperty(session.getTenantName() + SMTP_SENDER));
			mail.parseRecipients(email);
			mail.setFolder("outbox");
			mail.setSentDate(new Date());
			mail.setSubject("Hello from " + SystemInfo.get().getProduct());
			mail.setMessageText("This is a test email from " + SystemInfo.get().getProduct());

			log.info("Sending test email to {}", email);
			sender.send(mail);
			log.info("Test email sent");
			return true;
		} catch (Exception e) {
			log.error(e.getMessage(), e);
			return false;
		}
	}

	@Override
	public boolean testStorage(int id) throws ServerException {
		validateSession();
		try {
			Storer manager = (Storer) Context.get().getBean(Storer.class);
			Storer storer = manager.newStorer(id);
			log.info("Testing storer {}", storer);
			return storer.test();
		} catch (Exception e) {
			log.error(e.getMessage(), e);
			return false;
		}
	}

	@Override
	public void saveRegistration(String name, String email, String organization, String website)
			throws ServerException {
		Session session = validateSession();

		try {
			ContextProperties conf = Context.get().getProperties();
			conf.setProperty("reg.name", name != null ? name : "");
			conf.setProperty("reg.email", email != null ? email : "");
			conf.setProperty("reg.organization", organization != null ? organization : "");
			conf.setProperty("reg.website", website != null ? website : "");

			conf.write();

			log.info("Successfully saved registration informations");
		} catch (Exception e) {
			throwServerException(session, log, e);
		}
	}

	@Override
	public GUIParameter[] loadConverterParameters(String converter) throws ServerException {
		Session session = validateSession();

		List<GUIParameter> parameters = new ArrayList<>();
		try {
			ServletUtil.checkMenu(getThreadLocalRequest(), 1750L);
			FormatConverter conv = (FormatConverter) Class.forName(converter).getDeclaredConstructor().newInstance();
			for (String name : conv.getParameterNames())
				parameters.add(new GUIParameter(name, conv.getParameter(name)));
			return parameters.toArray(new GUIParameter[0]);
		} catch (Exception e) {
			return (GUIParameter[]) throwServerException(session, log, e);
		}
	}

	@Override
	public void saveExtensionAliases(String extension, String aliases) throws ServerException {
		Session session = validateSession();

		try {
			ContextProperties config = Context.get().getProperties();
			Map<String, String> aliasMap = config.getProperties(CONVERTER_ALIAS);

			// Get all the keys that correspond to the same target
			// extension
			Set<String> keys = aliasMap.entrySet().stream().filter(entry -> Objects.equals(entry.getValue(), extension))
					.map(Map.Entry::getKey).collect(Collectors.toSet());

			// Delete all actual aliases
			for (String key : keys)
				config.remove(CONVERTER_ALIAS + key);

			// Now add the new ones
			if (StringUtils.isNotEmpty(aliases)) {
				String[] tokens = aliases.split(",");
				for (String token : tokens)
					config.setProperty(
							CONVERTER_ALIAS
									+ token.toLowerCase().trim().replace(" ", "").replace(".", "").replace("=", ""),
							extension);
			}

			config.write();
		} catch (Exception e) {
			throwServerException(session, log, e);
		}
	}

	@Override
	public GUIParameter[] loadWebserviceStats(Long tenantId) throws ServerException {
		Session session = validateSession();

		try {
			checkMenu(getThreadLocalRequest(), Menu.SETTINGS);

			List<GUIParameter> params = new ArrayList<>();

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
		} catch (Exception e) {
			return (GUIParameter[]) throwServerException(session, log, e);
		}
	}
}