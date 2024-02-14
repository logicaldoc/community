package com.logicaldoc.web.service;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.ResourceBundle;

import javax.servlet.http.HttpServletRequest;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.cronutils.descriptor.CronDescriptor;
import com.cronutils.model.CronType;
import com.cronutils.model.definition.CronDefinition;
import com.cronutils.model.definition.CronDefinitionBuilder;
import com.cronutils.parser.CronParser;
import com.logicaldoc.core.SystemInfo;
import com.logicaldoc.core.communication.Message;
import com.logicaldoc.core.communication.SystemMessageDAO;
import com.logicaldoc.core.generic.Generic;
import com.logicaldoc.core.generic.GenericDAO;
import com.logicaldoc.core.i18n.Language;
import com.logicaldoc.core.i18n.LanguageManager;
import com.logicaldoc.core.metadata.Attribute;
import com.logicaldoc.core.metadata.AttributeSetDAO;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.SessionManager;
import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.core.security.TenantDAO;
import com.logicaldoc.core.security.user.UserDAO;
import com.logicaldoc.gui.common.client.InvalidSessionServerException;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUIAttribute;
import com.logicaldoc.gui.common.client.beans.GUIAttributeSet;
import com.logicaldoc.gui.common.client.beans.GUIInfo;
import com.logicaldoc.gui.common.client.beans.GUIMessage;
import com.logicaldoc.gui.common.client.beans.GUIParameter;
import com.logicaldoc.gui.common.client.beans.GUITenant;
import com.logicaldoc.gui.common.client.beans.GUIValue;
import com.logicaldoc.gui.common.client.services.InfoService;
import com.logicaldoc.i18n.I18N;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.LocaleUtil;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.web.listener.ApplicationListener;

/**
 * Implementation of the InfoService
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class InfoServiceImpl extends AbstractRemoteService implements InfoService {

	private static final String ENABLED = "enabled";

	private static final String LANG = ".lang.";

	private static Logger log = LoggerFactory.getLogger(InfoServiceImpl.class);

	private static final long serialVersionUID = 1L;

	@Override
	public GUIInfo getInfo(String locale, String tenantName, boolean login) throws ServerException {
		GUIInfo info = null;

		try {
			info = getInfo(tenantName);
			info.setBundle(getBundle(locale, tenantName));

			Locale withLocale = LocaleUtil.toLocale(locale);

			List<GUIValue> supportedLanguages = setSupportedLocales(tenantName, info, withLocale);

			LanguageManager manager = LanguageManager.getInstance();
			List<Language> languages = manager.getActiveLanguages(tenantName);
			supportedLanguages.clear();
			for (Language language : languages) {
				Locale lc = language.getLocale();
				GUIValue l = new GUIValue();
				l.setCode(lc.toString());
				l.setValue(lc.getDisplayName(withLocale));
				supportedLanguages.add(l);
			}
			info.setSupportedLanguages(supportedLanguages);

			List<GUIMessage> alerts = new ArrayList<>();

			// Checks if LogicalDOC has been initialized
			ContextProperties config = Context.get().getProperties();
			boolean needSetup = config.getProperty("jdbc.url").startsWith("jdbc:hsqldb:mem:");
			if (!needSetup)
				needSetup = "false".equals(config.getProperty("initialized", "true"));

			if (needSetup) {
				GUIMessage setupReminder = new GUIMessage();
				setupReminder.setMessage(getValue(info, "setup.reminder"));
				HttpServletRequest request = this.getThreadLocalRequest();
				if (request != null) {
					String urlPrefix = request.getScheme() + "://" + request.getServerName() + ":"
							+ request.getServerPort() + request.getContextPath();
					setupReminder.setUrl(urlPrefix + "/setup");
				}
				alerts.add(setupReminder);
			} else {
				// Check if the application needs to be restarted
				checkRestrartRequired(info, locale, alerts);
			}

			info.setAlerts(alerts);

			setAttributes(info, tenantName);

			return info;
		} catch (Exception t) {
			throw new ServerException(t.getMessage());
		}
	}

	private void setAttributes(GUIInfo info, String tenantName) {
		TenantDAO tDAO = (TenantDAO) Context.get().getBean(TenantDAO.class);
		AttributeSetDAO aDAO = (AttributeSetDAO) Context.get().getBean(AttributeSetDAO.class);
		try {
			Map<String, Attribute> attributes = aDAO.findAttributes(tDAO.findByName(tenantName).getId(), null);
			List<GUIAttribute> guiAttributes = new ArrayList<>();

			for (Map.Entry<String, Attribute> entry : attributes.entrySet()) {
				Attribute att = entry.getValue();
				GUIAttribute guiAtt = new GUIAttribute();
				guiAtt.setName(entry.getKey());
				guiAtt.setStringValue(att.getStringValue());
				guiAtt.setBooleanValue(att.getBooleanValue());
				guiAtt.setDoubleValue(att.getDoubleValue());
				guiAtt.setEditor(att.getEditor());
				guiAtt.setHidden(att.getHidden() == 1);
				guiAtt.setReadonly(att.getReadonly() == 1);
				guiAtt.setIntValue(att.getIntValue());
				guiAtt.setLabel(att.getLabel());
				guiAtt.setMandatory(att.getMandatory() == 1);
				guiAtt.setMultiple(att.getMultiple() == 1);
				guiAtt.setPosition(att.getPosition());
				guiAtt.setSetId(att.getSetId());
				guiAtt.setType(att.getType());
				guiAttributes.add(guiAtt);
			}
			info.setAttributeDefinitions(guiAttributes);
		} catch (Exception t) {
			log.error(t.getMessage(), t);
		}
	}

	private void checkRestrartRequired(GUIInfo info, String locale, List<GUIMessage> alerts) {
		if (ApplicationListener.isRestartRequired()) {
			GUIMessage restartReminder = new GUIMessage();
			restartReminder.setMessage(getValue(info, "needrestart"));
			alerts.add(restartReminder);
		} else {
			// Check if the database is connected
			UserDAO dao = (UserDAO) Context.get().getBean(UserDAO.class);
			int test = -1;
			try {
				test = dao.queryForInt("select count(*) from ld_user");
			} catch (Exception t) {
				test = -1;
			}
			if (test < 1) {
				info.setDatabaseConnected(false);
				GUIMessage m = new GUIMessage();
				m.setMessage(I18N.message("databasenotconnected", locale));
				alerts.add(m);
			}
		}
	}

	private ArrayList<GUIValue> setSupportedLocales(String tenantName, GUIInfo info, Locale withLocale) {
		ArrayList<GUIValue> supportedLanguages = new ArrayList<>();
		List<String> installedLocales = I18N.getLocales();
		for (String loc : installedLocales) {
			if (ENABLED.equals(Context.get().getProperties().getProperty(tenantName + LANG + loc + ".gui"))) {
				Locale lc = LocaleUtil.toLocale(loc);
				GUIValue l = new GUIValue();
				l.setCode(loc);
				l.setValue(lc.getDisplayName(withLocale));
				supportedLanguages.add(l);
			}
		}

		info.setSupportedGUILanguages(supportedLanguages);
		return supportedLanguages;
	}

	/**
	 * Retrieves the informations but not localization issues like messages and
	 * installed languages
	 * 
	 * @param tenantName name of the tenant
	 * 
	 * @return bean carrying informations about the User Interface
	 */
	public static GUIInfo getInfo(String tenantName) {
		ContextProperties config = Context.get().getProperties();

		String tname = tenantName;
		if (tname == null)
			tname = Tenant.DEFAULT_NAME;

		GUITenant tenant = null;
		try {
			tenant = SecurityServiceImpl.getTenant(tname);
		} catch (Exception t) {
			// Before setup we may have exception here
			log.warn(t.getMessage());
		}

		// If no tenant was found, go with the default one
		if (tenant == null) {
			tenant = new GUITenant();
			tenant.setName(Tenant.DEFAULT_NAME);
			tenant.setId(Tenant.DEFAULT_ID);
		}

		/*
		 * Populate the infos from the SystemInfo
		 */
		GUIInfo guiInfo = new GUIInfo();
		guiInfo.setTenant(tenant);
		guiInfo.setSessionHeartbeat(config.getInt(tname + ".session.heartbeat", 60));

		SystemInfo info = SystemInfo.get(tenant.getId());
		guiInfo.setLicensee(info.getLicensee());
		guiInfo.setRelease(info.getRelease());
		guiInfo.setRunLevel(info.getRunLevel());
		guiInfo.setYear(info.getYear());
		guiInfo.setHostName(info.getHostName());
		guiInfo.setDate(info.getDate());
		guiInfo.setInstallationId(info.getInstallationId());
		guiInfo.setFeatures(info.getFeatures());

		guiInfo.getBranding().setBugs(info.getBugs());
		guiInfo.getBranding().setForum(info.getForum());
		guiInfo.getBranding().setHelp(info.getHelp());
		guiInfo.getBranding().setProduct(info.getProduct());
		guiInfo.getBranding().setProductName(info.getProductName());
		guiInfo.getBranding().setSupport(info.getSupport());
		guiInfo.getBranding().setUrl(info.getUrl());
		guiInfo.getBranding().setVendor(info.getVendor());
		guiInfo.getBranding().setVendorAddress(info.getVendorAddress());
		guiInfo.getBranding().setVendorCap(info.getVendorCap());
		guiInfo.getBranding().setVendorCity(info.getVendorCity());
		guiInfo.getBranding().setVendorCountry(info.getVendorCountry());

		try {
			List<GUIValue> values = new ArrayList<>();
			for (Object key : config.keySet()) {
				GUIValue pair = new GUIValue();
				String keyString = (String) key;
				pair.setCode(keyString);
				pair.setValue(config.getProperty(keyString));
				values.add(pair);
			}

			loadGUISettingsFromDB(tenant, values);

			guiInfo.setConfig(values);
		} catch (Exception t) {
			log.warn("cannot load GUI settings", t);
		}

		/*
		 * Loads the default attribute set
		 */
		try {
			GUIAttributeSet defaultSet = new AttributeSetServiceImpl().getAttributeSet("default");
			guiInfo.setDefaultAttributeSet(defaultSet);
		} catch (Exception t) {
			// Nothing to do
		}

		return guiInfo;
	}

	private static void loadGUISettingsFromDB(GUITenant tenant, List<GUIValue> values) {
		try {
			GenericDAO dao = (GenericDAO) Context.get().getBean(GenericDAO.class);
			List<Generic> dbSettings = dao.findByTypeAndSubtype("guisetting", null, 0L, tenant.getId());
			for (Generic generic : dbSettings)
				values.add(new GUIValue(generic.getSubtype(), generic.getString1()));
		} catch (Exception t) {
			log.warn("cannot load GUI settings from the database", t);
		}
	}

	protected static List<GUIValue> getBundle(String locale, String tenantName) {
		Locale l = getLocaleForBundle(tenantName, LocaleUtil.toLocale(locale));

		ResourceBundle rb = ResourceBundle.getBundle("i18n.messages", l);
		List<GUIValue> buf = new ArrayList<>();
		for (String key : rb.keySet()) {
			GUIValue entry = new GUIValue();
			entry.setCode(key);
			entry.setValue(rb.getString(key));
			buf.add(entry);
		}
		return buf;
	}

	private static Locale getLocaleForBundle(String tenantName, Locale proposedLocale) {

		/*
		 * Check if the given locale is active and if the case peek an active
		 * locale
		 */
		List<String> installedLocales = I18N.getLocales();

		ContextProperties config = Context.get().getProperties();

		Locale locale = null;
		for (String loc : installedLocales) {
			if (ENABLED.equals(config.getProperty(tenantName + LANG + loc + ".gui"))
					&& loc.equals(proposedLocale.toString())) {
				locale = LocaleUtil.toLocale(loc);
				break;
			}
		}

		if (locale == null)
			for (String loc : installedLocales) {
				if (ENABLED.equals(config.getProperty(tenantName + LANG + loc + ".gui"))) {
					Locale x = LocaleUtil.toLocale(loc);
					if (proposedLocale.getLanguage().equals(x.getLanguage())) {
						locale = LocaleUtil.toLocale(loc);
						break;
					}
				}
			}

		if (locale == null)
			locale = Locale.ENGLISH;
		return locale;
	}

	protected String getValue(GUIInfo info, String message) {
		for (GUIValue valuePair : info.getBundle()) {
			if (valuePair.getCode().equals(message)) {
				return valuePair.getValue();
			}
		}
		return "";
	}

	@Override
	public List<GUIParameter> getSessionInfo() throws InvalidSessionServerException {
		Session session = validateSession();
		log.debug("Requested info for session {}", session.getSid());

		try {
			SystemMessageDAO messageDao = (SystemMessageDAO) Context.get().getBean(SystemMessageDAO.class);
			List<GUIParameter> parameters = new ArrayList<>();

			GUIParameter messages = new GUIParameter("messages",
					"" + messageDao.getUnreadCount(session.getUsername(), Message.TYPE_SYSTEM));
			parameters.add(messages);
			parameters.add(new GUIParameter("valid", "" + SessionManager.get().isOpen(session.getSid())));

			return parameters;
		} catch (Exception t) {
			throw new InvalidSessionServerException(t.getMessage());
		}
	}

	@Override
	public boolean ping() throws InvalidSessionServerException {
		try {
			Session session = validateSession();
			if (session == null)
				return false;
		} catch (Exception t) {
			return false;
		}
		return true;
	}

	@Override
	public String getCronDescription(String expression, String locale) throws ServerException {
		Session session = validateSession();

		try {
			CronDefinition cronDefinition = CronDefinitionBuilder.instanceDefinitionFor(CronType.QUARTZ);
			CronParser parser = new CronParser(cronDefinition);

			// parse some expression and ask descriptor for description
			CronDescriptor descriptor = CronDescriptor.instance(LocaleUtil.toLocale(locale));
			return descriptor.describe(parser.parse(expression));
		} catch (Exception e) {
			return (String) throwServerException(session, log, e);
		}
	}
}