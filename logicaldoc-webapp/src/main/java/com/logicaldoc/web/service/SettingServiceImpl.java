package com.logicaldoc.web.service;

import java.io.IOException;
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
import org.apache.hc.client5.http.classic.methods.HttpGet;
import org.apache.hc.client5.http.impl.classic.AbstractHttpClientResponseHandler;
import org.apache.hc.client5.http.impl.classic.CloseableHttpClient;
import org.apache.hc.core5.http.ClassicHttpResponse;
import org.apache.hc.core5.http.HttpEntity;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.security.web.firewall.StrictHttpFirewall;

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
import com.logicaldoc.core.store.Store;
import com.logicaldoc.gui.common.client.InvalidSessionServerException;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUIEmailSettings;
import com.logicaldoc.gui.common.client.beans.GUIParameter;
import com.logicaldoc.gui.frontend.client.services.SettingService;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.util.http.HttpUtil;
import com.logicaldoc.util.spring.Context;
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

    private static final String SMTP_SAVE_FOLDER_ID = "smtp.save.folderId";

    private static final String SMTP_SAVE_FOLDERING = "smtp.save.foldering";

    private static final String SMTP_USERASFROM = "smtp.userasfrom";

    private static final String SMTP_SENDER = "smtp.sender";

    private static final String SMTP_AUTH_ENCRYPTED = "smtp.authEncrypted";

    private static final String SMTP_CONNECTION_SECURITY = "smtp.connectionSecurity";

    private static final String SMTP_PASSWORD = "smtp.password";

    private static final String SMTP_USERNAME = "smtp.username";

    private static final String SMTP_CLIENTSECRET = "smtp.clientSecret";

    private static final String SMTP_CLIENTID = "smtp.clientId";

    private static final String SMTP_CLIENTTENANT = "smtp.clientTenant";

    private static final String SMTP_PROTOCOL = "smtp.protocol";

    private static final String SMTP_PORT = "smtp.port";

    private static final String SMTP_HOST = "smtp.host";

    private static final long serialVersionUID = 1L;

    private static final Logger log = LoggerFactory.getLogger(SettingServiceImpl.class);

    @Override
    public GUIEmailSettings loadEmailSettings() throws ServerException {
        Session session = checkMenu(getThreadLocalRequest(), Menu.SETTINGS);

        GUIEmailSettings emailSettings = new GUIEmailSettings();
        try {
            emailSettings.setProtocol(getTenantSetting(SMTP_PROTOCOL, "smtp"));
            emailSettings.setServer(getTenantSetting(SMTP_HOST));
            emailSettings.setPort(Integer.parseInt(getTenantSetting(SMTP_PORT, "25")));
            emailSettings.setUsername(getTenantSetting(SMTP_USERNAME, "").trim());
            emailSettings.setPwd(getTenantSetting(SMTP_PASSWORD, "").trim());
            emailSettings.setConnSecurity(getTenantSetting(SMTP_CONNECTION_SECURITY));
            emailSettings.setSecureAuth(Boolean.parseBoolean(getTenantSetting(SMTP_AUTH_ENCRYPTED, "false")));
            emailSettings.setSenderEmail(getTenantSetting(SMTP_SENDER));
            emailSettings.setUserAsFrom(Boolean.parseBoolean(getTenantSetting(SMTP_USERASFROM, "true")));
            emailSettings.setFoldering(Integer.parseInt(getTenantSetting(SMTP_SAVE_FOLDERING, "3")));
            emailSettings.setTargetFolder(new FolderServiceImpl().getFolder(session,
                    Long.parseLong(getTenantSetting(SMTP_SAVE_FOLDER_ID, "0"))));
            emailSettings.setClientSecret(getTenantSetting(SMTP_CLIENTSECRET, "").trim());
            emailSettings.setClientId(getTenantSetting(SMTP_CLIENTID, "").trim());
            emailSettings.setClientTenant(getTenantSetting(SMTP_CLIENTTENANT, "").trim());

            log.info("Email settings data loaded successfully.");
        } catch (Exception e) {
            log.error("Exception loading Email settings data", e);
        }

        return emailSettings;
    }

    private String getTenantSetting(String tenantName, String setting, String defaultValue) {
        return Context.get().getConfig().getTenantProperty(tenantName, setting, defaultValue);
    }

    private String getTenantSetting(String setting) throws InvalidSessionServerException {
        return getTenantSetting(setting, "");
    }

    private String getTenantSetting(String setting, String defaultValue) throws InvalidSessionServerException {
        Session session = validateSession();
        return getTenantSetting(session.getTenantName(), setting, defaultValue);
    }

    private void setTenantSetting(String setting, String value) throws InvalidSessionServerException {
        Session session = validateSession();
        Context.get().getConfig().setProperty("%s.%s".formatted(session.getTenantName(), setting), value);
    }

    @Override
    public void saveEmailSettings(GUIEmailSettings settings) throws ServerException {
        checkMenu(getThreadLocalRequest(), Menu.SETTINGS);

        try {
            setTenantSetting(SMTP_PROTOCOL, settings.getProtocol());
            setTenantSetting(SMTP_HOST, settings.getServer());
            setTenantSetting(SMTP_PORT, Integer.toString(settings.getPort()));
            setTenantSetting(SMTP_USERNAME, StringUtils.defaultString(settings.getUsername(), ""));
            setTenantSetting(SMTP_PASSWORD, StringUtils.defaultString(settings.getPwd(), ""));
            setTenantSetting(SMTP_CONNECTION_SECURITY, settings.getConnSecurity());
            setTenantSetting(SMTP_AUTH_ENCRYPTED, Boolean.toString(settings.isSecureAuth()));
            setTenantSetting(SMTP_SENDER, settings.getSenderEmail());
            setTenantSetting(SMTP_USERASFROM, Boolean.toString(settings.isUserAsFrom()));
            setTenantSetting(SMTP_SAVE_FOLDERING, Integer.toString(settings.getFoldering()));
            setTenantSetting(SMTP_SAVE_FOLDER_ID,
                    settings.getTargetFolder() != null ? Long.toString(settings.getTargetFolder().getId()) : "");
            setTenantSetting(SMTP_CLIENTID, StringUtils.defaultString(settings.getClientId()));
            setTenantSetting(SMTP_CLIENTSECRET, StringUtils.defaultString(settings.getClientSecret()));
            setTenantSetting(SMTP_CLIENTTENANT, StringUtils.defaultString(settings.getClientTenant()));

            Context.get().getConfig().write();

            // Always update the settings for the default sender
            EMailSender sender = EMailSender.get();
            sender.setHost(getTenantSetting(Tenant.DEFAULT_NAME, SMTP_HOST, "localhost"));
            sender.setPort(Integer.parseInt(getTenantSetting(Tenant.DEFAULT_NAME, SMTP_PORT, "25")));
            sender.setUsername(getTenantSetting(Tenant.DEFAULT_NAME, SMTP_USERNAME, ""));
            sender.setPassword(getTenantSetting(Tenant.DEFAULT_NAME, SMTP_PASSWORD, ""));
            sender.setSender(getTenantSetting(Tenant.DEFAULT_NAME, SMTP_SENDER, ""));
            sender.setAuthEncrypted(
                    Boolean.parseBoolean(getTenantSetting(Tenant.DEFAULT_NAME, SMTP_AUTH_ENCRYPTED, "false")));
            sender.setConnectionSecurity(
                    Integer.parseInt(getTenantSetting(Tenant.DEFAULT_NAME, SMTP_CONNECTION_SECURITY, "")));
            sender.setFoldering(Integer.parseInt(getTenantSetting(Tenant.DEFAULT_NAME, SMTP_SAVE_FOLDERING,
                    Integer.toString(EMailSender.FOLDERING_MONTH))));
            sender.setFolderId(Long.parseLong(getTenantSetting(Tenant.DEFAULT_NAME, SMTP_SAVE_FOLDER_ID, "0")));

            log.info("Email settings data written successfully.");
        } catch (Exception e) {
            log.error("Exception writing Email settings data", e);
        }
    }

    @Override
    public List<GUIParameter> loadSettings() throws ServerException {
        checkMenu(getThreadLocalRequest(), Menu.SETTINGS);

        TreeSet<String> sortedSet = new TreeSet<>();
        ContextProperties conf = Context.get().getConfig();
        for (Object key : conf.keySet()) {
            String name = key.toString();

            if (name.endsWith(".hidden") || name.endsWith("readonly")
                    || "true".equals(conf.getString("%s.hidden".formatted(name))) || name.startsWith("product")
                    || name.startsWith("skin") || name.startsWith("conf") || name.startsWith("ldap")
                    || name.startsWith("schedule") || name.contains(".smtp.") || name.contains("password")
                    || name.startsWith("ad") || name.startsWith("webservice") || name.startsWith("webdav")
                    || name.startsWith("cmis") || name.startsWith("runlevel") || name.startsWith("aspect.")
                    || name.startsWith("stat") || name.contains("index") || name.equals("id") || name.contains(".lang.")
                    || name.startsWith("reg.") || name.startsWith("ocr.") || name.contains(".ocr.")
                    || name.contains("barcode") || name.startsWith("task.") || name.startsWith("store")
                    || name.startsWith("advancedocr.") || name.startsWith("command.") || name.contains(".gui.")
                    || name.contains(".upload.") || name.equals("userno") || name.contains(".search.")
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
                    || name.contains(".downloadticket.") || name.startsWith("zonalocr.") || name.endsWith(".charset")
                    || name.startsWith("policy.") || name.startsWith("cookies.") || name.startsWith("saml.")
                    || name.startsWith("history.") || name.startsWith("proxy.") || name.startsWith("ai.")
                    || name.contains(".ai."))
                continue;

            sortedSet.add(key.toString());
        }

        List<GUIParameter> params = new ArrayList<>();
        for (String key : sortedSet)
            params.add(new GUIParameter(key, conf.getProperty(key)));

        return params;
    }

    @Override
    public List<GUIParameter> loadProtocolSettings() throws ServerException {
        checkMenu(getThreadLocalRequest(), Menu.SETTINGS);

        ContextProperties conf = Context.get().getConfig();
        List<GUIParameter> params = new ArrayList<>();
        for (Object key : conf.keySet())
            if (key.toString().startsWith("webservice.") || key.toString().startsWith("webdav.")
                    || key.toString().startsWith("cmis.") || key.toString().startsWith("ftp."))
                params.add(new GUIParameter(key.toString(), conf.getProperty(key.toString())));

        return params;
    }

    @Override
    public void saveSettings(List<GUIParameter> settings) throws ServerException {
        Session session = checkMenu(getThreadLocalRequest(), Menu.ADMINISTRATION);

        try {
            GenericDAO genericDao = GenericDAO.get();
            int counter = 0;
            ContextProperties conf = Context.get().getConfig();
            for (GUIParameter setting : settings) {
                saveSetting(setting, session, genericDao, conf);
                counter++;
            }

            conf.write();

            log.info("Successfully saved {} parameters", counter);
        } catch (Exception e) {
            throwServerException(session, log, e);
        }
    }

    private void saveSetting(GUIParameter parameter, Session session, GenericDAO genericDao, ContextProperties conf)
            throws PersistenceException {
        if (parameter == null || StringUtils.isEmpty(parameter.getName()))
            return;

        if (parameter.getName().endsWith(GUI_WELCOME)) {
            /*
             * This is a setting we save into the database
             */
            Generic setting = genericDao.findByAlternateKey(GUISETTING, GUI_WELCOME, 0L, session.getTenantId());
            if (setting == null)
                setting = new Generic(GUISETTING, GUI_WELCOME, 0L, session.getTenantId());
            setting.setString1(parameter.getValue());
            genericDao.store(setting);
        } else if (parameter.getName().endsWith(GUI_TAG_VOCABULARY)) {
            /*
             * This is a setting we save into the database
             */
            Generic setting = genericDao.findByAlternateKey(GUISETTING, GUI_TAG_VOCABULARY, 0L, session.getTenantId());
            if (setting == null)
                setting = new Generic(GUISETTING, GUI_TAG_VOCABULARY, 0L, session.getTenantId());
            setting.setString1(parameter.getValue());
            genericDao.store(setting);
        } else {
            conf.setProperty(parameter.getName(), parameter.getValue() != null ? parameter.getValue() : "");
        }
    }

    @Override
    public void saveFirewallSettings(List<GUIParameter> settings) throws ServerException {
        saveSettings(settings);

        ContextProperties config = Context.get().getConfig();

        StrictHttpFirewall firewall = (StrictHttpFirewall) Context.get("httpFirewall");
        firewall.setAllowBackSlash(config.getBoolean("firewall.allowBackSlash", false));
        firewall.setAllowSemicolon(config.getBoolean("firewall.allowSemicolon", false));
        firewall.setAllowUrlEncodedPercent(config.getBoolean("firewall.allowUrlEncodedPercent", false));
        firewall.setAllowUrlEncodedSlash(config.getBoolean("firewall.allowUrlEncodedSlash", false));
        firewall.setAllowUrlEncodedPeriod(config.getBoolean("firewall.allowUrlEncodedPeriod", false));
    }

    @Override
    public void saveStoreSettings(List<GUIParameter> settings) throws ServerException {
        saveSettings(settings.stream().filter(s -> s.getName().startsWith("store")).toList());
        Store.get().init();
    }

    @Override
    public List<String> removeStore(int storeId) throws ServerException {
        Session session = checkMenu(getThreadLocalRequest(), Menu.ADMINISTRATION);

        try {
            ContextProperties config = Context.get().getConfig();
            if (storeId == config.getInt("store.write"))
                throw new ServerException(
                        "You cannot delete the store %d because it is the current default".formatted(storeId));

            FolderDAO dao = FolderDAO.get();

            /*
             * Search for those folders that refer this store
             */
            List<Long> folderIds = dao.queryForList(
                    "select ld_folderid from ld_folder_store where ld_storeid = :storeId and ld_nodeid = :nodeId",
                    Map.of("storeId", storeId, "nodeId", config.getString("id")), Long.class, null);
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
                return paths;
            } else {
                Map<String, String> settings = config.getProperties("store.%d.".formatted(storeId));
                for (String setting : settings.keySet())
                    config.remove("store.%d.%s".formatted(storeId, setting));
                config.write();
                return new ArrayList<>();
            }
        } catch (Exception e) {
            return throwServerException(session, log, e);
        }
    }

    @Override
    public List<GUIParameter> loadSettingsByNames(List<String> names) throws ServerException {
        Session session = validateSession();

        List<GUIParameter> values = new ArrayList<>();
        try {
            ContextProperties conf = Context.get().getConfig();

            for (String name : names) {
                if (name.endsWith("*")) {
                    Map<String, String> map = conf.getProperties(name.substring(0, name.length() - 1));
                    for (Map.Entry<String, String> entry : map.entrySet())
                        values.add(new GUIParameter(entry.getKey(), entry.getValue()));
                } else
                    values.add(new GUIParameter(name, conf.getProperty(name)));
            }
        } catch (Exception e) {
            throwServerException(session, log, e);
        }
        return values;
    }

    @Override
    public List<GUIParameter> loadGUISettings() throws ServerException {
        Session session = checkMenu(getThreadLocalRequest(), Menu.SETTINGS);
        String tenantName = session.getTenantName();

        ContextProperties conf = Context.get().getConfig();

        List<GUIParameter> params = new ArrayList<>();
        for (Object name : conf.keySet()) {
            if (name.toString().startsWith("%s.gui".formatted(tenantName)))
                params.add(new GUIParameter(name.toString(), conf.getProperty(name.toString())));
        }

        params.add(new GUIParameter("%s.upload.maxsize".formatted(tenantName), getTenantSetting("upload.maxsize")));
        params.add(new GUIParameter("%s.upload.disallow".formatted(tenantName), getTenantSetting("upload.disallow")));
        params.add(new GUIParameter("%s.search.hits".formatted(tenantName), getTenantSetting("search.hits")));
        params.add(new GUIParameter("%s.search.extattr".formatted(tenantName), getTenantSetting("search.extattr")));
        params.add(new GUIParameter("%s.session.timeout".formatted(tenantName), getTenantSetting("session.timeout")));
        params.add(
                new GUIParameter("%s.session.heartbeat".formatted(tenantName), getTenantSetting("session.heartbeat")));
        params.add(new GUIParameter("%s.downloadticket.behavior".formatted(tenantName),
                getTenantSetting("downloadticket.behavior")));
        params.add(new GUIParameter("%s.charset".formatted(tenantName), getTenantSetting("charset")));

        /*
         * Now go into the DB
         */

        try {
            GenericDAO gDao = GenericDAO.get();
            List<Generic> generics = gDao.findByTypeAndSubtype(GUISETTING, null, null, session.getTenantId());
            for (Generic gen : generics)
                params.add(new GUIParameter("%s.%s".formatted(tenantName, gen.getSubtype()), gen.getString1()));
        } catch (PersistenceException e) {
            log.warn(e.getMessage(), e);
        }

        return params;
    }

    @Override
    public boolean testEmail(String email) throws ServerException {
        Session session = validateSession();

        ContextProperties config = Context.get().getConfig();
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
            mail.setMessageText("This is a test email from %s".formatted(SystemInfo.get().getProduct()));

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
    public boolean testStore(int id) throws ServerException {
        validateSession();
        try {
            Store store = Store.get().newStore(id);
            log.info("Testing store {}", store);
            return store.test();
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
            ContextProperties conf = Context.get().getConfig();
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
    public List<GUIParameter> loadConverterParameters(String converter) throws ServerException {
        Session session = validateSession();

        List<GUIParameter> parameters = new ArrayList<>();
        try {
            ServletUtil.checkMenu(getThreadLocalRequest(), 1750L);
            FormatConverter conv = (FormatConverter) Class.forName(converter).getDeclaredConstructor().newInstance();
            for (String name : conv.getParameterNames())
                parameters.add(new GUIParameter(name, conv.getParameter(name)));
            return parameters;
        } catch (Exception e) {
            return throwServerException(session, log, e);
        }
    }

    @Override
    public void saveExtensionAliases(String extension, String aliases) throws ServerException {
        Session session = validateSession();

        try {
            ContextProperties config = Context.get().getConfig();
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
    public List<GUIParameter> loadWebserviceStats(Long tenantId) throws ServerException {
        Session session = validateSession();

        try {
            checkMenu(getThreadLocalRequest(), Menu.SETTINGS);

            List<GUIParameter> params = new ArrayList<>();

            // Retrieve API calls stats
            SequenceDAO dao = SequenceDAO.get();
            GUIParameter p = new GUIParameter("webservice.apicalls",
                    Long.toString(dao.getCurrentValue("wscall", 0, tenantId != null ? tenantId : Tenant.SYSTEM_ID)));
            params.add(p);

            SimpleDateFormat df = new SimpleDateFormat("yyyyMM");
            p = new GUIParameter("webservice.apicalls.current",
                    Long.toString(dao.getCurrentValue("wscall-%s".formatted(df.format(new Date())), 0,
                            tenantId != null ? tenantId : Tenant.SYSTEM_ID)));
            params.add(p);

            return params;
        } catch (Exception e) {
            return throwServerException(session, log, e);
        }
    }

    @Override
    public List<GUIParameter> loadAuditingSettings() throws ServerException {
        Session session = checkMenu(getThreadLocalRequest(), Menu.AUDITING);
        String tenantName = session.getTenantName();

        ContextProperties conf = Context.get().getConfig();

        List<GUIParameter> params = new ArrayList<>();
        for (Object name : conf.keySet()) {
            if (name.toString().startsWith("%s.history".formatted(tenantName)) || name.toString().startsWith("history.")
                    || name.toString().equals("webservice.call.ttl"))
                params.add(new GUIParameter(name.toString(), conf.getProperty(name.toString())));
        }

        return params;
    }

    @Override
    public Boolean testProxy(String host, int port, String username, String password) throws ServerException {
        Session session = validateSession();
        HttpGet get = new HttpGet("https://activation.logicaldoc.com");
        try (CloseableHttpClient httpClient = HttpUtil.getNotValidatingClient(40, host, port, username, password);) {
            return httpClient.execute(get, new AbstractHttpClientResponseHandler<Boolean>() {

                @Override
                public Boolean handleResponse(ClassicHttpResponse response) throws IOException {
                    int responseStatusCode = response.getCode();
                    String responseBody = HttpUtil.getBodyString(response);
                    String responseReasonPhrase = StringUtils.defaultString(response.getReasonPhrase());
                    log.debug("Received LM response {} - {} - {}", responseStatusCode, responseReasonPhrase,
                            responseBody);
                    return responseStatusCode == 200 && responseBody.toLowerCase().contains("<body");
                }

                @Override
                public Boolean handleEntity(HttpEntity arg0) throws IOException {
                    return Boolean.FALSE;
                }
            });
        } catch (IOException e) {
            return throwServerException(session, log, e);
        }
    }
}