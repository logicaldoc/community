package com.logicaldoc.web.service;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUIEmailSettings;
import com.logicaldoc.gui.common.client.beans.GUIParameter;
import com.logicaldoc.util.spring.Context;
import com.logicaldoc.web.AbstractWPTestCase;

/**
 * Test case for {@link SettingServiceImpl}
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class SettingServiceImplTest extends AbstractWPTestCase {

    private static final Logger log = LoggerFactory.getLogger(SettingServiceImplTest.class);

    // Instance under test
    private SettingServiceImpl testSubject = new SettingServiceImpl();

    @Test
    public void testSaveEmailSettings() throws ServerException {
        GUIEmailSettings emailSettings = new GUIEmailSettings();
        emailSettings.setServer("smtp.logicalobjects.it");
        emailSettings.setPort(8080);
        emailSettings.setUsername("admin");
        emailSettings.setPwd("pippo");
        emailSettings.setConnSecurity(GUIEmailSettings.SECURITY_TLS);
        emailSettings.setSecureAuth(true);
        emailSettings.setSenderEmail("mario@acme.com");

        try {
            testSubject.saveEmailSettings(emailSettings);
        } catch (Exception t) {
            fail("Got error %s".formatted(t.getMessage()));
        }
    }

    @Test
    public void testSaveRegistration() throws ServerException, IOException {
        assertNull(Context.get().getConfig().getString("reg.name"));
        try {
            testSubject.saveRegistration("acme", "john@acme.com", "Acme", "https://www.acme.com");
            assertEquals("acme", Context.get().getConfig().getString("reg.name"));
        } finally {
            Context.get().getConfig().remove("reg.name");
            Context.get().getConfig().write();
        }
    }

    @Test
    public void testSaveExtensionAliases() throws ServerException, IOException {
        assertNull(Context.get().getConfig().getString("converter.alias.eft"));
        try {
            testSubject.saveExtensionAliases("txt", "eft,pqt");

            assertEquals("txt", Context.get().getConfig().getString("converter.alias.eft"));
            assertEquals("txt", Context.get().getConfig().getString("converter.alias.pqt"));
        } finally {
            Context.get().getConfig().remove("converter.alias.eft");
            Context.get().getConfig().remove("converter.alias.pqt");
            Context.get().getConfig().write();
        }
    }

    @Test
    public void testLoadConverterParameters() throws ServerException {
        List<GUIParameter> settings = testSubject.loadConverterParameters(DummyConverter.class.getName());
        assertEquals("30", settings.stream().filter(s -> "timeout".equals(s.getName())).map(GUIParameter::getValue)
                .findFirst().orElse(null));
    }

    @Test
    public void testLoadAuditingSettings() throws ServerException {
        List<GUIParameter> settings = testSubject.loadAuditingSettings();
        assertEquals("all", settings.stream().filter(s -> "default.history.events".equals(s.getName()))
                .map(GUIParameter::getValue).findFirst().orElse(null));
    }

    @Test
    public void testLoadWebserviceStats() throws ServerException {
        List<GUIParameter> settings = testSubject.loadWebserviceStats(null);
        assertFalse(settings.isEmpty());
        assertEquals("0", settings.stream().filter(s -> "webservice.apicalls".equals(s.getName()))
                .map(GUIParameter::getValue).findFirst().orElse(null));

        settings = testSubject.loadWebserviceStats(Tenant.DEFAULT_ID);
        assertFalse(settings.isEmpty());
        assertEquals("0", settings.stream().filter(s -> "webservice.apicalls".equals(s.getName()))
                .map(GUIParameter::getValue).findFirst().orElse(null));
    }

    @Test
    public void testLoadEmailSettings() throws ServerException {
        GUIEmailSettings settings = testSubject.loadEmailSettings();
        assertEquals(Context.get().getConfig().get("default.smtp.host"), settings.getServer());
    }

    @Test
    public void testLoadSettings() throws ServerException {
        List<GUIParameter> settings = testSubject.loadSettings();
        assertEquals("value", settings.stream().filter(s -> "test".equals(s.getName())).map(GUIParameter::getValue)
                .findFirst().orElse(null));

        settings = testSubject.loadSettingsByNames(List.of("converter.*", "aspect.*", "ftp.enabled"));
        assertTrue(settings.stream().anyMatch(s -> "ftp.enabled".equals(s.getName())));
        assertTrue(settings.stream().anyMatch(s -> "customid.default".equals(s.getName())));
        assertTrue(settings.stream().anyMatch(s -> "doc-pdf".equals(s.getName())));
        assertFalse(settings.stream().anyMatch(s -> "test".equals(s.getName())));
    }

    @Test
    public void testLoadGUISettings() throws ServerException {
        List<GUIParameter> settings = testSubject.loadGUISettings();
        assertEquals("100", settings.stream().filter(s -> "default.upload.maxsize".equals(s.getName()))
                .map(GUIParameter::getValue).findFirst().orElse(null));
    }

    @Test
    public void testTestStore() throws ServerException {
        assertTrue(testSubject.testStore(1));
        assertFalse(testSubject.testStore(3));
    }

    @Test
    public void testProtocolSettings() throws ServerException {
        List<GUIParameter> settings = testSubject.loadProtocolSettings();
        assertEquals("true", settings.stream().filter(s -> "webservice.enabled".equals(s.getName()))
                .map(GUIParameter::getValue).findFirst().orElse(null));
    }

    @Test
    public void testSaveFirewallSettings() throws ServerException {
        List<GUIParameter> settings = testSubject.loadSettingsByNames(List.of("firewall.enabled"));
        boolean enabled = Boolean.parseBoolean(settings.stream().filter(s -> "firewall.enabled".equals(s.getName()))
                .map(GUIParameter::getValue).findFirst().orElse("false"));
        settings.forEach(s -> {
            if ("firewall.enabled".equals(s.getName()))
                s.setValue(Boolean.toString(!enabled));
        });
        testSubject.saveFirewallSettings(settings);

        settings = testSubject.loadSettingsByNames(List.of("firewall.enabled"));
        assertEquals(!enabled,
                Boolean.parseBoolean(settings.stream().filter(s -> "firewall.enabled".equals(s.getName()))
                        .map(GUIParameter::getValue).findFirst().orElse(Boolean.toString(enabled))));
    }

    @Test
    public void testSaveSettings() throws ServerException {
        List<GUIParameter> params = new ArrayList<>();
        for (int i = 0; i < 50; i++)
            params.add(new GUIParameter("param%d_name".formatted(i), "Value %d".formatted(i)));
        params.add(new GUIParameter("default.gui.welcome", "hello"));
        params.add(new GUIParameter("gui.tag.vocabulary", "abc"));

        String notThrownTest = null;
        try {
            testSubject.saveSettings(params);
            notThrownTest = "ok";
        } catch (Exception t) {
            log.error(t.getMessage(), t);
        }
        assertNotNull(notThrownTest);
    }

    @Test
    public void testSaveStoreSettings() throws ServerException {
        try {
            assertNull(Context.get().getConfig().getString("store.3.dir"));
            List<GUIParameter> params = new ArrayList<>();
            params.add(new GUIParameter("store.3.dir", "target/tmp/repository/docs2/"));
            params.add(new GUIParameter("store.3.type", "fs"));

            testSubject.saveStoreSettings(params);
            assertEquals("target/tmp/repository/docs2/", Context.get().getConfig().getString("store.3.dir"));

        } finally {
            testSubject.removeStore(3);
            assertNull(Context.get().getConfig().getString("store.3.dir"));
        }
    }

    @Test
    public void testSaveWSSettings() throws ServerException {
        GUIParameter wsSettings = new GUIParameter();
        wsSettings.setName("webservice.enabled");
        wsSettings.setValue("true");

        GUIParameter wdSettings = new GUIParameter();
        wsSettings.setName("webdav.enabled");
        wsSettings.setValue("true");

        String notThrownTest = null;
        try {
            testSubject.saveSettings(List.of(wsSettings, wdSettings));
            notThrownTest = "ok";
        } catch (Exception t) {
            log.error(t.getMessage(), t);
        }
        assertNotNull(notThrownTest);
    }

    @Test
    public void testTestEmail() throws ServerException {
        assertTrue(testSubject.testEmail("john@acme.com"));
    }

    @Test(expected = ServerException.class)
    public void testProxy() throws ServerException {
        testSubject.testProxy("localhost", 1234, null, null);
    }
}