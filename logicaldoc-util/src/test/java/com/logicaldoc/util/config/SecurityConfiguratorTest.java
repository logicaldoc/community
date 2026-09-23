package com.logicaldoc.util.config;

import static org.junit.Assert.assertEquals;

import java.io.File;
import java.io.IOException;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.util.io.FileUtil;

/**
 * Test case for <code>SecurityConfigurator</code>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.7.3
 */
public class SecurityConfiguratorTest {

    private SecurityConfigurator testSubject;

    @Before
    public void setUp() throws IOException {
        File contextSecurityXml = new File("target/context-security.xml");
        FileUtil.copyResource("context-security.xml", contextSecurityXml);
        testSubject = new SecurityConfigurator(contextSecurityXml.getPath());
    }

    @Test
    public void testGetContentSecurityPolicy() {
        String policies = testSubject.getContentSecurityPolicy();
        Assert.assertNotNull(policies);
        Assert.assertTrue(policies.startsWith("default-src 'self' 'unsafe-inline' 'unsafe-eval'; script-src 'self'"));
    }

    @Test
    public void testSetInterceptUrl() {
        testSubject.setInterceptUrlBefore(null, "/download/**", "/data/wfattributeoptions.xml", "permitAll()");
        assertEquals("permitAll()", testSubject.getAccess(null, "/data/wfattributeoptions.xml"));

        assertEquals("isAuthenticated()", testSubject.getAccess(null, "/mobile.jsp"));
        testSubject.setInterceptUrlAfter(null, "/mobile.jsp", "/mobile.jsp", "permitAll()");
        assertEquals("permitAll()", testSubject.getAccess(null, "/mobile.jsp"));
    }
}