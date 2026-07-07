package com.logicaldoc.util.config;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.IOException;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.util.io.FileUtil;

/**
 * Test case for {@link WebConfigurator}
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 4.5
 */
public class WebConfiguratorTest {

    private File webXml = new File("target/web.xml");

    private WebConfigurator testSubject;

    @Before
    public void setUp() throws IOException {
        FileUtil.delete(webXml);
        FileUtil.copyResource("web.xml", webXml);
        testSubject = new WebConfigurator(webXml.getPath());
    }

    @After
    public void tearDown() {
        FileUtil.delete(webXml);
    }

    @Test
    public void testSetDisplayName() throws IOException {
        assertFalse(FileUtil.readFile(webXml).contains("Pippo"));
        testSubject.setDisplayName("Pippo");
        testSubject.setDescription("My Description");
        assertTrue(FileUtil.readFile(webXml).contains("Pippo"));
    }

    @Test
    public void testAddListener() throws IOException {
        String clazz = "test.Listener";
        String paramName = "initval";

        assertFalse(FileUtil.readFile(webXml).contains(clazz));
        testSubject.addListener(clazz);
        assertTrue(FileUtil.readFile(webXml).contains(clazz));

        assertFalse(FileUtil.readFile(webXml).contains(paramName));
        assertFalse(FileUtil.readFile(webXml).contains("<param-value>pippo</param-value>"));
        testSubject.addListenerInitParam(clazz, paramName, "pippo", WebConfigurator.ParamInsertMode.APPEND);
        assertTrue(FileUtil.readFile(webXml).contains(paramName));
        assertTrue(FileUtil.readFile(webXml).contains("<param-value>pippo</param-value>"));

        testSubject.addListenerInitParam(clazz, paramName, "paperino", WebConfigurator.ParamInsertMode.APPEND);
        assertTrue(FileUtil.readFile(webXml).contains("<param-name>initval</param-name>"));
        assertTrue(FileUtil.readFile(webXml).contains("<param-value>pippo,paperino</param-value>"));

        testSubject.addListenerInitParam(clazz, paramName, "archimede", WebConfigurator.ParamInsertMode.OVERWRITE);

        assertTrue(FileUtil.readFile(webXml).contains(paramName));
        assertTrue(FileUtil.readFile(webXml).contains("<param-value>archimede</param-value>"));
        assertFalse(FileUtil.readFile(webXml).contains("<param-value>pippo,paperino</param-value>"));
    }

    @Test
    public void testAddServlet() throws IOException {
        String clazz = "test.Servlet";
        String name = "tstservlet";

        assertFalse(FileUtil.readFile(webXml).contains(clazz));
        testSubject.addServlet(name, clazz);
        assertTrue(FileUtil.readFile(webXml).contains(clazz));

        assertFalse(FileUtil.readFile(webXml).contains("initval"));
        assertFalse(FileUtil.readFile(webXml).contains("<param-value>pippo</param-value>"));
        testSubject.addServletInitParam(name, "initval", "pippo", "pippo description");
        assertTrue(FileUtil.readFile(webXml).contains("initval"));
        assertTrue(FileUtil.readFile(webXml).contains("<param-value>pippo</param-value>"));

        testSubject.addServletInitParam(name, "initval", "paperino", "description of init parameter",
                WebConfigurator.ParamInsertMode.APPEND);
        assertTrue(FileUtil.readFile(webXml).contains("<param-name>initval</param-name>"));
        assertTrue(FileUtil.readFile(webXml).contains("<param-value>pippo,paperino</param-value>"));

        testSubject.addServletInitParam(name, "initval", "archimede", "description of init parameter",
                WebConfigurator.ParamInsertMode.OVERWRITE);

        assertTrue(FileUtil.readFile(webXml).contains("initval"));
        assertTrue(FileUtil.readFile(webXml).contains("<param-value>archimede</param-value>"));
        assertFalse(FileUtil.readFile(webXml).contains("<param-value>pippo,paperino</param-value>"));

        assertFalse(FileUtil.readFile(webXml).contains("<url-pattern>/test</url-pattern>"));
        testSubject.addServletMapping(name, "/test");
        assertTrue(FileUtil.readFile(webXml).contains("<url-pattern>/test</url-pattern>"));
    }

    @Test
    public void testAddContextParam() throws IOException {
        assertFalse(FileUtil.readFile(webXml).contains("pippo"));

        String name = "test";
        testSubject.addContextParam(name, "pippo", "a test context parameter", WebConfigurator.ParamInsertMode.APPEND);
        assertTrue(FileUtil.readFile(webXml).contains("<param-value>pippo</param-value>"));
        assertFalse(FileUtil.readFile(webXml).contains("<param-value>paperino</param-value>"));

        testSubject.addContextParam(name, "archimede", "a test context parameter",
                WebConfigurator.ParamInsertMode.STOP);
        assertTrue(FileUtil.readFile(webXml).contains("<param-value>pippo</param-value>"));
        assertFalse(FileUtil.readFile(webXml).contains("<param-value>archimede</param-value>"));

        testSubject.addContextParam(name, "paperino", "a test context parame",
                WebConfigurator.ParamInsertMode.OVERWRITE);
        assertFalse(FileUtil.readFile(webXml).contains("<param-value>pippo</param-value>"));
        assertTrue(FileUtil.readFile(webXml).contains("<param-value>paperino</param-value>"));
        assertFalse(FileUtil.readFile(webXml).contains("a test context parameter"));

        testSubject.addContextParam(name, "pluto", "another test context parameter",
                WebConfigurator.ParamInsertMode.APPEND);
        assertTrue(FileUtil.readFile(webXml).contains("paperino"));
        assertTrue(FileUtil.readFile(webXml).contains("pluto"));
        assertTrue(FileUtil.readFile(webXml).contains(name));
    }

    @Test
    public void testAddFilter() throws IOException {
        String name = "tstfilter";
        assertFalse(FileUtil.readFile(webXml).contains(name));
        testSubject.addFilter(name, "test.Filter");
        assertTrue(FileUtil.readFile(webXml).contains(name));
        assertTrue(FileUtil.readFile(webXml).contains("test.Filter"));

        assertFalse(FileUtil.readFile(webXml).contains("initval"));
        assertFalse(FileUtil.readFile(webXml).contains("pippo"));
        testSubject.addFilterInitParam(name, "initval", "pippo");
        assertTrue(FileUtil.readFile(webXml).contains("initval"));
        assertTrue(FileUtil.readFile(webXml).contains("pippo"));

        testSubject.addFilterInitParam(name, "initval", "paperino", "description of init parameter",
                WebConfigurator.ParamInsertMode.APPEND);
        assertTrue(FileUtil.readFile(webXml).contains("<param-name>initval</param-name>"));
        assertTrue(FileUtil.readFile(webXml).contains("<param-value>pippo,paperino</param-value>"));

        testSubject.addFilterInitParam(name, "initval", "archimede", "description of init parameter",
                WebConfigurator.ParamInsertMode.OVERWRITE);
        assertTrue(FileUtil.readFile(webXml).contains("initval"));
        assertFalse(FileUtil.readFile(webXml).contains("pippo"));
        assertFalse(FileUtil.readFile(webXml).contains("paperino"));
        assertTrue(FileUtil.readFile(webXml).contains("archimede"));

        assertFalse(FileUtil.readFile(webXml).contains("<url-pattern>/test</url-pattern>"));
        testSubject.addFilterMapping(name, "/test");
        assertTrue(FileUtil.readFile(webXml).contains("<url-pattern>/test</url-pattern>"));
    }

    @Test
    public void testSetTransportGuarantee() {
        assertTrue(testSubject.setTransportGuarantee("CONFIDENCIAL"));
        assertFalse(testSubject.setTransportGuarantee("CONFIDENCIAL"));
    }
}