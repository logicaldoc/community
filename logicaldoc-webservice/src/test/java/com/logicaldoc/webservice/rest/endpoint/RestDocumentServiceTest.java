package com.logicaldoc.webservice.rest.endpoint;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.io.FileInputStream;
import java.io.IOException;
import java.sql.SQLException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;

import org.apache.cxf.jaxrs.ext.multipart.InputStreamDataSource;
import org.junit.Test;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.document.Version;
import com.logicaldoc.core.document.VersionDAO;
import com.logicaldoc.core.security.authentication.AuthenticationException;
import com.logicaldoc.core.security.authorization.PermissionException;
import com.logicaldoc.util.plugin.PluginException;
import com.logicaldoc.util.time.DateUtil;
import com.logicaldoc.webservice.AbstractWebserviceTestCase;
import com.logicaldoc.webservice.WebserviceException;
import com.logicaldoc.webservice.model.WSDocument;

import jakarta.activation.DataHandler;
import jakarta.ws.rs.core.Response;

/**
 * Test case for <code>SoapDocumentService</code>
 * 
 * @author Alessandro Gasparini
 * @since 9.1.1
 */
public class RestDocumentServiceTest extends AbstractWebserviceTestCase {

    // Instance under test
    private RestDocumentService testSubject;

    @Override
    public void setUp() throws IOException, SQLException, PluginException {
        super.setUp();

        // Make sure that this is a DocumentServiceImpl instance
        testSubject = new RestDocumentService();
        testSubject.setValidateSession(false);
    }

    @Test
    public void testGetThumbnail() throws AuthenticationException, PersistenceException, PermissionException,
            WebserviceException, IOException {

        // First upload the resource
        FileInputStream fis = new FileInputStream("src/test/resources/fortnightemail-(1)-(1).png");
        DataHandler dh = new DataHandler(new InputStreamDataSource(fis, "image/png"));
        testSubject.uploadResource("", 1, "1.0", "mobile.png", dh);

        // Then check that the server return the resource
        Response res = testSubject.getThumbnail("mobile", "menu.adminxxx/text/menu.admin103/pippo");
        assertNotNull(res);

        assertEquals("image/png", res.getHeaderString("Content-Type"));
        assertNotNull(res.getHeaderString("Expires"));
    }

    @Test
    public void testGetVersionsDates() throws Exception {
        VersionDAO versionDao = VersionDAO.get();
        Version version = versionDao.findByVersion(1L, "testVer02");
        assertNotNull(version);

        SimpleDateFormat format = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss Z");

        Date date = format.parse("2026-02-18 17:04:26 +0100");
        Date creation = format.parse("2025-10-02 09:57:37 +0200");

        version.setDate(date);
        version.setCreation(creation);
        versionDao.store(version);

        Version saved = versionDao.findByVersion(1L, "testVer02");
        assertNotNull(saved);
        String expectedLastModified = DateUtil.format(saved.getLastModified());

        // Calls the REST method, which delegates to SoapDocumentService.
        List<WSDocument> versions = testSubject.getVersions(1L);

        WSDocument result = versions.stream().filter(v -> "testVer02".equals(v.getVersion())).findFirst().orElseThrow();

        assertEquals(date, format.parse(result.getDate()));
        assertEquals(creation, format.parse(result.getCreation()));
        assertEquals(expectedLastModified, result.getLastModified());
    }
}