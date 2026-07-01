package com.logicaldoc.dropbox;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import java.io.IOException;
import java.sql.SQLException;

import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.util.plugin.PluginException;
import com.logicaldoc.util.security.StringEncrypter.EncryptionException;

/**
 * Test case for {@link Dropbox}
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.3
 */
public class DropboxTest extends AbstractDropboxTestCase {

    private Dropbox testSubject;

    @Before
    @Override
    public void setUp() throws IOException, SQLException, PluginException {
        super.setUp();

        try {
            testSubject = new Dropbox(1L);
        } catch (PersistenceException | EncryptionException e) {
            throw new IOException(e);
        }
    }

    @Test
    public void testSaveSettings() throws PersistenceException, EncryptionException {
        assertNull(testSubject.getApiKey());
        testSubject.setApiKey("apikey");
        testSubject.setApiSecret("apisecret");
        testSubject.saveSettings();
        testSubject.loadSettings();
        assertEquals("apikey", testSubject.getApiKey());
    }
}