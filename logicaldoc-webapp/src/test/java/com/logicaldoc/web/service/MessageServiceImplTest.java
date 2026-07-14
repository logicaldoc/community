package com.logicaldoc.web.service;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.IOException;
import java.sql.SQLException;
import java.util.List;

import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.communication.MessageTemplate;
import com.logicaldoc.core.communication.MessageTemplateDAO;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUIMessageTemplate;
import com.logicaldoc.util.plugin.PluginException;
import com.logicaldoc.web.AbstractWPTestCase;

public class MessageServiceImplTest extends AbstractWPTestCase {

    // Instance under test
    private MessageServiceImpl testSubject = new MessageServiceImpl();

    private MessageTemplateDAO templateDao;

    @Before
    @Override
    public void setUp() throws IOException, SQLException, PluginException {
        super.setUp();

        templateDao = MessageTemplateDAO.get();
    }

    @Test
    public void testDelete() throws ServerException, PersistenceException {
        testSubject.deleteTemplates("psw.rec1");
        MessageTemplate template = templateDao.findById(2L);
        assertNotNull(template);

        template = templateDao.findById(500L);
        assertNotNull(template);

        testSubject.deleteTemplates("test");
        template = templateDao.findById(500L);
        assertNull(template);
    }

    @Test
    public void testLoadTemplates() throws ServerException, PersistenceException {
        List<GUIMessageTemplate> templates = testSubject.loadTemplates("en", "SYSTEM");
        assertEquals(6, templates.size());

        templates = testSubject.loadTemplates("en", "USER");
        assertEquals(2, templates.size());

        templates = testSubject.loadTemplates("en", "WHATSAPP");
        assertTrue(templates.isEmpty());

        
        templates = testSubject.loadTemplates("en", null);
        assertEquals(8, templates.size());
        
        try {
            templates = testSubject.loadTemplates("en", "UNEXISTING");
            fail("No exception in case of unexisting type?");
        } catch (ServerException ex) {
            // all ok
        }
    }
}
