package com.logicaldoc.web.service;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

import java.io.IOException;
import java.sql.SQLException;

import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.communication.MessageTemplate;
import com.logicaldoc.core.communication.MessageTemplateDAO;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.util.plugin.PluginException;
import com.logicaldoc.util.spring.Context;
import com.logicaldoc.web.AbstractWPTestCase;

public class MessageServiceImplTest extends AbstractWPTestCase {

	// Instance under test
	private MessageServiceImpl service = new MessageServiceImpl();

	private MessageTemplateDAO templateDao;

	@Before
	public void setUp() throws IOException, SQLException, PluginException {
		super.setUp();

		templateDao = Context.get(MessageTemplateDAO.class);
	}

	@Test
	public void testDelete() throws ServerException, PersistenceException {
		service.deleteTemplates("psw.rec1");
		MessageTemplate template = templateDao.findById(2L);
		assertNotNull(template);

		template = templateDao.findById(500L);
		assertNotNull(template);

		service.deleteTemplates("test");
		template = templateDao.findById(500L);
		assertNull(template);
	}
}
