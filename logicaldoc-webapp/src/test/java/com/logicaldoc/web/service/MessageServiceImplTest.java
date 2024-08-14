package com.logicaldoc.web.service;

import java.io.IOException;
import java.sql.SQLException;

import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.communication.MessageTemplate;
import com.logicaldoc.core.communication.MessageTemplateDAO;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.util.plugin.PluginException;
import com.logicaldoc.web.AbstractWebappTestCase;

import junit.framework.Assert;

public class MessageServiceImplTest extends AbstractWebappTestCase {

	// Instance under test
	private MessageServiceImpl service = new MessageServiceImpl();

	private MessageTemplateDAO templateDao;

	@Before
	public void setUp() throws IOException, SQLException, PluginException {
		super.setUp();

		templateDao = (MessageTemplateDAO) context.getBean("MessageTemplateDAO");
	}

	@Test
	public void testDelete() throws ServerException, PersistenceException {
		service.deleteTemplates("psw.rec1");
		MessageTemplate template = templateDao.findById(2L);
		Assert.assertNotNull(template);

		template = templateDao.findById(500L);
		Assert.assertNotNull(template);

		service.deleteTemplates("test");
		template = templateDao.findById(500L);
		Assert.assertNull(template);
	}
}
