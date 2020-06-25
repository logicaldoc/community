package com.logicaldoc.web.service;

import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.communication.MessageTemplate;
import com.logicaldoc.core.communication.MessageTemplateDAO;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.web.AbstractWebappTCase;

import junit.framework.Assert;

public class MessageServiceImplTest extends AbstractWebappTCase {

	// Instance under test
	private MessageServiceImpl service = new MessageServiceImpl();

	private MessageTemplateDAO templateDao;

	@Before
	public void setUp() throws Exception {
		super.setUp();

		templateDao = (MessageTemplateDAO) context.getBean("MessageTemplateDAO");
	}

	@Test
	public void testDelete() throws ServerException {
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
