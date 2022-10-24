package com.logicaldoc.web.service;

import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.metadata.Template;
import com.logicaldoc.core.metadata.TemplateDAO;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUIAttribute;
import com.logicaldoc.gui.common.client.beans.GUITemplate;
import com.logicaldoc.web.AbstractWebappTCase;

import junit.framework.Assert;

public class TemplateServiceImplTest extends AbstractWebappTCase {

	// Instance under test
	private TemplateServiceImpl service = new TemplateServiceImpl();

	private TemplateDAO templateDao;

	@Before
	public void setUp() throws Exception {
		super.setUp();

		templateDao = (TemplateDAO) context.getBean("TemplateDAO");
	}

	@Test
	public void testDelete() throws ServerException, PersistenceException {
		service.delete(1);
		Template template = templateDao.findById(1);
		Assert.assertNull(template);
	}

	@Test
	public void testSave() throws ServerException {
		GUITemplate template = service.getTemplate(5);
		Assert.assertNotNull(template);
		Assert.assertEquals("test1", template.getName());
		Assert.assertEquals("test1_desc", template.getDescription());

		template.setName("pippo");
		template.setDescription("paperino");

		template = service.save(template);

		template = service.getTemplate(5);
		Assert.assertNotNull(template);
		Assert.assertEquals("pippo", template.getName());
		Assert.assertEquals("paperino", template.getDescription());
		Assert.assertEquals(1, template.getAttributes().length);
		Assert.assertTrue(template.getAttributes()[0].getName().equals("attr1"));
		Assert.assertTrue(template.getAttributes()[0].getStringValue().equals("val1"));
	}

	@Test
	public void testGetTemplate() throws ServerException {
		GUITemplate template = service.getTemplate(6);
		Assert.assertNotNull(template);
		Assert.assertEquals("test2", template.getName());
		Assert.assertEquals("test2_desc", template.getDescription());

		template = service.getTemplate(8);
		Assert.assertNull(template);
	}

	@Test
	public void testGetAttributes() throws ServerException, PersistenceException {
		Template template = new Template();
		template.setName("test3");
		template.setValue("attr1", "v1");
		template.setValue("a2", 23L);
		templateDao.store(template);

		GUITemplate guiTemplate = service.getTemplate(template.getId());
		Assert.assertNotNull(guiTemplate);
		Assert.assertEquals("v1", guiTemplate.getAttribute("attr1").getValue());

		GUIAttribute[] extAttr = service.getAttributes(template.getId(), null);
		for (GUIAttribute at : extAttr) {
			if ("attr1".equals(at.getName())) {
				Assert.assertEquals(GUIAttribute.TYPE_STRING, at.getType());
				Assert.assertEquals("v1", at.getValue());
			}
			if ("a2".equals(at.getName())) {
				Assert.assertEquals(GUIAttribute.TYPE_INT, at.getType());
				Assert.assertEquals((Long) 23L, at.getIntValue());
			}
		}
	}
}
