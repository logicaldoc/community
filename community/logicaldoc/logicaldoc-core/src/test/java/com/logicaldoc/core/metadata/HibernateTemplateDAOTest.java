package com.logicaldoc.core.metadata;

import java.util.Collection;

import junit.framework.Assert;

import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTCase;
import com.logicaldoc.core.security.Tenant;

/**
 * Test case for <code>HibernateTemplateDAO</code>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 4.0
 */
public class HibernateTemplateDAOTest extends AbstractCoreTCase {

	private TemplateDAO dao;

	private AttributeSetDAO setDao;

	@Before
	public void setUp() throws Exception {
		super.setUp();

		// Retrieve the instance under test from spring context. Make sure that
		// it is an HibernateDocumentDAO
		dao = (TemplateDAO) context.getBean("TemplateDAO");

		setDao = (AttributeSetDAO) context.getBean("AttributeSetDAO");
	}

	@Test
	public void testDelete() {
		Assert.assertTrue(dao.delete(1));
		Template template = dao.findById(1);
		Assert.assertNull(template);
	}

	@Test
	public void testFindAll() {
		Collection<Template> templates = dao.findAll();
		Assert.assertNotNull(templates);
		Assert.assertEquals(4, templates.size());
	}

	@Test
	public void testFindById() {
		Template template = dao.findById(1);
		Assert.assertNotNull(template);
		Assert.assertEquals(1, template.getId());
		Assert.assertEquals("test1", template.getName());
		Assert.assertTrue(template.getAttributes().containsKey("attr1"));

		// Try with unexisting template
		template = dao.findById(99);
		Assert.assertNull(template);

		template = dao.findById(-1);
		Assert.assertNotNull(template);
		Assert.assertEquals(-1, template.getId());
		Assert.assertEquals("default", template.getName());
		Assert.assertTrue(template.getAttributes().containsKey("object"));
	}

	@Test
	public void testFindByName() {
		Template template = dao.findByName("test1", Tenant.DEFAULT_ID);
		Assert.assertNotNull(template);
		Assert.assertEquals(1, template.getId());
		Assert.assertEquals("test1", template.getName());

		template = dao.findByName("xxx", Tenant.DEFAULT_ID);
		Assert.assertNull(template);

		template = dao.findByName("test1", 99L);
		Assert.assertNull(template);
	}

	@Test
	public void testStore() {
		Template template = new Template();
		template.setName("test3");
		template.setValue("a1", "v1");
		template.setValue("a2", "v2");
		template.setValue("object", "value");
		
		Assert.assertTrue(dao.store(template));
		template = dao.findById(template.getId());
		Assert.assertEquals("test3", template.getName());
		Assert.assertTrue(template.getAttributes().containsKey("a1"));
		Assert.assertTrue(template.getAttributes().containsKey("a2"));
		Assert.assertTrue(template.getAttributes().containsKey("object"));
	}
}
