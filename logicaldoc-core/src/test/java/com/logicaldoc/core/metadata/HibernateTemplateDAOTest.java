package com.logicaldoc.core.metadata;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.sql.SQLException;
import java.util.Collection;

import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTestCase;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.Tenant;

import junit.framework.Assert;

/**
 * Test case for <code>HibernateTemplateDAO</code>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 4.0
 */
public class HibernateTemplateDAOTest extends AbstractCoreTestCase {

	private TemplateDAO dao;

	@Before
	public void setUp() throws FileNotFoundException, IOException, SQLException {
		super.setUp();

		// Retrieve the instance under test from spring context. Make sure that
		// it is an HibernateDocumentDAO
		dao = (TemplateDAO) context.getBean("TemplateDAO");
	}

	@Test
	public void testDelete() throws PersistenceException {
		try {
			dao.delete(1);
		} catch (PersistenceException e) {
			Assert.assertTrue(true);
		}

		dao.delete(-1L);
		Template template = dao.findById(-1L);
		Assert.assertNull(template);
	}

	@Test
	public void testFindAll() throws PersistenceException {
		Collection<Template> templates = dao.findAll();
		Assert.assertNotNull(templates);
		Assert.assertEquals(4, templates.size());
	}
	
	@Test
	public void testFindById() throws PersistenceException {
		Template template = dao.findById(1);
		Assert.assertNotNull(template);
		dao.initialize(template);
		Assert.assertEquals(1, template.getId());
		Assert.assertEquals("test1", template.getName());
		Assert.assertTrue(template.getAttributes().containsKey("attr1"));

		// Try with unexisting template
		template = dao.findById(99);
		Assert.assertNull(template);

		template = dao.findById(-1);
		Assert.assertNotNull(template);
		dao.initialize(template);
		Assert.assertEquals(-1, template.getId());
		Assert.assertEquals("default", template.getName());
		Assert.assertTrue(template.getAttributes().containsKey("object"));
	}

	@Test
	public void testClone() throws PersistenceException {
		Template template = dao.findById(1);
		Assert.assertNotNull(template);
		dao.initialize(template);
		Assert.assertEquals(1, template.getId());
		Assert.assertEquals("test1", template.getName());
		Assert.assertTrue(template.getAttributes().containsKey("attr1"));
		
		Template clone = dao.clone(1, "test1-Clone");
		Assert.assertNotNull(clone);
		clone = dao.findById(clone.getId());
		dao.initialize(clone);
		Assert.assertNotSame(1, clone.getId());
		Assert.assertEquals("test1-Clone", clone.getName());
		Assert.assertTrue(clone.getAttributes().containsKey("attr1"));
	}
	
	@Test
	public void testFindByName() throws PersistenceException {
		Template template = dao.findByName("test1", Tenant.DEFAULT_ID);
		Assert.assertNotNull(template);
		dao.initialize(template);
		Assert.assertEquals(1, template.getId());
		Assert.assertEquals("test1", template.getName());

		template = dao.findByName("xxx", Tenant.DEFAULT_ID);
		Assert.assertNull(template);

		template = dao.findByName("test1", 99L);
		Assert.assertNull(template);
	}

	@Test
	public void testStore() throws PersistenceException {
		Template template = new Template();
		template.setName("test3");
		template.setValue("a1", "v1");
		template.setValue("a2", "v2");
		template.setValue("object", "value");

		dao.store(template);
		Assert.assertNotNull(template);
		template = dao.findById(template.getId());
		dao.initialize(template);
		Assert.assertEquals("test3", template.getName());
		Assert.assertTrue(template.getAttributes().containsKey("a1"));
		Assert.assertTrue(template.getAttributes().containsKey("a2"));
		Assert.assertTrue(template.getAttributes().containsKey("object"));
	}

	@Test
	public void testPermissions() throws PersistenceException {
		Template template = dao.findById(1L);
		Assert.assertNotNull(template);

		Assert.assertTrue(dao.isReadEnable(1L, 1L));
		Assert.assertTrue(dao.isWriteEnable(1L, 1L));

		Assert.assertTrue(dao.isReadEnable(1L, 3L));
		Assert.assertTrue(dao.isWriteEnable(1L, 3L));

		Assert.assertFalse(dao.isReadEnable(1L, 5L));
		Assert.assertFalse(dao.isWriteEnable(1L, 5L));

		Assert.assertFalse(dao.isReadEnable(1L, 99L));
		Assert.assertFalse(dao.isReadEnable(2L, 99L));

		Assert.assertFalse(dao.isReadEnable(1L, 4L));
		Assert.assertFalse(dao.isWriteEnable(1L, 4L));

		Assert.assertFalse(dao.isReadEnable(2L, 4L));
		Assert.assertFalse(dao.isWriteEnable(2L, 4L));

	}
}