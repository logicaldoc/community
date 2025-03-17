package com.logicaldoc.core.metadata;

import java.io.IOException;
import java.sql.SQLException;
import java.util.Collection;

import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTestCase;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.plugin.PluginException;

import junit.framework.Assert;

/**
 * Test case for {@link HibernateTemplateDAO}
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 4.0
 */
public class HibernateTemplateDAOTest extends AbstractCoreTestCase {

	// Instance under test
	private TemplateDAO testSubject;

	@Before
	public void setUp() throws IOException, SQLException, PluginException {
		super.setUp();

		// Retrieve the instance under test from spring context. Make sure that
		// it is an HibernateDocumentDAO
		testSubject = Context.get(TemplateDAO.class);
	}

	@Test
	public void testDelete() throws PersistenceException {
		try {
			testSubject.delete(1);
		} catch (PersistenceException e) {
			Assert.assertTrue(true);
		}

		testSubject.delete(-1L);
		Template template = testSubject.findById(-1L);
		Assert.assertNull(template);
	}

	@Test
	public void testFindAll() throws PersistenceException {
		Collection<Template> templates = testSubject.findAll();
		Assert.assertNotNull(templates);
		Assert.assertEquals(4, templates.size());
	}
	
	@Test
	public void testFindById() throws PersistenceException {
		Template template = testSubject.findById(1);
		Assert.assertNotNull(template);
		testSubject.initialize(template);
		Assert.assertEquals(1, template.getId());
		Assert.assertEquals("test1", template.getName());
		Assert.assertTrue(template.getAttributes().containsKey("attr1"));

		// Try with unexisting template
		template = testSubject.findById(99);
		Assert.assertNull(template);

		template = testSubject.findById(-1);
		Assert.assertNotNull(template);
		testSubject.initialize(template);
		Assert.assertEquals(-1, template.getId());
		Assert.assertEquals("default", template.getName());
		Assert.assertTrue(template.getAttributes().containsKey("object"));
	}

	@Test
	public void testClone() throws PersistenceException {
		Template template = testSubject.findById(1);
		Assert.assertNotNull(template);
		testSubject.initialize(template);
		Assert.assertEquals(1, template.getId());
		Assert.assertEquals("test1", template.getName());
		Assert.assertTrue(template.getAttributes().containsKey("attr1"));
		
		Template clone = testSubject.clone(1, "test1-Clone");
		Assert.assertNotNull(clone);
		clone = testSubject.findById(clone.getId());
		testSubject.initialize(clone);
		Assert.assertNotSame(1, clone.getId());
		Assert.assertEquals("test1-Clone", clone.getName());
		Assert.assertTrue(clone.getAttributes().containsKey("attr1"));
	}
	
	@Test
	public void testFindByName() throws PersistenceException {
		Template template = testSubject.findByName("test1", Tenant.DEFAULT_ID);
		Assert.assertNotNull(template);
		testSubject.initialize(template);
		Assert.assertEquals(1, template.getId());
		Assert.assertEquals("test1", template.getName());

		template = testSubject.findByName("xxx", Tenant.DEFAULT_ID);
		Assert.assertNull(template);

		template = testSubject.findByName("test1", 99L);
		Assert.assertNull(template);
	}

	@Test
	public void testStore() throws PersistenceException {
		Template template = new Template();
		template.setName("test3");
		template.setValue("a1", "v1");
		template.setValue("a2", "v2");
		template.setValue("object", "value");

		testSubject.store(template);
		Assert.assertNotNull(template);
		template = testSubject.findById(template.getId());
		Assert.assertNotNull(template);
		testSubject.initialize(template);
		Assert.assertEquals("test3", template.getName());
		Assert.assertTrue(template.getTemplateAttributes().containsKey("a1"));
		Assert.assertTrue(template.getTemplateAttributes().containsKey("a2"));
		Assert.assertTrue(template.getTemplateAttributes().containsKey("object"));
	}

	@Test
	public void testPermissions() throws PersistenceException {
		Template template = testSubject.findById(1L);
		Assert.assertNotNull(template);

		Assert.assertTrue(testSubject.isReadEnable(1L, 1L));
		Assert.assertTrue(testSubject.isWriteEnable(1L, 1L));

		Assert.assertTrue(testSubject.isReadEnable(1L, 3L));
		Assert.assertTrue(testSubject.isWriteEnable(1L, 3L));

		Assert.assertFalse(testSubject.isReadEnable(1L, 5L));
		Assert.assertFalse(testSubject.isWriteEnable(1L, 5L));

		Assert.assertFalse(testSubject.isReadEnable(1L, 99L));
		Assert.assertFalse(testSubject.isReadEnable(2L, 99L));

		Assert.assertFalse(testSubject.isReadEnable(1L, 4L));
		Assert.assertFalse(testSubject.isWriteEnable(1L, 4L));

		Assert.assertFalse(testSubject.isReadEnable(2L, 4L));
		Assert.assertFalse(testSubject.isWriteEnable(2L, 4L));

	}
}