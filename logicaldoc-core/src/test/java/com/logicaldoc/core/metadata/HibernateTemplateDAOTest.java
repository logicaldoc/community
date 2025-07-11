package com.logicaldoc.core.metadata;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.sql.SQLException;
import java.util.Collection;
import java.util.Set;

import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTestCase;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.Permission;
import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.util.plugin.PluginException;
import com.logicaldoc.util.spring.Context;

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
			assertTrue(true);
		}

		testSubject.delete(-1L);
		Template template = testSubject.findById(-1L);
		assertNull(template);
	}

	@Test
	public void testFindAll() throws PersistenceException {
		Collection<Template> templates = testSubject.findAll();
		assertNotNull(templates);
		assertEquals(4, templates.size());
	}
	
	@Test
	public void testFindById() throws PersistenceException {
		Template template = testSubject.findById(1);
		assertNotNull(template);
		testSubject.initialize(template);
		assertEquals(1, template.getId());
		assertEquals("test1", template.getName());
		assertTrue(template.getAttributes().containsKey("attr1"));

		// Try with unexisting template
		template = testSubject.findById(99);
		assertNull(template);

		template = testSubject.findById(-1);
		assertNotNull(template);
		testSubject.initialize(template);
		assertEquals(-1, template.getId());
		assertEquals("default", template.getName());
		assertTrue(template.getAttributes().containsKey("object"));
	}

	@Test
	public void testClone() throws PersistenceException {
		Template template = testSubject.findById(1);
		assertNotNull(template);
		testSubject.initialize(template);
		assertEquals(1, template.getId());
		assertEquals("test1", template.getName());
		assertTrue(template.getAttributes().containsKey("attr1"));
		
		Template clone = testSubject.clone(1, "test1-Clone");
		assertNotNull(clone);
		clone = testSubject.findById(clone.getId());
		testSubject.initialize(clone);
		assertNotSame(1, clone.getId());
		assertEquals("test1-Clone", clone.getName());
		assertTrue(clone.getAttributes().containsKey("attr1"));
	}
	
	@Test
	public void testFindByName() throws PersistenceException {
		Template template = testSubject.findByName("test1", Tenant.DEFAULT_ID);
		assertNotNull(template);
		testSubject.initialize(template);
		assertEquals(1, template.getId());
		assertEquals("test1", template.getName());

		template = testSubject.findByName("xxx", Tenant.DEFAULT_ID);
		assertNull(template);

		template = testSubject.findByName("test1", 99L);
		assertNull(template);
	}

	@Test
	public void testStore() throws PersistenceException {
		Template template = new Template();
		template.setName("test3");
		template.setValue("a1", "v1");
		template.setValue("a2", "v2");
		template.setValue("object", "value");

		testSubject.store(template);
		assertNotNull(template);
		template = testSubject.findById(template.getId());
		assertNotNull(template);
		testSubject.initialize(template);
		assertEquals("test3", template.getName());
		assertTrue(template.getTemplateAttributes().containsKey("a1"));
		assertTrue(template.getTemplateAttributes().containsKey("a2"));
		assertTrue(template.getTemplateAttributes().containsKey("object"));
	}

	@Test
	public void testPermissions() throws PersistenceException {
		Template template = testSubject.findById(1L);
		assertNotNull(template);

		assertTrue(testSubject.isReadEnable(1L, 1L));
		assertTrue(testSubject.isWriteEnable(1L, 1L));

		assertTrue(testSubject.isReadEnable(1L, 3L));
		assertTrue(testSubject.isWriteEnable(1L, 3L));

		assertFalse(testSubject.isReadEnable(1L, 5L));
		assertFalse(testSubject.isWriteEnable(1L, 5L));

		assertFalse(testSubject.isReadEnable(1L, 99L));
		assertFalse(testSubject.isReadEnable(2L, 99L));

		assertFalse(testSubject.isReadEnable(1L, 4L));
		assertFalse(testSubject.isWriteEnable(1L, 4L));

		assertFalse(testSubject.isReadEnable(2L, 4L));
		assertFalse(testSubject.isWriteEnable(2L, 4L));
		
		Set<Permission> permissions = testSubject.getAllowedPermissions(1L, 1L);
		assertEquals(Permission.all().size(), permissions.size());
	}
}