package com.logicaldoc.core.document;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.sql.SQLException;
import java.util.Collection;

import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTestCase;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.util.plugin.PluginException;
import com.logicaldoc.util.spring.Context;

/**
 * Test case for {@link HibernateDocumentLinkDAO}
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 4.0
 */
public class HibernateDocumentLinkDAOTest extends AbstractCoreTestCase {

	// Instance under test
	private DocumentLinkDAO testSubject;

	private DocumentDAO docDao;

	@Before
	public void setUp() throws IOException, SQLException, PluginException {
		super.setUp();
		// Retrieve the instance under test from spring context. Make sure that
		// it is an HibernateDocumentLinkDAO
		testSubject = Context.get(DocumentLinkDAO.class);
		docDao = Context.get(DocumentDAO.class);
	}

	@Test
	public void testStore() throws PersistenceException {
		DocumentLink link = new DocumentLink();
		Document doc1 = docDao.findById(1);
		Document doc2 = docDao.findById(2);
		link.setDocument1(doc1);
		link.setDocument2(doc2);
		link.setType("zzz");
		testSubject.store(link);
		link = testSubject.findById(link.getId());
		assertEquals(1, link.getDocument1().getId());
		assertEquals(2, link.getDocument2().getId());
		assertEquals("zzz", link.getType());
	}

	@Test
	public void testDelete() throws PersistenceException {
		DocumentLink link = testSubject.findById(1);
		assertNotNull(link);
		testSubject.delete(1);
		link = testSubject.findById(1);
		assertNull(link);

		// Try with unexisting link
		testSubject.delete(99);

		link = testSubject.findById(1);
		assertNull(link);
		link = testSubject.findById(2);
		assertNotNull(link);
	}

	@Test
	public void testFindById() throws PersistenceException {
		DocumentLink link1 = testSubject.findById(1);
		assertNotNull(link1);
		assertEquals(1, link1.getDocument1().getId());
		assertEquals(3, link1.getDocument2().getId());
		assertEquals("test", link1.getType());

		DocumentLink link2 = testSubject.findById(2);
		assertNotNull(link2);
		assertEquals(3, link2.getDocument1().getId());
		assertEquals(1, link2.getDocument2().getId());

		link2 = testSubject.findById(99);
		assertNull(link2);
	}

	@Test
	public void testFindByDocId() throws PersistenceException {
		Collection<DocumentLink> links = testSubject.findByDocId(1, null);
		assertNotNull(links);
		assertEquals(4, links.size());

		links = testSubject.findByDocId(99, null);
		assertNotNull(links);
		assertTrue(links.isEmpty());

		links = testSubject.findByDocId(1, "test");
		assertNotNull(links);
		assertEquals(1, links.size());

		links = testSubject.findByDocId(99, "pippo");
		assertNotNull(links);
		assertTrue(links.isEmpty());

		links = testSubject.findByDocId(1, "pippo");
		assertNotNull(links);
		assertTrue(links.isEmpty());
	}

	@Test
	public void testFindByDocIdsAndType() throws PersistenceException {
		DocumentLink link = testSubject.findByDocIdsAndType(1, 3, "test");
		assertNotNull(link);
		assertEquals(1, link.getId());

		link = testSubject.findByDocIdsAndType(3L, 1L, "xyz");
		assertNotNull(link);
		assertEquals(2, link.getId());

		link = testSubject.findByDocIdsAndType(1L, 3L, "xxx");
		assertNotNull(link);
		assertEquals(3, link.getId());

		link = testSubject.findByDocIdsAndType(3, 1, "");
		assertNotNull(link);
		assertEquals(4, link.getId());

		link = testSubject.findByDocIdsAndType(3, 1, null);
		assertNull(link);

		link = testSubject.findByDocIdsAndType(1, 2, "zzz");
		assertNull(link);

	}
}