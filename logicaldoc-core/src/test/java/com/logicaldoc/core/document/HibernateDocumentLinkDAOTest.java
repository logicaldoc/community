package com.logicaldoc.core.document;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.sql.SQLException;
import java.util.Collection;

import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTestCase;
import com.logicaldoc.core.PersistenceException;

import junit.framework.Assert;

/**
 * Test case for <code>HibernateDocumentLinkDAO</code>
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 4.0
 */
public class HibernateDocumentLinkDAOTest extends AbstractCoreTestCase {

	// Instance under test
	private DocumentLinkDAO dao;

	private DocumentDAO docDao;

	@Before
	public void setUp() throws FileNotFoundException, IOException, SQLException {
		super.setUp();
		// Retrieve the instance under test from spring context. Make sure that
		// it is an HibernateDocumentLinkDAO
		dao = (DocumentLinkDAO) context.getBean("DocumentLinkDAO");

		docDao = (DocumentDAO) context.getBean("DocumentDAO");
	}

	@Test
	public void testStore() throws PersistenceException {
		DocumentLink link = new DocumentLink();
		Document doc1 = docDao.findById(1);
		Document doc2 = docDao.findById(2);
		link.setDocument1(doc1);
		link.setDocument2(doc2);
		link.setType("zzz");
		dao.store(link);
		link = dao.findById(link.getId());
		Assert.assertEquals(1, link.getDocument1().getId());
		Assert.assertEquals(2, link.getDocument2().getId());
		Assert.assertEquals("zzz", link.getType());
	}

	@Test
	public void testDelete() throws PersistenceException {
		DocumentLink link = dao.findById(1);
		Assert.assertNotNull(link);
		dao.delete(1);
		link = dao.findById(1);
		Assert.assertNull(link);

		// Try with unexisting link
		dao.delete(99);

		link = dao.findById(1);
		Assert.assertNull(link);
		link = dao.findById(2);
		Assert.assertNotNull(link);
	}

	@Test
	public void testFindById() throws PersistenceException {
		DocumentLink link1 = dao.findById(1);
		Assert.assertNotNull(link1);
		Assert.assertEquals(1, link1.getDocument1().getId());
		Assert.assertEquals(2, link1.getDocument2().getId());
		Assert.assertEquals("test", link1.getType());

		DocumentLink link2 = dao.findById(2);
		Assert.assertNotNull(link2);
		Assert.assertEquals(2, link2.getDocument1().getId());
		Assert.assertEquals(1, link2.getDocument2().getId());

		link2 = dao.findById(99);
		Assert.assertNull(link2);
	}

	@Test
	public void testFindByDocId() throws PersistenceException {
		Collection<DocumentLink> links = dao.findByDocId(1, null);
		Assert.assertNotNull(links);
		Assert.assertEquals(4, links.size());

		Collection<DocumentLink> links2 = dao.findByDocId(2, null);
		Assert.assertNotNull(links2);
		Assert.assertEquals(4, links.size());

		Collection<DocumentLink> links3 = dao.findByDocId(99, null);
		Assert.assertNotNull(links3);
		Assert.assertEquals(0, links3.size());

		links = dao.findByDocId(1, "test");
		Assert.assertNotNull(links);
		Assert.assertEquals(1, links.size());

		links2 = dao.findByDocId(99, "pippo");
		Assert.assertNotNull(links2);
		Assert.assertEquals(0, links2.size());

		links2 = dao.findByDocId(1, "pippo");
		Assert.assertNotNull(links2);
		Assert.assertEquals(0, links2.size());
	}

	@Test
	public void testFindByDocIdsAndType() throws PersistenceException {
		DocumentLink link = dao.findByDocIdsAndType(1, 2, "test");
		Assert.assertNotNull(link);
		Assert.assertEquals(1, link.getId());

		link = dao.findByDocIdsAndType(2, 1, "xyz");
		Assert.assertNotNull(link);
		Assert.assertEquals(2, link.getId());

		link = dao.findByDocIdsAndType(1, 2, "xxx");
		Assert.assertNotNull(link);
		Assert.assertEquals(3, link.getId());

		link = dao.findByDocIdsAndType(2, 1, "");
		Assert.assertNotNull(link);
		Assert.assertEquals(4, link.getId());

		link = dao.findByDocIdsAndType(2, 1, null);
		Assert.assertNull(link);

		link = dao.findByDocIdsAndType(1, 2, "zzz");
		Assert.assertNull(link);

	}
}