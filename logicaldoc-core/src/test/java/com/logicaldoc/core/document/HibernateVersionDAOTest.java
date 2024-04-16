package com.logicaldoc.core.document;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.sql.SQLException;
import java.util.List;

import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTestCase;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.user.User;
import com.logicaldoc.util.plugin.PluginException;

/**
 * Test case for <code>HibernateVersionDAO</code>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 4.5
 */
public class HibernateVersionDAOTest extends AbstractCoreTestCase {

	// Instance under test
	private VersionDAO dao;

	private DocumentDAO docDao;

	@Before
	public void setUp() throws FileNotFoundException, IOException, SQLException, PluginException {
		super.setUp();

		// Retrieve the instance under test from spring context. Make sure that
		// it is an HibernateVersionDAO
		dao = (VersionDAO) context.getBean("VersionDAO");
		docDao = (DocumentDAO) context.getBean("DocumentDAO");
	}

	@Test
	public void testFindByDocumentId() throws PersistenceException {
		List<Version> versions = dao.findByDocId(1);
		assertEquals(2, versions.size());
		assertTrue(versions.contains(dao.findByVersion(1, "0.1")));
		assertTrue(versions.contains(dao.findByVersion(1, "0.2")));

		versions = dao.findByDocId(2);
		assertEquals(0, versions.size());

		versions = dao.findByDocId(99);
		assertEquals(0, versions.size());
	}

	@Test
	public void testFindByVersion() throws PersistenceException {
		Version version = dao.findByVersion(1, "0.2");
		assertNotNull(version);
		assertEquals("0.2", version.getVersion());

		version = dao.findByVersion(1, "xxxx");
		assertNull(version);
	}

	@Test
	public void testFindByFileVersion() throws PersistenceException {
		Version version = dao.findByFileVersion(1, "0.1");
		assertNotNull(version);
		assertEquals("0.1", version.getFileVersion());

		version = dao.findByVersion(1, "30");
		assertNull(version);
	}

	@Test
	public void testStore() throws PersistenceException, InterruptedException {
		Document doc = docDao.findById(1);
		docDao.initialize(doc);
		assertEquals("1.0", doc.getVersion());
		User user = new User();
		user.setId(1);
		user.setUsername("admin");
		user.setName("xx");
		user.setFirstName("xx");
		Version version = Version.create(doc, user, "", DocumentEvent.STORED.toString(), true);
		dao.store(version);

		assertEquals("1.0", dao.findById(version.getId()).getVersion());

		version = Version.create(doc, user, "", DocumentEvent.CHANGED.toString(), true);
		dao.store(version);

		assertEquals("2.0", version.getVersion());
	}
}