package com.logicaldoc.core.document.dao;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.sql.SQLException;
import java.util.List;

import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTestCase;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.DocumentEvent;
import com.logicaldoc.core.document.Version;
import com.logicaldoc.core.security.User;

import junit.framework.Assert;

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
	public void setUp() throws FileNotFoundException, IOException, SQLException {
		super.setUp();

		// Retrieve the instance under test from spring context. Make sure that
		// it is an HibernateVersionDAO
		dao = (VersionDAO) context.getBean("VersionDAO");
		docDao = (DocumentDAO) context.getBean("DocumentDAO");
	}

	@Test
	public void testFindByDocumentId() {
		List<Version> versions = dao.findByDocId(1);
		Assert.assertEquals(2, versions.size());
		Assert.assertTrue(versions.contains(dao.findByVersion(1, "testVer01")));
		Assert.assertTrue(versions.contains(dao.findByVersion(1, "testVer02")));

		versions = dao.findByDocId(2);
		Assert.assertEquals(0, versions.size());

		versions = dao.findByDocId(99);
		Assert.assertEquals(0, versions.size());
	}

	@Test
	public void testFindByVersion() {
		Version version = dao.findByVersion(1, "testVer02");
		Assert.assertNotNull(version);
		Assert.assertEquals("testVer02", version.getVersion());

		version = dao.findByVersion(1, "xxxx");
		Assert.assertNull(version);
	}

	@Test
	public void testFindByFileVersion() {
		Version version = dao.findByFileVersion(1, "1.0");
		Assert.assertNotNull(version);
		Assert.assertEquals("1.0", version.getFileVersion());

		version = dao.findByVersion(1, "30");
		Assert.assertNull(version);
	}

	@Test
	public void testStore() throws PersistenceException {
		Version version = new Version();
		version.setDeleted(0);
		version.setComment("pippo");
		version.setUserId(1);
		version.setUsername("matteo");
		version.setDocId(1);
		dao.store(version);
		Assert.assertNotNull(dao.findById(2));
		Assert.assertNotNull(dao.findById(1));

		Document doc = docDao.findById(1);
		docDao.initialize(doc);
		User user = new User();
		user.setId(1);
		user.setUsername("admin");
		user.setName("xx");
		user.setFirstName("xx");
		version = Version.create(doc, user, "", DocumentEvent.STORED.toString(), true);
		dao.store(version);
	}
}