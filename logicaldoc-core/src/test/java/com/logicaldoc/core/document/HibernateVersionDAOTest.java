package com.logicaldoc.core.document;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.io.InputStream;
import java.sql.SQLException;
import java.util.List;

import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTestCase;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.user.User;
import com.logicaldoc.core.store.Store;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.plugin.PluginException;

/**
 * Test case for <code>HibernateVersionDAO</code>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 4.5
 */
public class HibernateVersionDAOTest extends AbstractCoreTestCase {

	// Instance under test
	private VersionDAO testSubject;

	private DocumentDAO docDao;

	@Before
	public void setUp() throws IOException, SQLException, PluginException {
		super.setUp();

		// Retrieve the instance under test from spring context. Make sure that
		// it is an HibernateVersionDAO
		testSubject = (VersionDAO) context.getBean("VersionDAO");
		docDao = (DocumentDAO) context.getBean("DocumentDAO");
	}

	@Test
	public void testFindByDocumentId() throws PersistenceException {
		List<Version> versions = testSubject.findByDocId(1);
		assertEquals(2, versions.size());
		assertTrue(versions.contains(testSubject.findByVersion(1, "0.1")));
		assertTrue(versions.contains(testSubject.findByVersion(1, "0.2")));

		versions = testSubject.findByDocId(2);
		assertEquals(0, versions.size());

		versions = testSubject.findByDocId(99);
		assertEquals(0, versions.size());
	}

	@Test
	public void testFindByVersion() throws PersistenceException {
		Version version = testSubject.findByVersion(1, "0.2");
		assertNotNull(version);
		assertEquals("0.2", version.getVersion());

		version = testSubject.findByVersion(1, "xxxx");
		assertNull(version);
	}

	@Test
	public void testFindByFileVersion() throws PersistenceException {
		Version version = testSubject.findByFileVersion(1, "0.1");
		assertNotNull(version);
		assertEquals("0.1", version.getFileVersion());

		version = testSubject.findByVersion(1, "30");
		assertNull(version);
	}

	@Test
	public void testStore() throws PersistenceException, IOException {
		Document doc = docDao.findById(1);
		docDao.initialize(doc);
		assertEquals("1.0", doc.getVersion());
		User user = new User();
		user.setId(1);
		user.setUsername("admin");
		user.setName("xx");
		user.setFirstName("xx");

		int versionsCap = Context.get().getProperties().getInt("document.maxversions");
		assertEquals(versionsCap, testSubject.findByDocId(doc.getId()).size());
		Store store = Context.get().getBean(Store.class);
		for (Version ver : testSubject.findByDocId(doc.getId())) {
			String res = store.getResourceName(doc.getId(), ver.getFileVersion(), null);
			store.store(this.getClass().getResourceAsStream("/data.sql"), doc.getId(), res);
		}
		for (Version ver : testSubject.findByDocId(doc.getId())) {
			String res = store.getResourceName(doc.getId(), ver.getFileVersion(), null);
			store.exists(doc.getId(), res);
		}

		Version version = Version.create(doc, user, "", DocumentEvent.STORED.toString(), true);
		testSubject.store(version);
		assertEquals("1.0", testSubject.findById(version.getId()).getVersion());

		String resourceName = store.getResourceName(doc.getId(), version.getFileVersion(), null);
		try (InputStream is = this.getClass().getResourceAsStream("/data.sql")) {
			store.store(is, doc.getId(), resourceName);
		}

		assertEquals(versionsCap, testSubject.findByDocId(doc.getId()).size());
		for (Version ver : testSubject.findByDocId(doc.getId())) {
			String res = store.getResourceName(doc.getId(), ver.getFileVersion(), null);
			store.exists(doc.getId(), res);
		}

		version = Version.create(doc, user, "", DocumentEvent.CHANGED.toString(), true);
		testSubject.store(version);
		assertEquals("2.0", version.getVersion());

		resourceName = store.getResourceName(doc.getId(), version.getFileVersion(), null);
		try (InputStream is = this.getClass().getResourceAsStream("/data.sql")) {
			store.store(is, doc.getId(), resourceName);
		}

		assertEquals(versionsCap, testSubject.findByDocId(doc.getId()).size());
		for (Version ver : testSubject.findByDocId(doc.getId())) {
			String res = store.getResourceName(doc.getId(), ver.getFileVersion(), null);
			store.exists(doc.getId(), res);
		}

		version = Version.create(doc, user, "", DocumentEvent.CHECKEDIN.toString(), false);
		testSubject.store(version);
		assertEquals("2.1", version.getVersion());

		resourceName = store.getResourceName(doc.getId(), version.getFileVersion(), null);
		try (InputStream is = this.getClass().getResourceAsStream("/data.sql")) {
			store.store(is, doc.getId(), resourceName);
		}

		assertEquals(versionsCap, testSubject.findByDocId(doc.getId()).size());
		for (Version ver : testSubject.findByDocId(doc.getId())) {
			String res = store.getResourceName(doc.getId(), ver.getFileVersion(), null);
			store.exists(doc.getId(), res);
		}
	}
}