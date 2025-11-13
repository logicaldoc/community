package com.logicaldoc.core.document;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.io.InputStream;
import java.sql.Date;
import java.sql.SQLException;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTestCase;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.security.user.User;
import com.logicaldoc.core.security.user.UserDAO;
import com.logicaldoc.core.store.Store;
import com.logicaldoc.core.store.StoreResource;
import com.logicaldoc.util.io.ResourceUtil;
import com.logicaldoc.util.plugin.PluginException;
import com.logicaldoc.util.spring.Context;

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
		testSubject = VersionDAO.get();
		docDao = DocumentDAO.get();
	}

	@Test
	public void testFindByDocumentId() throws PersistenceException {
		List<Version> versions = testSubject.findByDocId(1);
		assertEquals(3, versions.size());
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
		assertEquals(99L, version.getTemplateId().longValue());

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

		// version is empty
		version = testSubject.findByFileVersion(1, "");
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

		Store store = Store.get();

		int versionsCap = Context.get().getProperties().getInt("document.maxversions");
		for (int i = 0; i < versionsCap * 2; i++) {
			Version version = null;
			if (i % 2 == 0) {
				version = Version.create(doc, user, "checkin " + i, DocumentEvent.CHECKEDIN, true);
				store.store(ResourceUtil.getInputStream("data.sql"),
						StoreResource.builder().docId(doc.getId()).fileVersion(version.getFileVersion()).build());
			} else {
				version = Version.create(doc, user, "edit " + i, DocumentEvent.CHANGED, true);
			}
			testSubject.store(version);
		}

		// Check if the versions cap has been respected
		List<Version> versions = testSubject.findByDocId(doc.getId());
		assertEquals(versionsCap, versions.size());

		// Check if all the versions point to an existing file
		for (Version version : versions)
			assertTrue(store.exists(StoreResource.builder().document(version).build()));

		// Check if no files of deleted versions are still in the store
		Set<String> actualFileVersions = versions.stream().map(Version::getFileVersion).collect(Collectors.toSet());
		List<StoreResource> currentResourceFiles = store.listResources(doc.getId(), null).stream()
				.filter(r -> !r.name().contains("-")).toList();
		for (StoreResource resource : currentResourceFiles)
			assertTrue(actualFileVersions.contains(resource.name()));

		// Now reset all versions
		testSubject.deleteAll(versions);
		doc.setVersion("1.0");
		doc.setFileVersion("1.0");
		docDao.store(doc);

		Version version = Version.create(doc, user, "", DocumentEvent.STORED, true);
		testSubject.store(version);
		assertEquals("1.0", testSubject.findById(version.getId()).getVersion());

		try (InputStream is = ResourceUtil.getInputStream("data.sql")) {
			store.store(is,
					StoreResource.builder().docId(doc.getId()).fileVersion(version.getFileVersion()).build());
		}

		doc = docDao.findById(1);
		docDao.initialize(doc);
		version = Version.create(doc, user, "", DocumentEvent.CHANGED, true);
		testSubject.store(version);
		assertEquals("2.0", version.getVersion());

		try (InputStream is = ResourceUtil.getInputStream("data.sql")) {
			store.store(is,
					StoreResource.builder().docId(doc.getId()).fileVersion(version.getFileVersion()).build());
		}

		assertEquals(versionsCap, versions.size());
		for (Version ver : versions)
			store.exists(StoreResource.builder().docId(doc.getId()).fileVersion(ver.getFileVersion()).build());

		version = Version.create(doc, user, "", DocumentEvent.CHECKEDIN, false);
		testSubject.store(version);
		assertEquals("2.1", version.getVersion());

		try (InputStream is = ResourceUtil.getInputStream("data.sql")) {
			store.store(is,
					StoreResource.builder().docId(doc.getId()).fileVersion(version.getFileVersion()).build());
		}

		assertEquals(versionsCap, versions.size());
		for (Version ver : versions)
			store.exists(StoreResource.builder().docId(doc.getId()).fileVersion(ver.getFileVersion()).build());

		FolderDAO folderDao = FolderDAO.get();
		Folder folder1 = new Folder();
		folder1.setName("folderBVO");
		folder1.setType(Folder.TYPE_WORKSPACE);
		folder1.setMaxVersions(1);
		folderDao.store(folder1);

		Document doc1 = new Document();
		doc1.setFolder(folder1);
		doc1.setFileName("doc1_name");
		doc1.setVersion("1.0");
		docDao.store(doc1);
		assertNotNull(doc1);
		assertEquals("1.0", doc1.getVersion());

		version = Version.create(doc1, user, "", DocumentEvent.STORED, false);
		testSubject.store(version);
		assertEquals("1.0", testSubject.findById(version.getId()).getVersion());
	}

	@Test
	public void testUpdateDigest() throws PersistenceException {
		Document document = docDao.findById(3);
		assertNotNull(document);

		Version version = testSubject.findByVersion(3, "1.3");
		assertNotNull(version);

		testSubject.updateDigest(version);
	}

	@Test
	public void testCreate() throws PersistenceException {
		UserDAO userDao = UserDAO.get();

		Document doc = docDao.findById(1);
		docDao.initialize(doc);
		assertNotNull(doc);

		User user = userDao.findById(1);
		userDao.initialize(user);
		assertNotNull(user);

		assertNotNull(Version.create(doc, user, "testVersion", DocumentEvent.COPYED, false));

		Version version1 = new Version();
		version1.setComment("version1Comment");

		Version version2 = new Version();
		version2.setComment("version2Comment");

		Version version3 = new Version(testSubject.findById(1));
		assertNotNull(version3.toString());

		assertNotSame(version1.hashCode(), version2.hashCode());

		assertEquals(true, version1.equals(version1));
		assertEquals(version1, version2);
		assertEquals(false, version1.equals(new Object()));

		version2.setCreatorId(1);
		assertEquals(false, version1.equals(version2));

		version2.setCreatorId(0);
		version2.setDocId(1);
		assertEquals(false, version1.equals(version2));

		version2.setDocId(0);
		version2.setVersionDate(new Date(2025 - 26 - 02));
		version1.setVersionDate(null);

		assertEquals(false, version1.equals(version2));

		version2.setVersionDate(null);
		version1.setVersionDate(new Date(2025 - 26 - 02));
		assertEquals(false, version1.equals(version2));
	}
}