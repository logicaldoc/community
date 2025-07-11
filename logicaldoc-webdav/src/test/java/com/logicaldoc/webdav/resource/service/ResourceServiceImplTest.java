package com.logicaldoc.webdav.resource.service;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.sql.SQLException;
import java.util.List;

import org.apache.jackrabbit.webdav.DavException;
import org.junit.Assert;
import org.junit.Test;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.document.BookmarkDAO;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.DocumentDAO;
import com.logicaldoc.core.document.DocumentStatus;
import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.util.io.IOUtil;
import com.logicaldoc.util.io.ResourceUtil;
import com.logicaldoc.util.plugin.PluginException;
import com.logicaldoc.util.spring.Context;
import com.logicaldoc.webdav.AbstractWebdavTestCase;
import com.logicaldoc.webdav.context.ImportContextImpl;
import com.logicaldoc.webdav.resource.model.Resource;
import com.logicaldoc.webdav.resource.model.ResourceImpl;

public class ResourceServiceImplTest extends AbstractWebdavTestCase {

	// Instance under test
	private ResourceServiceImpl testSubject;

	private DocumentDAO docDao;

	private BookmarkDAO bookmarkDao;

	@Override
	public void setUp() throws IOException, SQLException, PluginException {
		super.setUp();

		testSubject = Context.get(ResourceServiceImpl.class);
		docDao = Context.get(DocumentDAO.class);
		bookmarkDao = Context.get(BookmarkDAO.class);
	}

	@Test
	public void testGetChildResources() throws DavException {
		ResourceImpl resource = new ResourceImpl();
		resource.setID("4");
		resource.setRequestedPerson(1L);
		resource.setSession(davSession);

		List<Resource> children = testSubject.getChildResources(resource);
		assertNotNull(children);
		assertEquals(9, children.size());

		assertTrue(children.stream().anyMatch(
				child -> "1".equals(child.getID()) && "one.pdf".equals(child.getName()) && !child.isFolder()));
		assertTrue(children.stream()
				.anyMatch(child -> "6".equals(child.getID()) && "folder6".equals(child.getName()) && child.isFolder()));

		resource = new ResourceImpl();
		resource.setID("4");
		resource.setRequestedPerson(4L);
		resource.setSession(davSession);
		children = testSubject.getChildResources(resource);
		assertNotNull(children);
		assertTrue(children.isEmpty());
	}

	@Test
	public void testGetResource() throws DavException {
		Resource resource = testSubject.getResource("/Default", davSession);
		assertNotNull(resource);
		assertTrue(resource.isFolder());

		resource = testSubject.getResource("/Default/one.pdf", davSession);
		assertNotNull(resource);
		assertEquals("one.pdf", resource.getName());
		assertFalse(resource.isFolder());

		resource = testSubject.getResource("/Default/folder6", davSession);
		assertNotNull(resource);
		assertEquals("folder6", resource.getName());
		assertTrue(resource.isFolder());

		resource = testSubject.getResource("/", davSession);
		assertNotNull(resource);
		assertEquals("/", resource.getName());
		assertTrue(resource.isFolder());

		resource = testSubject.getResource("/unexisting", davSession);
		assertNull(resource);

		davSession.putObject("id", 99L);
		try {
			testSubject.getResource("/Default/one.pdf", davSession);
			fail("The user should not have access to the resource");
		} catch (DavException e) {
			// All ok
		}

	}

	@Test
	public void testGetParentResource() throws DavException {
		Resource resource = testSubject.getParentResource("/Default/one.pdf", 1L, null);
		assertNotNull(resource);
		assertTrue(resource.isFolder());
		assertEquals("Default", resource.getName());
	}

	@Test
	public void testCreateResource() throws DavException, IOException {
		Resource parent = testSubject.getResource("/Default/folder6", davSession);
		assertNotNull(parent);
		assertTrue(parent.isFolder());

		Resource newResource = new ResourceImpl();
		newResource.setName("test.pdf");

		try (InputStream is = ResourceUtil.getInputStream("pdf1.pdf")) {
			ImportContextImpl importContext = new ImportContextImpl(newResource, null, is);
			newResource = testSubject.createResource(parent, "test.pdf", false, importContext, davSession);
		}
		assertNotNull(newResource);
		assertFalse(newResource.isFolder());

		newResource = testSubject.createResource(parent, "folderX", true, null, davSession);
		assertNotNull(newResource);
		assertTrue(newResource.isFolder());
	}

	@Test
	public void testUpdateResource() throws DavException, IOException {
		Resource resource = testSubject.getResource("/Default/one.pdf", davSession);
		assertNotNull(resource);
		assertEquals("one.pdf", resource.getName());
		assertFalse(resource.isFolder());
		assertEquals(Long.valueOf(1356L), resource.getContentLength());

		try (InputStream is = ResourceUtil.getInputStream("pdf2.pdf")) {
			ImportContextImpl importContext = new ImportContextImpl(resource, null, is);
			testSubject.updateResource(resource, importContext, davSession);
		}

		resource = testSubject.getResource("/Default/one.pdf", davSession);
		assertNotNull(resource);
		assertEquals("one.pdf", resource.getName());
		assertFalse(resource.isFolder());
		assertEquals(Long.valueOf(620094L), resource.getContentLength());
	}

	@Test
	public void testGetChildByName() throws DavException {
		Resource resource = testSubject.getResource("/Default", davSession);
		assertNotNull(resource);
		assertTrue(resource.isFolder());

		resource = testSubject.getChildByName(resource, "one.pdf");
		assertNotNull(resource);
		assertFalse(resource.isFolder());
		assertEquals("one.pdf", resource.getName());

		resource = testSubject.getChildByName(resource, "unexisting.pdf");
		assertNull(resource);
	}

	@Test
	public void testMoveFile() throws DavException {
		Resource source = testSubject.getResource("/Default/one.pdf", davSession);
		assertNotNull(source);
		assertEquals("one.pdf", source.getName());
		assertFalse(source.isFolder());

		Resource destinationFolder = testSubject.getResource("/Default/folder6", davSession);
		assertNotNull(destinationFolder);

		source.setName("test.pdf");
		Resource destination = testSubject.move(source, destinationFolder, null, davSession);
		assertNotNull(destination);
		assertEquals("test.pdf", destination.getName());

		destination = testSubject.getResource("/Default/folder6/test.pdf", davSession);
		assertNotNull(destination);
		assertEquals("test.pdf", destination.getName());
		assertFalse(destination.isFolder());

		source = testSubject.getResource("/Default/one.pdf", davSession);
		assertNull(source);
	}

	@Test
	public void testMoveFolder() throws DavException {
		Resource source = testSubject.getResource("/Default/folder6", davSession);
		assertNotNull(source);
		assertEquals("folder6", source.getName());
		assertTrue(source.isFolder());

		Resource destinationFolder = testSubject.getResource("/Default/folder7", davSession);
		assertNotNull(destinationFolder);
		assertEquals("folder7", destinationFolder.getName());
		assertTrue(destinationFolder.isFolder());

		source.setName("test");
		testSubject.move(source, destinationFolder, null, davSession);

		Resource destination = testSubject.getResource("/Default/folder7/test", davSession);
		assertNotNull(destination);
		assertEquals("test", destination.getName());
		assertTrue(destination.isFolder());

		source = testSubject.getResource("/Default/folder6", davSession);
		assertNull(source);

		// rename a folder
		Resource source02 = testSubject.getResource("/Default/folder7", davSession);
		Resource destination02 = testSubject.getResource("/Default", davSession);

		// Bad example, the rename must be done in a different way
		Resource badBlood = testSubject.move(source02, destination02, "reputation", davSession);
		assertNotNull(badBlood);
		assertEquals("folder7", badBlood.getName());
		assertTrue(badBlood.isFolder());

		// good example (folder rename)
		source02.setName("delicate");
		Resource delicate = testSubject.move(source02, destination02, null, davSession);
		assertNotNull(delicate);
		assertEquals("delicate", delicate.getName());
		assertTrue(delicate.isFolder());

	}

	@Test
	public void testDeleteResource() throws DavException {

		// delete document
		Resource resource = testSubject.getResource("/Default/one.pdf", davSession);
		assertNotNull(resource);
		assertEquals("one.pdf", resource.getName());
		assertFalse(resource.isFolder());

		testSubject.deleteResource(resource, davSession);
		assertNull(testSubject.getResource("/Default/one.pdf", davSession));

		// delete folder
		resource = testSubject.getResource("/Default/folder6", davSession);
		assertNotNull(resource);
		assertEquals("folder6", resource.getName());
		assertTrue(resource.isFolder());

		testSubject.deleteResource(resource, davSession);
		assertNull(testSubject.getResource("/Default/folder6", davSession));
	}

	@Test
	public void testCopy() throws DavException {
		Resource source = testSubject.getResource("/Default/one.pdf", davSession);
		assertNotNull(source);
		assertEquals("one.pdf", source.getName());
		assertFalse(source.isFolder());

		Resource destinationFolder = testSubject.getResource("/Default/folder6", davSession);
		assertNotNull(destinationFolder);

		Resource destination = testSubject.getResource("/Default/folder6/one.pdf", davSession);
		assertNull(destination);

		testSubject.copy(source, destinationFolder, null, davSession);
		destination = testSubject.getResource("/Default/folder6/one.pdf", davSession);
		assertNotNull(destination);

		source = testSubject.getResource("/Default/one.pdf", davSession);
		assertNotNull(source);

		// Test copy Folder (it should produce an exception, as right now copy
		// of folder is not supported)
		Resource dest02 = testSubject.getResource("/Default/folder7", davSession);
		try {
			testSubject.copy(destinationFolder, dest02, null, davSession);
			Assert.fail();
		} catch (DavException e) {
			// All as expected
		}
	}

	@Test
	public void testStreamOut() throws DavException, IOException {
		Resource source = testSubject.getResource("/Default/one.pdf", davSession);
		assertNotNull(source);
		assertEquals("one.pdf", source.getName());
		assertFalse(source.isFolder());

		File tmpFile = FileUtil.createTempFile("test", ".pdf");
		try (InputStream is = testSubject.streamOut(source)) {
			IOUtil.write(is, tmpFile);
			assertEquals(127810L, tmpFile.length());
		} finally {
			FileUtil.delete(tmpFile);
		}
	}

	@Test
	public void testCheckout() throws DavException, PersistenceException {
		Document doc = docDao.findById(1L);
		assertEquals(DocumentStatus.UNLOCKED, doc.getStatus());

		Resource resource = testSubject.getResource("/Default/one.pdf", davSession);
		assertNotNull(resource);
		assertEquals("one.pdf", resource.getName());
		assertFalse(resource.isFolder());

		testSubject.checkout(resource, davSession);

		doc = docDao.findById(1L);
		assertEquals(DocumentStatus.CHECKEDOUT, doc.getStatus());

		// Test uncheckout
		resource = testSubject.getResource("/Default/one.pdf", davSession);
		testSubject.uncheckout(resource, davSession);

		doc = docDao.findById(1L);
		assertEquals(DocumentStatus.UNLOCKED, doc.getStatus());
	}

	@Test
	public void testGetHistory() throws DavException {
		Resource resource = testSubject.getResource("/Default/one.pdf", davSession);
		assertNotNull(resource);
		assertEquals("one.pdf", resource.getName());
		assertFalse(resource.isFolder());

		List<Resource> history = testSubject.getHistory(resource);
		assertNotNull(history);
		assertEquals(2, history.size());
	}

	@Test
	public void testAddBookmark() throws DavException, PersistenceException {
		assertEquals(0, bookmarkDao.findAll().size());

		Resource resource = testSubject.getResource("/Default/one.pdf", davSession);
		assertNotNull(resource);
		assertEquals("one.pdf", resource.getName());
		assertFalse(resource.isFolder());

		testSubject.addBookmark(resource, davSession);
		assertEquals(1, bookmarkDao.findAll().size());

		testSubject.deleteBookmark(resource, davSession);
		assertEquals(0, bookmarkDao.findAll().size());

		resource = testSubject.getResource("/Default/folder6", davSession);
		assertNotNull(resource);
		assertEquals("folder6", resource.getName());
		assertTrue(resource.isFolder());

		testSubject.addBookmark(resource, davSession);
		assertEquals(1, bookmarkDao.findAll().size());
	}
}