package com.logicaldoc.webservice.soap.endpoint;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.sql.SQLException;
import java.util.Arrays;
import java.util.List;

import org.junit.Test;

import com.logicaldoc.core.document.DocumentDAO;
import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.plugin.PluginException;
import com.logicaldoc.webservice.AbstractWebserviceTestCase;
import com.logicaldoc.webservice.model.WSDocument;
import com.logicaldoc.webservice.model.WSFolder;
import com.logicaldoc.webservice.model.WSTagCloud;

public class SoapTagServiceTest extends AbstractWebserviceTestCase {

	private FolderDAO folderDao;

	private DocumentDAO documentDao;

	private SoapTagService testSubject;

	@Override
	public void setUp() throws IOException, SQLException, PluginException {
		super.setUp();
		
		folderDao = Context.get(FolderDAO.class);
		documentDao = Context.get(DocumentDAO.class);
		documentDao.insertNewUniqueTags();

		testSubject = new SoapTagService();
		testSubject.setValidateSession(false);
	}

	@Test
	public void testGetTags() throws Exception {
		List<String> tags = testSubject.getTags("");
		assertNotNull(tags);
		assertEquals(4, tags.size());
	}

	@Test
	public void testGetTagsPreset() throws Exception {
		testSubject.setValidateSession(true);

		List<String> tags = testSubject.getTagsPreset(session.getSid());
		assertNotNull(tags);
		assertEquals(0, tags.size());

		Context.get().getProperties().setProperty("default.tag.mode", "preset");
		tags = testSubject.getTagsPreset(session.getSid());
		assertNotNull(tags);
		assertEquals(2, tags.size());
		assertTrue(tags.contains("tagx") && tags.contains("tagy"));
	}
	
	@Test
	public void testGetTagCloud() throws Exception {
		testSubject.setValidateSession(true);

		List<WSTagCloud> tags = testSubject.getTagCloud(session.getSid());
		assertNotNull(tags);
		assertEquals(4, tags.size());
	}

	@Test
	public void testAddDocumentTags() throws Exception {
		List<WSDocument> documents = testSubject.findDocumentsByTag("", "abc");
		assertNotNull(documents);
		assertEquals(1, documents.size());

		testSubject.addDocumentTags("", documents.get(0).getId(), Arrays.asList("pippo", "pluto"));
		documents = testSubject.findDocumentsByTag("", "pippo");
		assertNotNull(documents);
		assertEquals(1, documents.size());
	}

	@Test
	public void testAddFolderTags() throws Exception {
		testSubject.addFolderTags("", 4L, Arrays.asList("pippo", "pluto"));
		List<WSFolder> folders = testSubject.findFoldersByTag("", "pippo");
		assertNotNull(folders);
		assertEquals(1, folders.size());
		folders = testSubject.findFoldersByTag("", "unexisting");
		assertNotNull(folders);
		assertEquals(0, folders.size());
	}

	@Test
	public void testSetDocumentTags() throws Exception {
		List<WSDocument> documents = testSubject.findDocumentsByTag("", "abc");
		assertNotNull(documents);
		assertEquals(1, documents.size());

		testSubject.setDocumentTags("", documents.get(0).getId(), List.of("pippo", "pluto"));
		documents = testSubject.findDocumentsByTag("", "pippo");
		assertNotNull(documents);
		assertEquals(1, documents.size());
		documents = testSubject.findDocumentsByTag("", "abc");
		assertNotNull(documents);
		assertEquals(0, documents.size());
	}

	@Test
	public void testSetFolderTags() throws Exception {
		testSubject.addFolderTags("", 4L, Arrays.asList("pippo", "pluto"));
		List<WSFolder> folders = testSubject.findFoldersByTag("", "pippo");
		assertNotNull(folders);
		assertEquals(1, folders.size());

		testSubject.setFolderTags("", 4L, Arrays.asList("paperino"));
		folders = testSubject.findFoldersByTag("", "pippo");
		assertNotNull(folders);
		assertEquals(0, folders.size());
		folders = testSubject.findFoldersByTag("", "paperino");
		assertNotNull(folders);
		assertEquals(1, folders.size());
	}

	@Test
	public void testGetDocumentTags() throws Exception {
		List<WSDocument> documents = testSubject.findDocumentsByTag("", "abc");
		assertNotNull(documents);
		assertEquals(1, documents.size());

		testSubject.addDocumentTags("", documents.get(0).getId(), Arrays.asList("pippo", "pluto"));
		List<String> tags = testSubject.getDocumentTags("", documents.get(0).getId());
		assertTrue(tags.contains("abc"));
		assertTrue(tags.contains("pippo"));
		assertTrue(tags.contains("pluto"));
	}

	@Test
	public void testGetFolderTags() throws Exception {
		testSubject.addFolderTags("", 4L, Arrays.asList("pippo", "pluto"));
		List<String> tags = testSubject.getFolderTags("", 4L);
		assertTrue(tags.contains("pippo"));
		assertTrue(tags.contains("pluto"));
	}

	@Test
	public void testFindDocumentsByTag() throws Exception {
		List<WSDocument> documents = testSubject.findDocumentsByTag("", "abc");
		assertNotNull(documents);
		assertEquals(1, documents.size());
		assertEquals(1, documents.get(0).getId());

		documents = testSubject.findDocumentsByTag("", "ask");
		assertNotNull(documents);
		assertEquals(1, documents.size());
		assertEquals(2, documents.get(0).getId());

		documents = testSubject.findDocumentsByTag("", "xxx");
		assertNotNull(documents);
		assertEquals(0, documents.size());
	}

	@Test
	public void testFindFoldersByTag() throws Exception {
		Folder folder = new Folder();
		folder.setName("test");
		folder.setParentId(5L);
		folder.addTag("xyz");
		folderDao.store(folder);

		List<WSFolder> folders = testSubject.findFoldersByTag("", "xyz");
		assertNotNull(folders);
		assertEquals(1, folders.size());

		folders = testSubject.findFoldersByTag("", "unexisting");
		assertNotNull(folders);
		assertEquals(0, folders.size());
	}
}