package com.logicaldoc.web.service;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

import java.io.IOException;
import java.sql.SQLException;
import java.util.List;

import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.DocumentDAO;
import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.metadata.Template;
import com.logicaldoc.core.metadata.TemplateDAO;
import com.logicaldoc.core.security.authorization.PermissionException;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUIAccessControlEntry;
import com.logicaldoc.gui.common.client.beans.GUIAttribute;
import com.logicaldoc.gui.common.client.beans.GUIForm;
import com.logicaldoc.gui.common.client.beans.GUITemplate;
import com.logicaldoc.util.plugin.PluginException;
import com.logicaldoc.web.AbstractWebappTestCase;

public class TemplateServiceImplTest extends AbstractWebappTestCase {

	// Instance under test
	private TemplateServiceImpl testSubject = new TemplateServiceImpl();

	private TemplateDAO templateDao;

	@Before
	public void setUp() throws IOException, SQLException, PluginException {
		super.setUp();
		templateDao = (TemplateDAO) context.getBean("TemplateDAO");
	}

	@Test
	public void testDelete() throws ServerException, PersistenceException {
		assertNull(templateDao.findById(99L));
		testSubject.delete(99L);
		assertNull(templateDao.findById(99L));

		assertNotNull(templateDao.findById(-1L));
		testSubject.delete(-1L);
		assertNull(templateDao.findById(-1L));
	}

	@Test
	public void testCountDocuments() throws ServerException {
		assertEquals(0L, testSubject.countDocuments(-1L));
		assertEquals(0L, testSubject.countDocuments(99L));
	}

	@Test
	public void testClone() throws ServerException, PersistenceException {
		GUITemplate cloned = testSubject.clone(-1L, "cloned");
		assertNotNull(cloned);

		Template t1 = templateDao.findById(-1L);
		templateDao.initialize(t1);
		Template t2 = templateDao.findById(cloned.getId());
		templateDao.initialize(t2);
		assertEquals("cloned", t2.getName());
		assertEquals(t1.getAttributes(), t2.getAttributes());
	}

	@Test
	public void testSave() throws ServerException, PersistenceException {
		GUITemplate template = testSubject.getTemplate(5);
		assertNotNull(template);
		assertEquals("test1", template.getName());
		assertEquals("test1_desc", template.getDescription());

		template.setName("pippo");
		template.setDescription("paperino");
		template.getAccessControlList().add(new GUIAccessControlEntry(4L, "read", "write"));

		testSubject.save(template);

		prepareSession("author", "admin");

		template = testSubject.getTemplate(5);
		assertNotNull(template);
		assertEquals("pippo", template.getName());
		assertEquals("paperino", template.getDescription());
		assertEquals(1, template.getAttributes().size());
		assertEquals("attr1", template.getAttributes().get(0).getName());
		assertEquals("val1", template.getAttributes().get(0).getStringValue());

		template = testSubject.getTemplate(-1L);
		template.setId(0L);
		template.setName("newTemplate");
		template.getAccessControlList().clear();
		testSubject.save(template);
		template = testSubject.getTemplate(template.getId());
		assertNotNull(template);
		assertEquals("newTemplate", template.getName());
	}

	@Test
	public void testGetTemplate() throws ServerException {
		GUITemplate template = testSubject.getTemplate(6);
		assertNotNull(template);
		assertEquals("test2", template.getName());
		assertEquals("test2_desc", template.getDescription());

		template = testSubject.getTemplate(8);
		assertNull(template);
	}

	@Test
	public void testGetAttributes() throws ServerException, PersistenceException, PermissionException {
		DocumentDAO documentDao = (DocumentDAO) context.getBean("DocumentDAO");
		FolderDAO folderDao = (FolderDAO) context.getBean("FolderDAO");

		Template template = templateDao.findById(-1L);

		Folder folder = folderDao.findById(Folder.DEFAULTWORKSPACEID);
		folderDao.initialize(folder);
		folder.setTemplate(template);
		folderDao.store(folder);

		Document document = new Document();
		document.setFileName("test.pdf");
		document.setFolder(folder);
		document.setTemplate(template);
		document.getAccessControlList().addAll(folder.getAccessControlList());
		document.setValues("multi", List.of("a","b","c"));
		documentDao.store(document);

		List<GUIAttribute> extAttr = testSubject.getAttributes(template.getId(), null);
		assertEquals(9, extAttr.size());

		extAttr = testSubject.getAttributes(template.getId(),
				new FolderServiceImpl().getFolder(session, folder.getId()));
		assertEquals(9, extAttr.size());

		extAttr = testSubject.getAttributes(template.getId(),
				new DocumentServiceImpl().getDocument(session, document.getId()));
		assertEquals(11, extAttr.size());

		GUIForm form = new GUIForm();
		form.setTemplateId(template.getId());

		extAttr = testSubject.getAttributes(template.getId(), form);
		assertEquals(9, extAttr.size());
	}
}
