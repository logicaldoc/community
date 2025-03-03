package com.logicaldoc.core.document;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNotSame;

import java.io.IOException;
import java.sql.SQLException;

import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTestCase;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.plugin.PluginException;

/**
 * Test case for {@link DocumentLink}
 * 
 * @author Giuseppe Desiato - LogicalDOC
 * @since 9.1.1
 */
public class DocumentLinkTest extends AbstractCoreTestCase {

	private FolderDAO folderDao;

	private DocumentDAO docDao;

	@Before
	public void setUp() throws IOException, SQLException, PluginException {
		super.setUp();

		folderDao = Context.get(FolderDAO.class);
		docDao = Context.get(DocumentDAO.class);
	}

	@Test
	public void testHashCode() throws PersistenceException {
		Folder folder1 = new Folder();
		folder1.setName("folder1");
		folder1.setTemplateName("folder1Templ");
		folderDao.store(folder1);

		Document doc1 = new Document();
		doc1.setFileName("document1");
		doc1.setFolder(folder1);
		docDao.store(doc1);

		Document doc2 = new Document();
		doc2.setFileName("document2");
		doc2.setFolder(folder1);
		docDao.store(doc2);

		DocumentLink link1 = new DocumentLink();
		link1.setDocument1(doc1);
		assertNotNull(link1.getDocument1());

		DocumentLink link2 = new DocumentLink();
		link2.setDocument2(doc2);
		assertNotNull(link2.getDocument2());

		assertNotSame(link1.hashCode(), link2.hashCode());
	}

	@Test
	public void testEquals() throws PersistenceException {
		Folder folder1 = new Folder();
		folder1.setName("folder1");
		folder1.setTemplateName("folder1Templ");
		folderDao.store(folder1);

		Folder folder2 = new Folder();
		folder2.setName("folder2");
		folder2.setTemplateName("folder2Templ");
		folderDao.store(folder2);

		Document doc1 = new Document();
		doc1.setFileName("document1");
		doc1.setFolder(folder1);
		docDao.store(doc1);

		Document doc2 = new Document();
		doc2.setFileName("document2");
		doc2.setFolder(folder2);
		docDao.store(doc2);

		Document doc3 = new Document();
		doc3.setFileName("document3");
		doc3.setFolder(folder2);
		docDao.store(doc3);

		DocumentLink link1 = new DocumentLink();
		link1.setDocument1(doc1);

		DocumentLink link2 = new DocumentLink();
		link2.setDocument2(doc2);

		assertEquals(link1, link1);
		assertEquals(false, link2.equals(link1));
		assertEquals(false, link1.equals(link2));

		link2 = null;
		assertEquals(false, link1.equals(link2));

		link1 = new DocumentLink();
		link1.setDocument1(null);
		link2 = new DocumentLink();
		link2.setDocument2(null);
		assertEquals(link2, link1);

		link2.setDocument2(doc3);
		DocumentLink link3 = new DocumentLink();
		link3.setDocument2(doc2);
		assertEquals(false, link2.equals(link3));

		link1 = new DocumentLink();
		link1.setType("linkType1");
		assertNotNull(link1.getType());
		link2 = new DocumentLink();
		link2.setType("linkType2");
		assertNotNull(link2.getType());
		assertEquals(false, link1.equals(link2));
		assertEquals(false, link2.equals(link1));
	}
}
