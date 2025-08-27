package com.logicaldoc.core.document;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.sql.SQLException;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.List;

import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTestCase;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.metadata.Template;
import com.logicaldoc.core.metadata.TemplateDAO;
import com.logicaldoc.util.plugin.PluginException;
import com.logicaldoc.util.spring.Context;

/**
 * Test Case for {@link DocumentComparator}
 * 
 * @author Giuseppe Desiato - LogicalDOC
 * @since 9.1.1
 */
public class DocumentComparatorTest extends AbstractCoreTestCase {

	// Instance under test
	private DocumentDAO testSubject;

	private FolderDAO folderDao;

	@Before
	public void setUp() throws IOException, SQLException, PluginException {
		super.setUp();

		// Retrieve the instance under test from spring context. Make sure that
		// it is an HibernateDocumentDAO
		testSubject = Context.get(DocumentDAO.class);
		folderDao = Context.get(FolderDAO.class);
	}

	@Test
	public void testGetComparatorString() throws PersistenceException {
		Comparator<AbstractDocument> fdatec = DocumentComparator.getComparator("fileName desc, date asc");
		assertNotNull(fdatec);

		// add some document to a list
		List<Document> docs = testSubject.findAll();

		// check that the list is sorted
		Collections.sort(docs, fdatec);

		Document firstDoc = docs.get(0);
		assertEquals("pluto.pdf", firstDoc.getFileName());

		Document lstDoc = docs.get(3);
		assertEquals("context.xml", lstDoc.getFileName());
	}

	@Test
	public void testCompareDifferentParameters() throws PersistenceException {
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
		testSubject.store(doc1);

		Document doc2 = new Document();
		doc2.setFileName("document2");
		doc2.setFolder(folder2);
		
		testSubject.store(doc2);

		Comparator<AbstractDocument> comparator = DocumentComparator.getComparator("id asc");
		assertEquals(-1, comparator.compare(doc1, doc2));
		assertEquals(1, comparator.compare(doc2, doc1));

		comparator = DocumentComparator.getComparator("fileName asc");
		assertEquals(-1, comparator.compare(doc1, doc2));
		assertEquals(1, comparator.compare(doc2, doc1));

		comparator = DocumentComparator.getComparator("fileName desc");
		assertEquals(1, comparator.compare(doc1, doc2));
		assertEquals(-1, comparator.compare(doc2, doc1));

		comparator = DocumentComparator.getComparator("creation asc");
		assertEquals(-1, comparator.compare(doc1, doc2));
		assertEquals(1, comparator.compare(doc2, doc1));
	}

	@Test
	public void testGetComparatorSingleField() {
		Comparator<AbstractDocument> comparator = DocumentComparator.getComparator("lower(fileName) asc");
		Document doc1 = new Document();
		doc1.setFileName("Alpha");
		Document doc2 = new Document();
		doc2.setFileName("alpha");

		assertEquals(0, comparator.compare(doc1, doc2));

		comparator = DocumentComparator.getComparator("fileSize asc");
		doc1 = new Document();
		doc1.setFileSize(1234);
		doc2 = new Document();
		doc2.setFileSize(4356);

		assertEquals(-1, comparator.compare(doc1, doc2));
		assertEquals(1, comparator.compare(doc2, doc1));

		comparator = DocumentComparator.getComparator("version asc");
		doc1 = new Document();
		doc1.setVersion("1.0");
		doc2 = new Document();
		doc2.setVersion("2.0");

		assertEquals(-1, comparator.compare(doc1, doc2));
		assertEquals(1, comparator.compare(doc2, doc1));

		comparator = DocumentComparator.getComparator("fileVersion asc");
		doc1 = new Document();
		doc1.setFileVersion("1.0");
		doc2 = new Document();
		doc2.setFileVersion("2.0");

		assertEquals(-1, comparator.compare(doc1, doc2));
		assertEquals(1, comparator.compare(doc2, doc1));

		comparator = DocumentComparator.getComparator("customId asc");
		doc1 = new Document();
		doc1.setCustomId("a");
		doc2 = new Document();
		doc2.setCustomId("b");

		assertEquals(-1, comparator.compare(doc1, doc2));
		assertEquals(1, comparator.compare(doc2, doc1));

		comparator = DocumentComparator.getComparator("type asc");
		doc1 = new Document();
		doc1.setType(".pdfa");
		doc2 = new Document();
		doc2.setType(".pdfb");

		assertEquals(-1, comparator.compare(doc1, doc2));
		assertEquals(1, comparator.compare(doc2, doc1));
	}

	@Test
	public void testCommentSort() {
		// case sensitive
		Comparator<AbstractDocument> comparator = DocumentComparator.getComparator("comment asc");

		Document doc1 = new Document();
		doc1.setComment("abc");
		Document doc2 = new Document();
		doc2.setComment("ABC");

		assertEquals(true, comparator.compare(doc1, doc2) > 0);

		doc1.setComment(null);
		doc2.setComment(null);
		assertEquals(0, comparator.compare(doc1, doc2));

		doc1.setComment("abc");
		doc2.setComment(null);
		assertEquals(1, comparator.compare(doc1, doc2));

		doc1.setComment(null);
		doc2.setComment("ABC");
		assertEquals(-1, comparator.compare(doc1, doc2));

		// case insensitive
		comparator = DocumentComparator.getComparator("lower(comment) asc");

		doc1.setComment("abc");
		doc2.setComment("ABC");
		assertEquals(0, comparator.compare(doc1, doc2));

		doc1.setComment(null);
		doc2.setComment(null);
		assertEquals(0, comparator.compare(doc1, doc2));

		doc1.setComment("abc");
		doc2.setComment(null);
		assertEquals(1, comparator.compare(doc1, doc2));

		doc1.setComment(null);
		doc2.setComment("ABC");
		assertEquals(-1, comparator.compare(doc1, doc2));
	}

	@Test
	public void testStartAndStopPubSort() {
		// testing STARTPUB
		Comparator<AbstractDocument> comparator = DocumentComparator.getComparator("startPublishing asc");

		Document doc1 = new Document();
		doc1.setStartPublishing(new Date());
		Document doc2 = new Document();
		doc2.setStartPublishing(new Date());

		assertEquals(0, comparator.compare(doc1, doc2));

		doc1.setStartPublishing(null);
		doc2.setStartPublishing(null);
		assertEquals(0, comparator.compare(doc1, doc2));

		doc1.setStartPublishing(new Date());
		doc2.setStartPublishing(null);
		assertEquals(1, comparator.compare(doc1, doc2));

		doc1.setStartPublishing(null);
		doc2.setStartPublishing(new Date());
		assertEquals(-1, comparator.compare(doc1, doc2));

		// testing STOPPUB
		comparator = DocumentComparator.getComparator("stopPublishing asc");

		doc1 = new Document();
		doc1.setStopPublishing(new Date());
		doc2 = new Document();
		doc2.setStopPublishing(new Date());

		assertEquals(0, comparator.compare(doc1, doc2));

		doc1.setStopPublishing(null);
		doc2.setStopPublishing(null);
		assertEquals(0, comparator.compare(doc1, doc2));

		doc1.setStopPublishing(new Date());
		doc2.setStopPublishing(null);
		assertEquals(1, comparator.compare(doc1, doc2));

		doc1.setStopPublishing(null);
		doc2.setStopPublishing(new Date());
		assertEquals(-1, comparator.compare(doc1, doc2));
	}

	@Test
	public void testWorkflowStatusStatusSort() {
		// testing case sensitive
		Comparator<AbstractDocument> comparator = DocumentComparator.getComparator("workflowStatus asc");

		Document doc1 = new Document();
		doc1.setWorkflowStatus("status1");
		Document doc2 = new Document();
		doc2.setWorkflowStatus("STATUS2");

		assertEquals(true, comparator.compare(doc1, doc2) > 0);

		doc1.setWorkflowStatus(null);
		doc2.setWorkflowStatus(null);
		assertEquals(0, comparator.compare(doc1, doc2));

		doc1.setWorkflowStatus("status1");
		doc2.setWorkflowStatus(null);
		assertEquals(1, comparator.compare(doc1, doc2));

		doc1.setWorkflowStatus(null);
		doc2.setWorkflowStatus("STATUS2");
		assertEquals(-1, comparator.compare(doc1, doc2));

		// testing case insensitive
		comparator = DocumentComparator.getComparator("lower(workflowStatus) asc");

		doc1 = new Document();
		doc1.setWorkflowStatus("status1");
		doc2 = new Document();
		doc2.setWorkflowStatus("STATUS2");

		assertEquals(-1, comparator.compare(doc1, doc2));

		doc1.setWorkflowStatus(null);
		doc2.setWorkflowStatus(null);
		assertEquals(0, comparator.compare(doc1, doc2));

		doc1.setWorkflowStatus("status1");
		doc2.setWorkflowStatus(null);
		assertEquals(1, comparator.compare(doc1, doc2));

		doc1.setWorkflowStatus(null);
		doc2.setWorkflowStatus("STATUS2");
		assertEquals(-1, comparator.compare(doc1, doc2));
	}

	@Test
	public void testPublishedStatusSort() {
		Comparator<AbstractDocument> comparator = DocumentComparator.getComparator("publishedStatus asc");

		Document doc1 = new Document();
		doc1.setStartPublishing(new Date());
		Document doc2 = new Document();
		doc2.setStartPublishing(new Date());

		assertEquals(0, comparator.compare(doc1, doc2));
	}

	@Test
	public void testTemplateNameSort() throws PersistenceException {
		TemplateDAO templateDao = Context.get(TemplateDAO.class);

		Template template1 = new Template();
		template1.setName("template1");
		template1.setTemplateName("templateName1");
		templateDao.store(template1);

		Template template2 = new Template();
		template2.setName("TEMPLATE1");
		template2.setTemplateName("TemplateNAME1");
		templateDao.store(template2);

		// testing case sensitive
		Comparator<AbstractDocument> comparator = DocumentComparator.getComparator("template asc");

		Document doc1 = new Document();
		doc1.setTemplate(template1);
		doc1.setTemplateName(template1.getTemplateName());
		Document doc2 = new Document();
		doc2.setTemplate(template2);
		doc2.setTemplateName(template2.getTemplateName());

		assertTrue(comparator.compare(doc1, doc2) > 0);

		doc1.setTemplate(null);
		doc2.setTemplate(null);
		assertEquals(0, comparator.compare(doc1, doc2));

		// testing case insensitive
		comparator = DocumentComparator.getComparator("lower(template) asc");

		doc1 = new Document();
		doc1.setTemplate(template1);
		doc1.setTemplateName(template1.getTemplateName());
		doc2 = new Document();
		doc2.setTemplate(template2);
		doc2.setTemplateName(template2.getTemplateName());

		assertEquals(0, comparator.compare(doc1, doc2));

		doc1.setTemplate(null);
		doc2.setTemplate(null);
		assertEquals(0, comparator.compare(doc1, doc2));
	}

	@Test
	public void testExtendedAttributes() throws PersistenceException {
		Folder folder1 = new Folder();
		folder1.setName("folder1");
		folder1.setTemplateName("folder1Templ");
		folderDao.store(folder1);

		Folder folder2 = new Folder();
		folder2.setName("folder2");
		folder2.setTemplateName("folder2Templ");
		folderDao.store(folder2);

		Comparator<AbstractDocument> comparator = DocumentComparator.getComparator("ext_customAttr asc");

		DocumentDAO docDao = Context.get(DocumentDAO.class);

		Document doc1 = new Document();
		doc1.setFileName("document1");
		doc1.setFolder(folder1);
		doc1.setValue("customAttr", "Apple");
		docDao.store(doc1);

		Document doc2 = new Document();
		doc2.setFileName("document2");
		doc2.setFolder(folder2);
		doc2.setValue("customAttr", "Banana");
		docDao.store(doc2);

		assertEquals(true, comparator.compare(doc1, doc2) < 0);
		assertEquals(true, comparator.compare(doc2, doc1) > 0);

		// Extended attributes with null values
		comparator = DocumentComparator.getComparator("ext_customAttr asc");

		doc1 = new Document();
		doc1.setValue("customAttr", null);
		doc2 = new Document();
		doc2.setValue("customAttr", "Banana");

		Document doc3 = new Document();
		doc3.setFileName("document3");
		doc3.setFolder(folder1);
		doc3.setValue("customAttr", null);
		docDao.store(doc3);

		assertEquals(true, comparator.compare(doc1, doc2) < 0);
		assertEquals(true, comparator.compare(doc2, doc1) > 0);
		assertEquals(0, comparator.compare(doc1, doc3));

		// sorting extended attribute with case insensitivity
		Comparator<AbstractDocument> caseInsensitiveComparator = DocumentComparator
				.getComparator("lower(ext_customAttr) asc");

		doc1 = new Document();
		doc1.setFileName("document1");
		doc1.setFolder(folder1);
		doc1.setValue("customAttr", "apple");
		docDao.store(doc1);

		doc2 = new Document();
		doc2.setFileName("document2");
		doc2.setFolder(folder2);
		doc2.setValue("customAttr", "Banana");
		docDao.store(doc2);

		doc3 = new Document();
		doc3.setFileName("document3");
		doc3.setFolder(folder1);
		doc3.setValue("customAttr", "APPLE");
		docDao.store(doc3);

		assertEquals(0, caseInsensitiveComparator.compare(doc1, doc3));
		assertEquals(true, caseInsensitiveComparator.compare(doc1, doc2) < 0);

		Comparator<AbstractDocument> caseSensitiveComparator = DocumentComparator.getComparator("ext_customAttr asc");
		assertEquals(true, caseSensitiveComparator.compare(doc1, doc3) > 0);
		assertEquals(false, caseSensitiveComparator.compare(doc1, doc2) < 0);
	}
}
