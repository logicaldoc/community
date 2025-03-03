package com.logicaldoc.core.folder;

import static org.junit.Assert.assertEquals;

import java.io.IOException;
import java.sql.SQLException;
import java.util.Comparator;

import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTestCase;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.metadata.Template;
import com.logicaldoc.core.metadata.TemplateDAO;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.plugin.PluginException;

/**
 * Test case for {@link FolderComparator}
 * 
 * @author Giuseppe Desiato - LogicalDOC
 * @since 9.1.1
 */
public class FolderComparatorTest extends AbstractCoreTestCase {

	private FolderDAO testSubject;

	private TemplateDAO templateDao;

	@Before
	public void setUp() throws IOException, SQLException, PluginException {
		super.setUp();

		testSubject = Context.get(FolderDAO.class);
		templateDao = Context.get(TemplateDAO.class);
	}

	@Test
	public void testCompareDifferentIds() throws PersistenceException {
		Template template1 = new Template();
		template1.setName("template1");
		templateDao.store(template1);

		Template template2 = new Template();
		template2.setName("template2");
		templateDao.store(template2);

		Folder folder1 = new Folder();
		folder1.setName("folder1");
		folder1.setTemplateName("folder1Templ");
		testSubject.store(folder1);

		Folder folder2 = new Folder();
		folder2.setName("folder2");
		folder2.setTemplateName("folder2Templ");
		testSubject.store(folder2);

		Folder folder3 = new Folder();
		folder3.setName("folder3");
		folder3.setTemplate(template1);
		testSubject.store(folder3);

		Folder folder4 = new Folder();
		folder4.setName("folder4");
		folder4.setTemplate(template2);
		testSubject.store(folder4);

		Comparator<Folder> comparator = FolderComparator.getComparator("id asc");
		assertEquals(-1, comparator.compare(folder1, folder2));
		assertEquals(1, comparator.compare(folder2, folder1));

		comparator = FolderComparator.getComparator("name asc");
		assertEquals(-1, comparator.compare(folder1, folder2));
		assertEquals(1, comparator.compare(folder2, folder1));

		comparator = FolderComparator.getComparator("name desc");
		assertEquals(1, comparator.compare(folder1, folder2));
		assertEquals(-1, comparator.compare(folder2, folder1));

		comparator = FolderComparator.getComparator("lastModified asc");
		assertEquals(-1, comparator.compare(folder1, folder2));
		assertEquals(1, comparator.compare(folder2, folder1));

		comparator = FolderComparator.getComparator("creation asc");
		assertEquals(-1, comparator.compare(folder1, folder2));
		assertEquals(1, comparator.compare(folder2, folder1));

		comparator = FolderComparator.getComparator("template asc");
		assertEquals(-1, comparator.compare(folder1, folder2));
		assertEquals(1, comparator.compare(folder2, folder1));
		assertEquals(-1, comparator.compare(folder3, folder4));
		assertEquals(1, comparator.compare(folder4, folder3));

		comparator = FolderComparator.getComparator("lower(template) desc");
		assertEquals(1, comparator.compare(folder1, folder2));
		assertEquals(-1, comparator.compare(folder2, folder1));
		assertEquals(1, comparator.compare(folder3, folder4));
		assertEquals(-1, comparator.compare(folder4, folder3));
	}

	@Test
	public void testDescendingComparator() throws PersistenceException {
		Folder folder1 = new Folder();
		folder1.setName("folder1");
		folder1.setTemplateName("folder1Templ");
		testSubject.store(folder1);

		Folder folder2 = new Folder();
		folder2.setName("folder2");
		folder2.setTemplateName("folder2Templ");
		testSubject.store(folder2);

		Comparator<Folder> descendingIdComparator = FolderComparator.getComparator("id desc");
		Comparator<Folder> descendingNameComparator = FolderComparator.getComparator("name desc");

		assertEquals(1, descendingIdComparator.compare(folder1, folder2));
		assertEquals(-1, descendingIdComparator.compare(folder2, folder1));

		assertEquals(1, descendingNameComparator.compare(folder1, folder2));
		assertEquals(-1, descendingNameComparator.compare(folder2, folder1));
	}

	@Test
	public void testCompareExtendedAttribute() throws PersistenceException {
		Comparator<Folder> comparator = FolderComparator.getComparator("ext_customAttr asc");

		Folder folderA = new Folder();
		folderA.setValue("customAttr", "Apple");
		testSubject.store(folderA);

		Folder folderB = new Folder();
		folderB.setValue("customAttr", "Banana");
		testSubject.store(folderB);

		Folder folderC = new Folder();
		folderC.setValue("customAttr", "Apple");
		testSubject.store(folderC);

		assertEquals(true, comparator.compare(folderA, folderB) < 0);
		assertEquals(true, comparator.compare(folderB, folderA) > 0);
		assertEquals(0, comparator.compare(folderA, folderC));

		comparator = FolderComparator.getComparator("ext_customAttr asc");

		Folder folder1 = new Folder();
		folder1.setValue("customAttr", "Apple");
		Folder folder2 = new Folder();
		folder2.setValue("customAttr", "Banana");

		assertEquals(true, comparator.compare(folder1, folder2) < 0);

		// Extended attributes with null values
		comparator = FolderComparator.getComparator("ext_customAttr asc");

		folder1 = new Folder();
		folder1.setValue("customAttr", null);
		folder2 = new Folder();
		folder2.setValue("customAttr", "Banana");

		Folder folder3 = new Folder();
		folder3.setValue("customAttr", null);

		assertEquals(true, comparator.compare(folder1, folder2) < 0);
		assertEquals(true, comparator.compare(folder2, folder1) > 0);
		assertEquals(0, comparator.compare(folder1, folder3));
		
		// sorting extended attribute with case insensitivity
		Comparator<Folder> caseInsensitiveComparator = FolderComparator.getComparator("lower(ext_customAttr) asc");

		folderA = new Folder();
		folderA.setValue("customAttr", "apple");
		testSubject.store(folderA);

		folderB = new Folder();
		folderB.setValue("customAttr", "Banana");
		testSubject.store(folderB);

		folderC = new Folder();
		folderC.setValue("customAttr", "APPLE");
		testSubject.store(folderC);

		assertEquals(0, caseInsensitiveComparator.compare(folderA, folderC));
		assertEquals(true, caseInsensitiveComparator.compare(folderA, folderB) < 0);

		Comparator<Folder> caseSensitiveComparator = FolderComparator.getComparator("ext_customAttr asc");
		assertEquals(true, caseSensitiveComparator.compare(folderA, folderC) > 0);
		assertEquals(false, caseSensitiveComparator.compare(folderA, folderB) < 0);
	}

	@Test
	public void testGetComparatorSingleField() {
		// ascending
		Comparator<Folder> comparator = FolderComparator.getComparator("name asc");
		Folder folder1 = new Folder();
		folder1.setName("Folder1");
		Folder folder2 = new Folder();
		folder2.setName("Folder2");

		assertEquals(true, comparator.compare(folder1, folder2) < 0);
		assertEquals(true, comparator.compare(folder2, folder1) > 0);
		
		// descending
		comparator = FolderComparator.getComparator("name desc");
		folder1 = new Folder();
		folder1.setName("Alpha");
		folder2 = new Folder();
		folder2.setName("Beta");

		assertEquals(true, comparator.compare(folder1, folder2) > 0);
		assertEquals(true, comparator.compare(folder2, folder1) < 0);
		
		// non-case sensitive
		comparator = FolderComparator.getComparator("lower(name) asc");
		folder1 = new Folder();
		folder1.setName("Alpha");
		folder2 = new Folder();
		folder2.setName("alpha");

		assertEquals(0, comparator.compare(folder1, folder2));
	}

	@Test
	public void testGetComparatorMultipleFields() throws PersistenceException {
		Comparator<Folder> comparator = FolderComparator.getComparator("name asc, id desc");

		Folder folder1 = new Folder();
		folder1.setName("Alpha");
		testSubject.store(folder1);

		Folder folder2 = new Folder();
		folder2.setName("Alpha");
		testSubject.store(folder2);

		Folder folder3 = new Folder();
		folder3.setName("Beta");
		testSubject.store(folder3);

		assertEquals(true, comparator.compare(folder1, folder3) < 0);
		assertEquals(true, comparator.compare(folder2, folder1) < 0);
	}
}
